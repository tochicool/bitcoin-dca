{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BitcoinDCA.Strategy.ScheduledBuys where

import BitcoinDCA.Common
import BitcoinDCA.Exchange
  ( MonadExchange (getAssetPair, getOrder, placeMarketBuyOrder),
  )
import BitcoinDCA.Strategy
import BitcoinDCA.Types
import Colog
import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except
import Control.Monad.Reader (MonadReader, ask, withReaderT)
import Control.Monad.Validate (MonadValidate (refute), runValidateT)
import Control.Retry
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Number), genericParseJSON, genericToJSON)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (Sum (..))
import qualified Data.Text.Encoding as Text
import Data.Time
  ( UTCTime,
    diffUTCTime,
    getCurrentTime,
  )
import GHC.Generics
import GHC.TypeLits (KnownSymbol, Symbol)
import Money (mkSomeDense)
import UnliftIO.Async (conc, runConc)
import UnliftIO.Concurrent (forkIO)
import UnliftIO.STM
import Prelude hiding (log)

data Config = Config
  { buy :: AssetId,
    withFunds :: Funds,
    frequency :: Frequency,
    minimumAmount :: Funds
  }
  deriving (Generic, Show)

instance FromJSON Config where
  parseJSON =
    genericParseJSON jsonConfigOptions
      . ("minimumAmount" ?:. Number 0)

instance ToJSON Config where
  toJSON = genericToJSON jsonConfigOptions

type ScheduledBuysStrategy = StrategyT Config

data UpdateAssetPair = UpdateAssetPair

data Rebalance = Rebalance

newtype ScheduleRebalance = ScheduleRebalance UTCTime

newtype Buy quote = Buy (Money quote)

data PendingOrder quote = PendingOrder OrderId (Money quote)

scheduledBuys :: MonadStrategy Config e m => ScheduledBuysStrategy m (Either Errors a)
scheduledBuys = withTypedConfig $ do
  Env {config = conf@TypedConfig {..} :: TypedConfig base quote} <- ask
  log Info $ "Initialising scheduled buys strategy: " +> conf
  startTime <- liftIO getCurrentTime
  actualVar <- newTVarIO 0
  optimisticVar <- newTVarIO 0
  assetPairMVar :: TMVar (AssetPair base quote) <- newEmptyTMVarIO
  updateAssetPairMVar <- newTMVarIO UpdateAssetPair
  rebalanceMVar <- newEmptyTMVarIO
  latestRebalanceTimeTVar <- newTVarIO startTime
  scheduleRebalanceMVar <- newEmptyTMVarIO
  filledSizeBroadcastChan <- askFilledSizeBroadcastChan $ assetId @base
  spentFundsBroadcastChan <- askSpentFundsBroadcastChan $ assetId @quote
  let rebalance = rebalance' rebalanceMVar
      scheduleRebalance = scheduleRebalance' scheduleRebalanceMVar
      onOrderFailed = onOrderFailed' optimisticVar rebalanceMVar updateAssetPairMVar
      updateAssetPair = updateAssetPair' updateAssetPairMVar
      completeOrder = completeOrder' actualVar filledSizeBroadcastChan spentFundsBroadcastChan
      CronSchedule cronSchedule = frequency
      target = CronTarget {startTime, everyTick = withFunds, cronSchedule}
      initialFunds = fundsAt target startTime
  buysChan <- newTChanIO
  pendingOrdersChan <- newTChanIO

  let assetPairUpdater = conc $
        forever $ do
          UpdateAssetPair <- atomically . takeTMVar $ updateAssetPairMVar
          retrying
            (fullJitterBackoff baseRetryDelay)
            (const return)
            $ \RetryStatus {rsIterNumber} -> localLogMessage ("[Retry " +> rsIterNumber <+ "] " +>) $ do
              log Info $ "Retrieving asset pair " +> (assetId @base, assetId @quote)
              result <- (Right <$> getAssetPair) `catchError` (return . Left)
              case result of
                Right (Just retrievedAssetPair) -> do
                  newAssetPair <- adjustAssetPair minimumAmount retrievedAssetPair
                  result <-
                    atomically $
                      Just <$> takeTMVar assetPairMVar
                        <|> Nothing <$ putTMVar assetPairMVar newAssetPair
                  case result of
                    Just oldAssetPair
                      | oldAssetPair == newAssetPair ->
                        log Info $ "No change in asset pair" +> newAssetPair
                      | otherwise ->
                        log Info $ "Updated asset pair from " +> oldAssetPair <+ " to " +> newAssetPair
                    Nothing ->
                      log Info $ "Updated asset pair " +> newAssetPair
                  atomically rebalance
                  return False
                Right Nothing -> do
                  log Error $ "Asset pair with id=" +> (assetId @base, assetId @quote) <+ " does not exist"
                  return True
                Left err -> do
                  log Error $ "An error occured whilst retrieving asset pair with id=" +> (assetId @base, assetId @quote) <+ ": " +> err
                  return True

      assetPairUpdateProducer = conc $
        forever $ do
          delay assetPairUpdateInterval
          atomically updateAssetPair

      rebalanceConsumer = conc $
        forever $ do
          atomically $ readTMVar rebalanceMVar
          rebalanceStartTime <- liftIO getCurrentTime
          (buys, availableFunds) <- atomically $ do
            Rebalance <- takeTMVar rebalanceMVar
            opt <- readTVar optimisticVar
            assetPair <- readTMVar assetPairMVar

            let expected = fundsAt target rebalanceStartTime - initialFunds
                funds = expected - opt
                (Sum availableFunds, buys) = minimumBuys assetPair funds
                opt' = opt + sum buys
                nextRebalanceTime = earliestTimeAt target $ initialFunds + opt' + minMarketFunds assetPair
            writeTChan buysChan `traverse_` (Buy <$> buys)
            writeTVar optimisticVar opt'
            scheduleRebalance nextRebalanceTime
            return (buys, availableFunds)
          let buyCount = length buys
          log Info $ "Rebalance triggered " +> buyCount <+ case buyCount of { 1 -> " buy "; _ -> " buys " } +> buys
          log Info $ "Rebalance left " +> availableFunds <+ " remaining funds"

      rebalanceScheduler = conc . forever $ do
        ScheduleRebalance rebalanceTime <-
          atomically $ takeTMVar scheduleRebalanceMVar
        let shouldRebalance = do
              latest <- (rebalanceTime >=) <$> readTVar latestRebalanceTimeTVar
              when latest $
                writeTVar latestRebalanceTimeTVar rebalanceTime
              return latest

        whenM (atomically shouldRebalance) $ do
          log Info $ "Scheduling rebalance for " +> rebalanceTime

          registeredTime <- liftIO getCurrentTime
          let μsUntilRebalance = max 0 . round $ (rebalanceTime `diffUTCTime` registeredTime) * second

          void . forkIO $ do
            delay μsUntilRebalance
            atomically $ whenM
              shouldRebalance
              rebalance

      buyer = conc . forever $ do
        Buy funds <- atomically . readTChan $ buysChan
        void . forkIO $ do
          (orderId, maybeOrder) <-
            placeMarketBuyOrder @_ @base funds `catchError` \err -> do
              log Error $ "An error occurred whilst placing market order: " +> err
              delay $ 10 * second
              atomically . onOrderFailed $ funds
              log Info "Adjusted available funds"
              throwError err
          case maybeOrder of
            Just order@Order {status = OrderComplete} -> do
              atomically $ completeOrder funds order
              log Info $ "Market order completed by exchange: " +> order
            Just order@Order {status = OrderFailed} -> do
              atomically . onOrderFailed $ funds
              log Warning $ "Market order rejected by exchange: " +> order
              log Info "Adjusted available funds"
            _ -> do
              atomically . writeTChan pendingOrdersChan $ PendingOrder orderId funds
              log Info $ "Market order with id=" +> orderId <+ " for funds " +> funds <+ " received by exchange and pending completion"

      orderTracker = conc . forever $ do
        PendingOrder orderId funds <- atomically . readTChan $ pendingOrdersChan
        forkIO $
          void $
            retrying
              (fullJitterBackoff baseRetryDelay)
              (const return)
              $ \RetryStatus {rsIterNumber} -> localLogMessage ("[Retry " +> rsIterNumber <+ "] " +>) $ do
                log Info $ "Retrieving order with id=" +> orderId
                result <- (Right <$> getOrder @_ @base orderId) `catchError` (return . Left)
                case result of
                  Left err -> do
                    log Error $ "An error occured whilst retrieving order with id=" +> orderId <+ ": " +> err
                    return True
                  Right (Just order@Order {status = OrderPending}) -> do
                    log Info $ "Order with id=" +> orderId <+ " is still pending - will check again: " +> order
                    return True
                  Right (Just order@Order {status = OrderComplete}) -> do
                    atomically $ completeOrder funds order
                    log Info $ "Order with id=" +> orderId <+ " is complete: " +> order
                    return False
                  Right (Just order) -> do
                    log Error $ "Order with id=" +> orderId <+ " is in an unexpected state: " +> order
                    atomically . onOrderFailed $ funds
                    return False
                  Right Nothing -> do
                    log Error $ "Order with id=" +> orderId <+ " was not found"
                    atomically . onOrderFailed $ funds
                    return False

      statusLogger = conc . forever $ do
        (optimistic, actual) <- atomically $ (,) <$> readTVar optimisticVar <*> readTVar actualVar
        currentTime <- liftIO getCurrentTime
        let expected = fundsAt target currentTime - initialFunds
        log Info $ expected <+ " (expected) ≥ " +> optimistic <+ " (optimistic) ≥ " +> actual <+ " (actual)"
        delay $ 12 * hour

  runConc $
    assetPairUpdater
      <|> assetPairUpdateProducer
      <|> rebalanceConsumer
      <|> rebalanceScheduler
      <|> buyer
      <|> orderTracker
      <|> statusLogger

data TypedConfig (base :: Symbol) quote = TypedConfig
  { withFunds :: Money quote,
    frequency :: Frequency,
    minimumAmount :: Money quote
  }
  deriving (Generic, Show)

withTypedConfig :: Monad m => (forall base quote. (KnownSymbol base, KnownSymbol quote) => StrategyT (TypedConfig base quote) m a) -> ScheduledBuysStrategy m (Either Errors a)
withTypedConfig m = do
  Env {config = Config {..}} <- ask
  withSomeMoney withFunds $ \(withFunds' :: Money quote) ->
    withSomeMoney minimumAmount $ \minimumAmount' ->
      withSomeMoney (fromJust $ mkSomeDense (Text.decodeUtf8 . coerce $ buy) 0) $ \(_ :: Money base) ->
        runValidateT $ do
          validWithFunds <-
            whenNothing
              ( do
                  funds <- money . toRational $ withFunds'
                  guard $ funds > 0
                  return funds
              )
              $ refute $ pure "Funds must be a positive monetary value"
          validMinimumAmount <-
            whenNothing (money . toRational $ minimumAmount') $
              refute $ pure "Minimum amount of funds must be a non-negative monetary value"
          let withFundsAsset = fundsAssetId withFunds
              minimumFundsAsset = fundsAssetId minimumAmount
          when (withFundsAsset /= minimumFundsAsset) $
            refute $
              pure $
                "Denomination of funds (" +> withFundsAsset
                  <+ ") and the minimum amount (" +> minimumFundsAsset
                  <+ ") are inconsistent"
          when (buy == withFundsAsset) $
            refute $
              pure $
                "Denomination of funds and the asset to buy are equal (" +> buy
                  <+ ")"
          let config :: TypedConfig base quote =
                TypedConfig
                  { withFunds = validWithFunds,
                    minimumAmount = validMinimumAmount,
                    ..
                  }
          lift $ StrategyT (withReaderT (mapEnvConfig (const config)) $ unStrategyT m)

updateAssetPair' :: TMVar UpdateAssetPair -> STM Bool
updateAssetPair' = flip tryPutTMVar UpdateAssetPair

rebalance' :: TMVar Rebalance -> STM ()
rebalance' = void <$> flip tryPutTMVar Rebalance

scheduleRebalance' :: TMVar ScheduleRebalance -> UTCTime -> STM ()
scheduleRebalance' v = putTMVar v . ScheduleRebalance

onOrderFailed' :: TVar (Money quote) -> TMVar Rebalance -> TMVar UpdateAssetPair -> Money quote -> STM ()
onOrderFailed' optimisticVar rebalanceMVar updateAssetPairMVar funds = void $ do
  modifyTVar optimisticVar $ subtract funds
  rebalance' rebalanceMVar RebalanceNow
  updateAssetPair' updateAssetPairMVar

completeOrder' :: TVar (Money quote) -> TChan Rational -> TChan Rational -> Money quote -> Order base -> STM ()
completeOrder' actualVar filledSizeBroadcastChan spentFundsBroadcastChan funds Order {filledSize} = do
  modifyTVar actualVar (+ funds)
  writeTChan filledSizeBroadcastChan $ toRational filledSize
  writeTChan spentFundsBroadcastChan $ toRational funds

minimumBuys :: AssetPair base quote -> Money quote -> (Sum (Money quote), [Money quote])
minimumBuys AssetPair {..} = minimumSplits minMarketFunds maxMarketFunds quoteIncrement

assetPairUpdateInterval :: Num a => a
assetPairUpdateInterval = 1 * hour

adjustAssetPair :: (MonadReader env m, HasLog env Message m, KnownSymbol quote) => Money quote -> AssetPair base quote -> m (AssetPair base quote)
adjustAssetPair minimumAmount assetPair@AssetPair {..}
  | minimumAmount > minMarketFunds,
    quoteIncrement > 0 = do
    let increments = fromMaybe 0 . money . toRational @Integer . ceiling $ toRational (minimumAmount - minMarketFunds) / toRational quoteIncrement
        adjustedMinMarketFunds = minMarketFunds + increments * quoteIncrement
    case maxMarketFunds of
      Just finiteMaxMarketFunds | adjustedMinMarketFunds > finiteMaxMarketFunds -> do
        log Warning $
          "Minimum amount " +> minimumAmount
            <+ " implies that the minimum market funds is larger than the maximum " +> finiteMaxMarketFunds
            <+ " for pair " +> (base, quote)
        return $ assetPair {minMarketFunds = finiteMaxMarketFunds}
      _ -> do
        log Info $
          "Minimum amount " +> minimumAmount
            <+ " implies adjustment of the minimum market funds to " +> adjustedMinMarketFunds
            <+ " for pair " +> (base, quote)
        return $ assetPair {minMarketFunds = adjustedMinMarketFunds}
  | otherwise = return assetPair
