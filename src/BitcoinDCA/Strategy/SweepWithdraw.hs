{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module BitcoinDCA.Strategy.SweepWithdraw where

import BitcoinDCA.Common
import BitcoinDCA.Exchange
  ( MonadExchange (getAsset, getWithdraw),
  )
import qualified BitcoinDCA.Exchange as Exchange
import BitcoinDCA.Explorer (MonadExplorer)
import qualified BitcoinDCA.Explorer as Explorer
import BitcoinDCA.Strategy
import BitcoinDCA.Types
import Colog (Severity (Error, Info), log)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), catchError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
  ( MonadReader (ask),
    forever,
    void,
    withReaderT,
  )
import Control.Monad.Trans (lift)
import Control.Monad.Validate (MonadValidate (refute), runValidateT)
import Control.Retry
  ( RetryStatus (RetryStatus, rsIterNumber),
    fullJitterBackoff,
    retrying,
  )
import Data.Aeson (FromJSON, ToJSON, genericParseJSON, genericToJSON)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.List (genericSplitAt)
import Data.Maybe (fromJust, fromMaybe)
import Data.Semigroup (Sum (Sum))
import qualified Data.Text.Encoding as Text
import Data.Word (Word32)
import Data.Yaml.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol)
import qualified Haskoin as Bitcoin
import qualified Language.Bitcoin.Script.Descriptors as Desc
import Money (ExchangeRate, exchangeRate, mkSomeDense)
import UnliftIO.Async (conc, runConc)
import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.STM
import Prelude hiding (log)

type SweepWithdrawStrategy = StrategyT Config

data Config = Config
  { withdraw :: AssetId,
    address :: Maybe Address,
    outputDescriptor :: Maybe OutputDescriptor,
    minimumSize :: Maybe Size,
    minimumFunds :: Maybe Funds
  }
  deriving (Generic, Show)

instance FromJSON Config where
  parseJSON =
    genericParseJSON jsonConfigOptions

instance ToJSON Config where
  toJSON = genericToJSON jsonConfigOptions

data UpdateAsset = UpdateAsset

data OnBalanceChangedEvent = OnBalanceChangedEvent

data WithdrawEvent base quote = WithdrawEvent (Money base) (Maybe (ExchangeRate base quote))

data PendingWithdraw base quote = PendingWithdraw WithdrawId (WithdrawEvent base quote)

sweepWithdraw :: MonadStrategy Config e m => SweepWithdrawStrategy m (Either Errors a)
sweepWithdraw = withTypedConfig $ do
  Env {config = conf@TypedConfig {..} :: TypedConfig base quote} <- ask
  log Info $ "Initialising sweep withdraw strategy: " +> conf
  assetMVar <- newEmptyTMVarIO
  updateAssetMVar <- newTMVarIO UpdateAsset
  onBalanceChangedEventMVar <- newEmptyTMVarIO
  withdrawEventChan <- newTChanIO
  pendingWithdrawsChan <- newTChanIO
  filledSizeBroadcastChan <- askFilledSizeBroadcastChan $ assetId @base
  spentFundsBroadcastChan <- askSpentFundsBroadcastChan $ assetId @quote
  filledSizeChan <- atomically $ dupTChan filledSizeBroadcastChan
  spentFundsChan <- atomically $ dupTChan spentFundsBroadcastChan
  filledTotalVar <- newTVarIO 0
  spentTotalVar <- newTVarIO 0
  descriptorAddressesVar <- newTVarIO $ case destination of
    DestinationOutputDescriptor desc -> fromBitcoinAddress <$> deriveAddresses minBound maxBound desc
    _ -> []

  let updateAsset = updateAsset' updateAssetMVar
      onWithdrawFailed = onWithdrawFailed' filledTotalVar spentTotalVar updateAssetMVar

      assetUpdater = conc . forever $ do
        UpdateAsset <- atomically . takeTMVar $ updateAssetMVar
        retrying
          (fullJitterBackoff baseRetryDelay)
          (const return)
          $ \RetryStatus {rsIterNumber} -> localLogMessage ("[Retry " +> rsIterNumber <+ "] " +>) $ do
            log Info $ "Retrieving asset " +> assetId @base
            result <- (Right <$> getAsset @_ @base) `catchError` (return . Left)
            case result of
              Right (Just newAsset) -> do
                result <-
                  atomically $
                    Just <$> takeTMVar assetMVar
                      <|> Nothing <$ putTMVar assetMVar newAsset
                case result of
                  Just oldAsset
                    | oldAsset == newAsset ->
                      log Info $ "No change in asset " +> newAsset
                    | otherwise ->
                      log Info $ "Updated asset from " +> oldAsset <+ " to " +> newAsset
                  Nothing ->
                    log Info $ "Updated asset " +> newAsset
                return False
              Right Nothing -> do
                log Error $ "Asset with id=" +> assetId @base <+ " does not exist"
                return True
              Left err -> do
                log Error $ "An error occured whilst retrieving asset with id=" +> assetId @base <+ ": " +> err
                return True

      assetUpdateScheduler = conc . forever $ do
        threadDelay assetUpdateInterval
        atomically updateAsset

      filledTotalUpdater = conc . forever $ do
        atomically $ do
          filledSize <- readTChan filledSizeChan
          modifyTVar filledTotalVar (+ fromMaybe 0 (money filledSize))
          tryPutTMVar onBalanceChangedEventMVar OnBalanceChangedEvent

      spentTotalUpdater = conc . forever $ do
        atomically $ do
          spentFunds <- readTChan spentFundsChan
          modifyTVar spentTotalVar (+ fromMaybe 0 (money spentFunds))
          tryPutTMVar onBalanceChangedEventMVar OnBalanceChangedEvent

      balanceChangeHandler = conc . forever $ do
        OnBalanceChangedEvent <- atomically $ takeTMVar onBalanceChangedEventMVar
        log Info "Balance changed event received"
        (withdrew, filledLeft, spentLeft, withdrawals) <- atomically $ do
          filledTotal <- readTVar filledTotalVar
          spentTotal <- readTVar spentTotalVar
          asset <- readTMVar assetMVar
          case () of
            _
              | filledTotal > 0 && spentTotal >= 0
                  && minWithdrawalAmount asset <= filledTotal
                  && minimumSize <= filledTotal
                  && minimumFunds <= spentTotal -> do
                let (Sum availableFunds, withdrawals) = minimumWithdraws asset filledTotal
                writeTVar filledTotalVar availableFunds
                let maybePrice = exchangeRate $ toRational spentTotal / toRational filledTotal
                whenJust maybePrice $ \price ->
                  writeTVar spentTotalVar $ price `exchangeMoney` availableFunds
                spentRemaining <- readTVar spentTotalVar
                let withdrawEvents = WithdrawEvent <$> withdrawals <*> [maybePrice]
                writeTChan withdrawEventChan `traverse_` withdrawEvents
                return (True, availableFunds, spentRemaining, withdrawals)
              | otherwise -> return (False, filledTotal, spentTotal, [])
        if withdrew
          then do
            let withdrawCount = length withdrawals
            log Info $
              "Balance change triggered " +> withdrawCount
                <+ case withdrawCount of 1 -> " withdrawal "; _ -> " withdrawals "
                +> withdrawals
          else log Info "Balance changed event did not meet conditions for withdraw"
        log Info $ filledLeft <+ " left to withdraw from purchases with an estimated cost basis of " +> spentLeft

      withdrawer = conc . forever $ do
        event@(WithdrawEvent amount _) <- atomically . readTChan $ withdrawEventChan
        log Info "Withdraw event received"
        forkIO $ do
          withdrawId <-
            withdrawDestination descriptorAddressesVar destination amount `catchError` \err -> do
              log Error $ "An error occurred whilst making withdraw: " +> err
              threadDelay $ 10 * second
              atomically . onWithdrawFailed $ event
              log Info "Adjusted internal balances"
              throwError err
          atomically . writeTChan pendingWithdrawsChan $ PendingWithdraw withdrawId event
          log Info $ "Withdraw with id=" +> withdrawId <+ " for amount " +> amount <+ " received by exchange and pending completion"

      withdrawTracker = conc . forever $ do
        PendingWithdraw withdrawId event <- atomically . readTChan $ pendingWithdrawsChan
        forkIO $
          void $
            retrying
              (fullJitterBackoff baseRetryDelay)
              (const return)
              $ \RetryStatus {rsIterNumber} -> localLogMessage ("[Retry " +> rsIterNumber <+ "] " +>) $ do
                log Info $ "Retrieving withdraw with id=" +> withdrawId
                result <- (Right <$> getWithdraw withdrawId) `catchError` (return . Left)
                case result of
                  Left err -> do
                    log Error $ "An error occured whilst retrieving withdraw with id=" +> withdrawId <+ ": " +> err
                    return True
                  Right (Just withdraw@Withdraw {status = WithdrawPending}) -> do
                    log Info $ "Withdraw with id=" +> withdrawId <+ " is still pending - will check again: " +> withdraw
                    return True
                  Right (Just withdraw@Withdraw {status = WithdrawComplete}) -> do
                    log Info $ "Withdraw with id=" +> withdrawId <+ " is complete: " +> withdraw
                    return False
                  Right (Just withdraw) -> do
                    log Error $ "Withdraw with id=" +> withdrawId <+ " is in an unexpected state: " +> withdraw
                    atomically . onWithdrawFailed $ event
                    return False
                  Right Nothing -> do
                    log Error $ "Withdraw with id=" +> withdrawId <+ " was not found"
                    atomically . onWithdrawFailed $ event
                    return False

      statusLogger = conc . forever $ do
        (filledLeft, spentLeft) <-
          atomically $
            (,)
              <$> readTVar filledTotalVar
              <*> readTVar spentTotalVar
        log Info $ filledLeft <+ " left to withdraw from purchases with an estimated cost basis of " +> spentLeft
        threadDelay $ 12 * hour

  runConc $
    assetUpdater
      <|> assetUpdateScheduler
      <|> filledTotalUpdater
      <|> spentTotalUpdater
      <|> balanceChangeHandler
      <|> withdrawer
      <|> withdrawTracker
      <|> statusLogger

data TypedConfig (base :: Symbol) quote = TypedConfig
  { destination :: Destination,
    minimumSize :: Money base,
    minimumFunds :: Money quote
  }
  deriving (Generic, Show)

data Destination
  = DestinationAddress Address
  | DestinationOutputDescriptor Desc.OutputDescriptor
  deriving (Generic, Show)

withTypedConfig :: Monad m => (forall base quote. (KnownSymbol base, KnownSymbol quote) => StrategyT (TypedConfig base quote) m a) -> SweepWithdrawStrategy m (Either Errors a)
withTypedConfig m = do
  Env {config = Config {..}} <- ask
  let zero = fromJust $ mkSomeDense (Text.decodeUtf8 . coerce $ withdraw) 0
  withSomeMoney (fromMaybe (Size zero) minimumSize) $ \(minimumSize' :: Money base) ->
    withSomeMoney (fromMaybe (Funds zero) minimumFunds) $ \(minimumFunds' :: Money quote) ->
      runValidateT $ do
        validMinimumSize <-
          whenNothing (money . toRational $ minimumSize') $
            refute $ pure "Minimum withdraw size must be a non-negative monetary value"
        validMinimumFunds <-
          whenNothing (money . toRational $ minimumFunds') $
            refute $ pure "Minimum amount of funds must be a non-negative monetary value"
        let minimumSizeAsset = maybe withdraw sizeAssetId minimumSize
            minimumFundsAsset = maybe "" fundsAssetId minimumFunds
        when (withdraw /= minimumSizeAsset) $
          refute $
            pure $
              "Denomination of the asset to withdraw (" +> withdraw
                <+ ") and the denomination of the minimum size (" +> minimumSizeAsset
                <+ ") are inconsistent"
        when (withdraw == minimumFundsAsset) $
          refute $
            pure $
              "Denomination of the minimum funds and the asset to buy are equal (" +> withdraw
                <+ ")"

        if isShitcoin @base
          then whenJust outputDescriptor $ \_ ->
            refute $ pure "Output descriptors are only supported for Bitcoin withdraws"
          else whenJust address $ \(Address addr) ->
            void $
              whenNothing (Bitcoin.textToAddr Bitcoin.btc (Text.decodeUtf8 addr)) $
                refute $ pure $ "Bitcoin withdraw address is invalid: " +> addr

        destination <- case (address, outputDescriptor) of
          (Just address, Nothing) -> pure $ DestinationAddress address
          (Nothing, Just (OutputDescriptor desc)) -> pure $ DestinationOutputDescriptor desc
          _ -> refute $ pure "Exactly one of an address or an output descriptor much be provided for a withdraw destination"

        whenJust outputDescriptor $ \(OutputDescriptor desc) -> do
          when (null $ deriveAddresses minBound maxBound desc) $
            refute $
              pure $
                "Could not derive any destination addresses from " +> desc
                  <+ " either because the descriptor is not ranged"
                  <+ ", the output script is Pay-to-Public-Key (P2PK)"
                  <+ ", or the output has a non-standard script"

        let config :: TypedConfig base quote =
              TypedConfig
                { minimumSize = validMinimumSize,
                  minimumFunds = validMinimumFunds,
                  ..
                }
        lift $ StrategyT (withReaderT (mapEnvConfig (const config)) $ unStrategyT m)

withdrawDestination :: forall asset m. (MonadExchange m, MonadExplorer m, MonadIO m, KnownSymbol asset) => TVar [Address] -> Destination -> Money asset -> m WithdrawId
withdrawDestination _ (DestinationAddress address) amount = Exchange.withdraw address amount
withdrawDestination descriptorAddressesVar DestinationOutputDescriptor {} amount = do
  addresses <- readTVarIO descriptorAddressesVar
  unusedAddresses <- findFirstAscM (fmap not . Explorer.isAddressUsed "BTC") addresses
  case unusedAddresses of
    [] -> error "No unused addresses left to withdraw to"
    address : addresses' -> do
      atomically $ writeTVar descriptorAddressesVar addresses'
      Exchange.withdraw address amount

-- | Derive a set of addresses associated with a ranged output descriptor
-- within the given index range. The list will be empty if:
--
--   * the output is p2pk
--   * the output has a non-standard script
--   * the range is empty
--
-- This can derive more addresses than the size of the range in the case of
-- the "combo" construct. The indexes of all keys will be the same to avoid a
-- combinatorial blow up in a same way as the 'deriveaddresses' RPC in Bitcoin
-- Core.
deriveAddresses :: Word32 -> Word32 -> Desc.OutputDescriptor -> [Bitcoin.Address]
deriveAddresses lowerBound upperBound
  | lowerBound <= upperBound =
    concatMap Desc.descriptorAddresses . \case
      Desc.ScriptPubKey descriptor -> Desc.ScriptPubKey <$> deriveScripts descriptor
      Desc.P2SH descriptor -> Desc.P2SH <$> deriveScripts descriptor
      Desc.P2WPKH key -> Desc.P2WPKH <$> deriveKeys key
      Desc.P2WSH descriptor -> Desc.P2WSH <$> deriveScripts descriptor
      Desc.WrappedWPkh key -> Desc.WrappedWPkh <$> deriveKeys key
      Desc.WrappedWSh descriptor -> Desc.WrappedWSh <$> deriveScripts descriptor
      Desc.Combo key -> Desc.Combo <$> deriveKeys key
      Desc.Addr addr -> [Desc.Addr addr]
  | otherwise = const []
  where
    deriveScripts = \case
      Desc.Pk desc -> Desc.Pk <$> deriveKeys desc
      Desc.Pkh key -> Desc.Pkh <$> deriveKeys key
      Desc.Multi m keys -> Desc.Multi m <$> traverse deriveKeys keys
      Desc.SortedMulti m keys -> Desc.SortedMulti m <$> traverse deriveKeys keys
      _ -> mempty
    deriveKeys = \case
      desc@(Desc.KeyDescriptor origin key)
        | Desc.isDefinite desc -> pure desc
        | otherwise -> Desc.KeyDescriptor origin <$> [Desc.keyAtIndex i key | i <- [lowerBound .. upperBound]]

fromBitcoinAddress :: Bitcoin.Address -> Address
fromBitcoinAddress addr = Address $ Text.encodeUtf8 $ fromJust $ Bitcoin.addrToText Bitcoin.btc addr

-- | @findFirstAscM p xs@ finds the longest suffix @y:ys@ of @xs@ such that
-- @p y@ holds or the empty list if no such suffix exists. This assumes that
-- @xs@ is monotonically non-decreasing in terms of @p@.
findFirstAscM :: Monad m => (a -> m Bool) -> [a] -> m [a]
findFirstAscM p xs = findFirstAscM' (1 :: Integer) xs xs
  where
    findFirstAscM' _ _ [] = return []
    findFirstAscM' i skipped ys@(x : xs) =
      p x >>= \case
        False -> uncurry (findFirstAscM' (2 * i)) $ genericSplitAt i xs
        True -> findFirstAscM'' (1 :: Integer) [] skipped ys
    findFirstAscM'' _ [] [] upperBound = return upperBound
    findFirstAscM'' _ lowerBound [] upperBound = findFirstAscM'' 1 [] lowerBound upperBound
    findFirstAscM'' i lowerBound (x : xs) upperBound =
      p x >>= \case
        False -> uncurry (findFirstAscM'' (2 * i)) (genericSplitAt i xs) upperBound
        True -> findFirstAscM'' 1 [] lowerBound (x : xs ++ upperBound)

updateAsset' :: TMVar UpdateAsset -> STM Bool
updateAsset' = flip tryPutTMVar UpdateAsset

onWithdrawFailed' :: TVar (Money base) -> TVar (Money quote) -> TMVar UpdateAsset -> WithdrawEvent base quote -> STM ()
onWithdrawFailed' filledTotalVar spentTotalVar updateAssetMVar (WithdrawEvent amount maybePrice) = void $ do
  modifyTVar filledTotalVar (+ amount)
  case maybePrice of
    Just price -> modifyTVar spentTotalVar (+ price `exchangeMoney` amount)
    _ -> return ()
  updateAsset' updateAssetMVar

minimumWithdraws :: Asset asset -> Money asset -> (Sum (Money asset), [Money asset])
minimumWithdraws Asset {..} = minimumSplits minWithdrawalAmount maxWithdrawalAmount maxPrecision

assetUpdateInterval :: Num a => a
assetUpdateInterval = 1 * hour
