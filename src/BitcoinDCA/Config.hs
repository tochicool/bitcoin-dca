{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module BitcoinDCA.Config where

import BitcoinDCA.Common
import BitcoinDCA.Exchange (ExchangeConfig, MonadExchange (ExchangeError), RunnableExchange)
import BitcoinDCA.Exchange.Coinbase (Coinbase)
import BitcoinDCA.Explorer (ExplorerConfig, MonadExplorer (ExplorerError), RunnableExplorer)
import BitcoinDCA.Explorer.Esplora (Esplora)
import qualified BitcoinDCA.External as External
import BitcoinDCA.Strategy (MonadStrategy, State (filledSizeBroadcastChans, spentFundsBroadcastChans))
import qualified BitcoinDCA.Strategy as Strategy
import qualified BitcoinDCA.Strategy.ScheduledBuys as ScheduledBuys
import qualified BitcoinDCA.Strategy.SweepWithdraw as SweepWithdraw
import BitcoinDCA.Types
import Colog
import Control.Monad (foldM)
import Control.Monad.Except (MonadIO)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Object, String),
    genericParseJSON,
    genericToJSON,
    withObject,
    withText,
    (.:),
  )
import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import qualified Data.SemVer as SemVer
import qualified Data.Text as Text
import GHC.Generics
import System.Exit (exitFailure, exitSuccess)
import UnliftIO (MonadIO (liftIO), conc, runConc)
import UnliftIO.STM (newBroadcastTChanIO)

data Config = Config
  { version :: Version,
    exchanges :: ExchangesConfig,
    explorers :: ExplorersConfig
  }
  deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON jsonConfigOptions
    . ("explorers" ?:. Object mempty)

instance ToJSON Config where
  toJSON = genericToJSON jsonConfigOptions

newtype Version = Version SemVer.Version
  deriving (Show)

instance FromJSON Version where
  parseJSON = withText "SemVer" $ \t ->
    Version <$> case SemVer.fromText t of
      Left err -> fail err
      Right v
        | v < SemVer.incrementMajor configVersion -> pure v
        | otherwise -> fail $ "the version " <> SemVer.toString v <> " is incompatible with the expected config version " <> SemVer.toString configVersion

instance ToJSON Version where
  toJSON (Version v) = String . SemVer.toText $ v

newtype ExchangesConfig = ExchangesConfig
  { coinbase :: Maybe (ExchangeStrategyConfig Coinbase)
  }
  deriving (Generic, Show)

instance FromJSON ExchangesConfig where
  parseJSON = genericParseJSON jsonConfigOptions

instance ToJSON ExchangesConfig where
  toJSON = genericToJSON jsonConfigOptions

data ExchangeStrategyConfig exchange = ExchangeStrategyConfig
  { config :: ExchangeConfig exchange,
    strategies :: [StrategyConfig]
  }
  deriving (Generic)

deriving instance Show (ExchangeConfig exchange) => Show (ExchangeStrategyConfig exchange)

instance FromJSON (ExchangeConfig exchange) => FromJSON (ExchangeStrategyConfig exchange) where
  parseJSON = withObject "ExchangeStrategy Config" $ \v ->
    ExchangeStrategyConfig
      <$> parseJSON (Object $ HashMap.delete "strategies" v)
      <*> v .: "strategies"

instance ToJSON (ExchangeConfig exchange) => ToJSON (ExchangeStrategyConfig exchange) where
  toJSON ExchangeStrategyConfig {..} =
    let Object o = toJSON config
     in Object $ HashMap.insert "strategies" (toJSON strategies) o

data StrategyConfig
  = ScheduledBuys ScheduledBuys.Config
  | SweepWithdraw SweepWithdraw.Config
  deriving (Generic, Show)

instance FromJSON StrategyConfig where
  parseJSON = withObject "StrategyConfig" $ \v ->
    (v .: "type")
      >>= withText
        "StrategyType"
        ( \case
            "ScheduledBuys" -> ScheduledBuys <$> parseJSON (Object $ HashMap.delete "type" v)
            "SweepWithdraw" -> SweepWithdraw <$> parseJSON (Object $ HashMap.delete "type" v)
            strategy -> fail $ "unknown strategy type: " <> Text.unpack strategy
        )

instance ToJSON StrategyConfig where
  toJSON = \case
    ScheduledBuys x -> let Object o = toJSON x in Object $ HashMap.insert "type" "ScheduledBuys" o
    SweepWithdraw x -> let Object o = toJSON x in Object $ HashMap.insert "type" "SweepWithdraw" o

newtype ExplorersConfig = ExplorersConfig
  { esplora :: ExplorerConfig Esplora
  }
  deriving (Generic, Show)

instance FromJSON ExplorersConfig where
  parseJSON = genericParseJSON jsonConfigOptions
    . ("esplora" ?:. Object mempty)

instance ToJSON ExplorersConfig where
  toJSON = genericToJSON jsonConfigOptions

configVersion :: SemVer.Version
configVersion = SemVer.version 2 0 0 [] []


fromConfig :: forall m. MonadIO m => Config -> m ()
fromConfig Config {exchanges = ExchangesConfig {..}, explorers = ExplorersConfig {..}} = do
  _ <- whenJust coinbase (fromExchangeConfig esplora)
  return ()

fromExchangeConfig ::
  forall exchange explorer m.
  ( MonadIO m,
    RunnableExchange IO exchange,
    RunnableExplorer IO explorer,
    Show (ExchangeError exchange),
    Show (ExplorerError explorer)
  ) =>
  ExplorerConfig explorer ->
  ExchangeStrategyConfig exchange ->
  m ()
fromExchangeConfig explorerConfig ExchangeStrategyConfig {..} = do
  let externalConfig =
        External.Config
          { someExchangeConfig = External.SomeExchangeConfig config,
            someExplorerConfig = External.SomeExplorerConfig explorerConfig
          }
  result <- External.runExternal externalConfig $ do
    st <- initialStrategyState strategies
    runConc $ asum $ conc . fromStrategyConfig st <$> strategies
  liftIO $ case result of
    Left err -> do
      print err
      exitFailure
    Right _ ->
      exitSuccess

initialStrategyState :: MonadIO m => [StrategyConfig] -> m Strategy.State
initialStrategyState =
  foldM
    ( \st@Strategy.State {..} -> \case
        ScheduledBuys ScheduledBuys.Config {..} -> do
          filled <- newBroadcastTChanIO
          spent <- newBroadcastTChanIO
          return $
            st
              { filledSizeBroadcastChans = Map.insert buy filled filledSizeBroadcastChans,
                spentFundsBroadcastChans = Map.insert (fundsAssetId withFunds) spent spentFundsBroadcastChans
              }
        SweepWithdraw SweepWithdraw.Config {..} -> do
          filled <- newBroadcastTChanIO
          spent <- newBroadcastTChanIO
          return $
            st
              { filledSizeBroadcastChans = Map.insert withdraw filled filledSizeBroadcastChans,
                spentFundsBroadcastChans = case minimumFunds of
                  Just funds -> Map.insert (fundsAssetId funds) spent spentFundsBroadcastChans
                  Nothing -> spentFundsBroadcastChans
              }
    )
    Strategy.State
      { filledSizeBroadcastChans = mempty,
        spentFundsBroadcastChans = mempty
      }

fromStrategyConfig :: MonadStrategy c e m => Strategy.State -> StrategyConfig -> m ()
fromStrategyConfig state strategyConfig = do
  case strategyConfig of
    ScheduledBuys config -> Strategy.reportErrorsStrategyT Strategy.Env {logAction = richMessageAction, ..} ScheduledBuys.scheduledBuys
    SweepWithdraw config -> Strategy.reportErrorsStrategyT Strategy.Env {logAction = richMessageAction, ..} SweepWithdraw.sweepWithdraw
