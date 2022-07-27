{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module BitcoinDCA.External where

import BitcoinDCA.Exchange (MonadExchange (..), RunnableExchange (runExchange))
import BitcoinDCA.Explorer (MonadExplorer (ExplorerConfig, ExplorerError, isAddressUsed), RunnableExplorer (runExplorer))
import Control.Arrow ((>>>))
import Control.Concurrent.Forkable (ForkableMonad (forkIO))
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), MonadIO (liftIO), runExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT (ReaderT, runReaderT), asks)
import UnliftIO (Exception, MonadUnliftIO (withRunInIO), throwIO, try)

newtype External a = External
  { unExternal :: ReaderT Config (ExceptT Error IO) a
  }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError Error, MonadIO)

instance MonadExchange External where
  type ExchangeConfig External = Config
  type ExchangeError External = Error
  getAssetPair = exchange getAssetPair
  placeMarketBuyOrder funds = exchange $ placeMarketBuyOrder funds
  getOrder id = exchange $ getOrder id
  getAsset = exchange getAsset
  withdraw address amount = exchange $ withdraw address amount
  getWithdraw id = exchange $ getWithdraw id

instance MonadExplorer External where
  type ExplorerConfig External = Config
  type ExplorerError External = Error
  isAddressUsed asset address = explore $ isAddressUsed asset address

instance MonadUnliftIO External where
  withRunInIO inner =
    External $
      ReaderT $ \env -> ExceptT $
        try $
          withRunInIO $ \run ->
            inner (run . (either throwIO pure <=< runExternal env))

instance ForkableMonad External where
  forkIO m = do
    r <- ask
    forkIO . runExternal r $ m

data Config = Config
  { someExchangeConfig :: SomeExchangeConfig,
    someExplorerConfig :: SomeExplorerConfig
  }

data SomeExchangeConfig = forall exchange. (RunnableExchange IO exchange, Show (ExchangeError exchange)) => SomeExchangeConfig (ExchangeConfig exchange)

data SomeExplorerConfig = forall explorer. (RunnableExplorer IO explorer, Show (ExplorerError explorer)) => SomeExplorerConfig (ExplorerConfig explorer)

data Error
  = forall exchange. Show (ExchangeError exchange) => SomeExchangeError (ExchangeError exchange)
  | forall explorer. Show (ExplorerError explorer) => SomeExplorerError (ExplorerError explorer)

deriving instance Show Error

instance Exception Error

runExternal :: MonadIO m => Config -> External a -> m (Either Error a)
runExternal config =
  unExternal
    >>> flip runReaderT config
    >>> runExceptT
    >>> liftIO

exchange :: forall a. (forall exchange. (RunnableExchange IO exchange, Show (ExchangeError exchange)) => exchange a) -> External a
exchange f =
  asks someExchangeConfig >>= \case
    SomeExchangeConfig (config :: ExchangeConfig exchange) ->
      liftIO (runExchange config f) >>= \case
        Left err -> throwError $ SomeExchangeError err
        Right x -> return x

explore :: forall a. (forall explorer. (RunnableExplorer IO explorer, Show (ExplorerError explorer)) => explorer a) -> External a
explore f =
  asks someExplorerConfig >>= \case
    SomeExplorerConfig (config :: ExplorerConfig explorer) ->
      liftIO (runExplorer config f) >>= \case
        Left err -> throwError $ SomeExplorerError err
        Right x -> return x
