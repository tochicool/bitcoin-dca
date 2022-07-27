{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module BitcoinDCA.Explorer.Esplora where

import BitcoinDCA.Explorer
import qualified BitcoinDCA.Explorer.Esplora.API as API
import BitcoinDCA.Types
import Control.Arrow ((>>>))
import Control.Concurrent.Forkable (ForkableMonad (forkIO))
import Control.Monad (void)
import Control.Monad.Except (MonadError, MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Coerce (coerce)
import Servant.Client (ClientError, ClientM)
import Servant.Client.Internal.HttpClient.Extended ()
import UnliftIO (MonadUnliftIO)

newtype Esplora a = Esplora {unEsplora :: ReaderT API.Config ClientM a}
  deriving newtype (Functor, Applicative, Monad, MonadError ClientError, MonadIO, MonadUnliftIO)

instance MonadExplorer Esplora where
  type ExplorerError Esplora = ClientError

  type ExplorerConfig Esplora = API.Config
  isAddressUsed _ address = Esplora . lift $ do
    API.GetAddressResponse {..} <- API.getAddress $ coerce address
    return $
      API.txCount chainStats > 0
        || API.txCount mempoolStats > 0

instance MonadIO m => RunnableExplorer m Esplora where
  runExplorer config =
    unEsplora
      >>> flip runReaderT config
      >>> API.authenticateAndRunClientM config
      >>> liftIO

instance ForkableMonad Esplora where
  forkIO m = do
    config <- Esplora ask
    liftIO . forkIO . void . runExplorer config $ m
