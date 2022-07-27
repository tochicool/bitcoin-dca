{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Client.Internal.HttpClient.Extended where

import UnliftIO (MonadUnliftIO (withRunInIO))
import Servant.Client.Internal.HttpClient (ClientM (ClientM), runClientM)
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Exception (try, throwIO)
import Control.Monad ((<=<))

instance MonadUnliftIO ClientM where
  withRunInIO inner =
    ClientM $ ReaderT $ \env -> ExceptT $ try $
      withRunInIO $ \run ->
        inner (run . (either throwIO pure <=< (`runClientM` env)))
