{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module BitcoinDCA.Explorer where

import BitcoinDCA.Types
import Control.Concurrent.Forkable (ForkableMonad)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Functor.Const (Const)
import UnliftIO (MonadUnliftIO)

-- | A computation that explores timechains
class Monad m => MonadExplorer m where
  type ExplorerError m = e | e -> m
  type ExplorerConfig m = c | c -> m
  isAddressUsed :: AssetId -> Address -> m Bool

class MonadExplorer m => RunnableExplorer n m where
  runExplorer :: ExplorerConfig m -> m a -> n (Either (ExplorerError m) a)

instance (ForkableMonad (t m), MonadUnliftIO (t m), MonadExplorer m, MonadTrans t, Monad (t m)) => MonadExplorer (t m) where
  type ExplorerError (t m) = Const (ExplorerError m) t
  type ExplorerConfig (t m) = Const (ExplorerConfig m) t
  isAddressUsed asset = lift . isAddressUsed asset
