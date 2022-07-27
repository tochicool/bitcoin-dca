{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module BitcoinDCA.Strategy where

import BitcoinDCA.Common
import BitcoinDCA.Exchange
import BitcoinDCA.Explorer (MonadExplorer)
import BitcoinDCA.Types
import Colog (HasLog (..), LogAction (..), Message, Msg (Msg, msgText), Severity (Error), cmap, hoistLogAction, log)
import Control.Concurrent.Forkable (ForkableMonad (forkIO))
import Control.Concurrent.STM (TChan)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Sum (Sum))
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import UnliftIO (MonadUnliftIO, newBroadcastTChanIO)
import Prelude hiding (log)

newtype StrategyT c m a = StrategyT {unStrategyT :: ReaderT (Env c m) m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (Env c m),
      MonadUnliftIO
    )

data Env c m = Env
  { logAction :: LogAction (StrategyT c m) Message,
    state :: State,
    config :: c
  }

data State = State
  { filledSizeBroadcastChans :: Map AssetId (TChan Rational),
    spentFundsBroadcastChans :: Map AssetId (TChan Rational)
  }

type Errors = Seq Text

instance Monad m => HasLog (Env c m) Message (StrategyT c m) where
  {-# INLINE getLogAction #-}
  getLogAction = logAction

  {-# INLINE setLogAction #-}
  setLogAction logAction env = env {logAction}

instance MonadError e m => MonadError e (StrategyT c m) where
  throwError e = lift $ throwError e
  catchError (StrategyT (ReaderT reader)) f = StrategyT $
    ReaderT $ \env ->
      reader env `catchError` (runStrategyT env . f)

instance ForkableMonad m => ForkableMonad (StrategyT c m) where
  forkIO m = do
    r <- ask
    lift . forkIO . flip runReaderT r . unStrategyT $ m

instance MonadTrans (StrategyT c) where
  lift = StrategyT . lift

type MonadStrategy c e m =
  (MonadExchange m, MonadExplorer m, MonadError e m, LogShow (IsText e) e, Show e, MonadUnliftIO m, ForkableMonad m)

reportErrorsStrategyT :: Monad m => Env c m -> StrategyT c m (Either Errors a) -> m ()
reportErrorsStrategyT env m =
  runStrategyT env $
    m >>= \case
      Left errors -> do
        log Error $
          "The following errors where found in the configuration:"
            <> Text.Lazy.toStrict (Text.Builder.toLazyText (foldMap (\err -> "\n - " <> Text.Builder.fromText err) errors))
      _ -> return ()

runStrategyT :: Env c m -> StrategyT c m a -> m a
runStrategyT env = flip runReaderT env . unStrategyT

localLogMessage :: forall r m a. (MonadReader r m, HasLog r Message m) => (Text -> Text) -> m a -> m a
localLogMessage f = local $
  overLogAction @_ @Message @m $
    cmap $
      \m@Msg {msgText} -> m {msgText = f msgText}

minimumSplits :: Money asset -> Maybe (Money asset) -> Money asset -> Money asset -> (Sum (Money asset), [Money asset])
minimumSplits lowerBound upperBoundM increment amount
  | increment <= 0
      || maybe False (<= 0) upperBoundM
      || lowerBound < 0
      || maybe False (lowerBound >) upperBoundM
      || increment > lowerBound
      || not ((isMultipleOf `on` toRational) lowerBound increment)
      || maybe False (\upperBound -> not ((isMultipleOf `on` toRational) upperBound increment)) upperBoundM =
    (Sum amount, [])
  | amount < lowerBound = (Sum amount, [])
  | Just upperBound <- upperBoundM, amount > upperBound = (upperBound :) <$> minimumSplits lowerBound upperBoundM increment (amount - upperBound)
  | otherwise = do
    let numIncrement = toRational @Integer . floor $ toRational (amount - lowerBound) / toRational increment
        buy = lowerBound + money' numIncrement * increment
        amount' = amount - buy
    (Sum amount', [buy])

askFilledSizeBroadcastChan :: (MonadReader (Env c m') m, MonadIO m) => AssetId -> m (TChan Rational)
askFilledSizeBroadcastChan = askAssetBroadcastChan filledSizeBroadcastChans

askSpentFundsBroadcastChan :: (MonadReader (Env c m') m, MonadIO m) => AssetId -> m (TChan Rational)
askSpentFundsBroadcastChan = askAssetBroadcastChan spentFundsBroadcastChans

askAssetBroadcastChan :: (MonadReader (Env c m') m, Ord k, MonadIO m, Show k) => (State -> Map k (TChan a)) -> k -> m (TChan a)
askAssetBroadcastChan f assetId =
  asks (Map.lookup assetId . f . state) >>= \case
    Nothing -> newBroadcastTChanIO
    Just x -> return x

mapEnvConfig :: (c -> c') -> Env c m -> Env c' m
mapEnvConfig f env@Env {..} =
  Env
    { config = f config,
      logAction = hoistLogAction (\(StrategyT r) -> StrategyT $ withReaderT (const env) r) logAction,
      ..
    }

baseRetryDelay :: Num a => a
baseRetryDelay = 500 * millisecond

hour :: Num a => a
hour = 60 * minute

minute :: Num a => a
minute = 60 * second

second :: Num a => a
second = 1_000 * millisecond

millisecond :: Num a => a
millisecond = 1_000 * microsecond

microsecond :: Num a => a
microsecond = 1
