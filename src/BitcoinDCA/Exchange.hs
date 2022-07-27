{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module BitcoinDCA.Exchange
  ( MonadExchange (..),
    RunnableExchange (..),
  )
where

import BitcoinDCA.Types
import Control.Monad.Trans (MonadTrans (lift))
import Data.Functor.Const
import GHC.TypeLits (KnownSymbol)

-- | A computation that connects to exchanges
class Monad m => MonadExchange m where
  type ExchangeError m = e | e -> m
  type ExchangeConfig m = c | c -> m
  getAssetPair :: (KnownSymbol base, KnownSymbol quote) => m (Maybe (AssetPair base quote))
  placeMarketBuyOrder :: (KnownSymbol base, KnownSymbol quote) => Money quote -> m (OrderId, Maybe (Order base))
  getOrder :: KnownSymbol asset => OrderId -> m (Maybe (Order asset))
  getAsset :: KnownSymbol asset => m (Maybe (Asset asset))
  withdraw :: KnownSymbol asset => Address -> Money asset -> m WithdrawId
  getWithdraw :: WithdrawId -> m (Maybe Withdraw)

class MonadExchange m => RunnableExchange n m where
  runExchange :: ExchangeConfig m -> m a -> n (Either (ExchangeError m) a)

instance (MonadExchange m, Monad (t m), MonadTrans t) => MonadExchange (t m) where
  type ExchangeError (t m) = Const (ExchangeError m) t
  type ExchangeConfig (t m) = Const (ExchangeConfig m) t
  getAssetPair = lift getAssetPair
  placeMarketBuyOrder = lift . placeMarketBuyOrder
  getOrder = lift . getOrder
  getAsset = lift getAsset
  withdraw address = lift . withdraw address
  getWithdraw = lift . getWithdraw
