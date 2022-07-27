{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BitcoinDCA.Exchange.Coinbase where

import BitcoinDCA.Common
import BitcoinDCA.Exchange
import qualified BitcoinDCA.Exchange.Coinbase.API as API
import BitcoinDCA.Types
import Control.Arrow ((>>>))
import Control.Concurrent.Forkable (ForkableMonad (forkIO))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.Trans (MonadTrans (lift))
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.UUID (fromASCIIBytes, toASCIIBytes)
import GHC.TypeLits (KnownSymbol)
import Servant.Client (ClientError, ClientM)
import Servant.Client.Internal.HttpClient.Extended ()
import UnliftIO (MonadUnliftIO)

newtype Coinbase a = Coinbase {unCoinbase :: ReaderT API.Config ClientM a}
  deriving newtype (Functor, Applicative, Monad, MonadError ClientError, MonadIO, MonadUnliftIO)

fromCurrencyIdPair :: AssetId -> AssetId -> API.ProductId
fromCurrencyIdPair base quote =
  API.ProductId $ BS.concat [coerce base, "-", coerce quote]

fromAPIOrderStatus :: API.OrderStatus -> OrderStatus
fromAPIOrderStatus = \case
  API.Pending -> OrderPending
  API.Done -> OrderComplete
  _ -> OrderFailed

instance MonadExchange Coinbase where
  type ExchangeError Coinbase = ClientError

  type ExchangeConfig Coinbase = API.Config

  getAssetPair :: forall base quote. (KnownSymbol base, KnownSymbol quote) => Coinbase (Maybe (AssetPair base quote))
  getAssetPair = Coinbase . lift . expectNotFound $
    do
      API.Product {..} <- API.getProduct $ fromCurrencyIdPair (assetId @base) (assetId @quote)
      return
        AssetPair
          { base = coerce baseCurrency,
            quote = coerce quoteCurrency,
            minMarketFunds = 0,
            maxMarketFunds = Nothing,
            quoteIncrement = coerce quoteIncrement
          }

  placeMarketBuyOrder :: forall base quote. (KnownSymbol base, KnownSymbol quote) => Money quote -> Coinbase (OrderId, Maybe (Order base))
  placeMarketBuyOrder amount = Coinbase . lift $
    do
      API.FundsMarketOrder {..} <-
        API.placeFundsMarketOrder
          API.FundsMarketOrderRequest
            { side = API.Buy,
              productId = fromCurrencyIdPair (assetId @base) (assetId @quote),
              funds = API.Funds . coerce $ amount
            }
      let orderId = coerce . toASCIIBytes . coerce $ id
      return
        ( orderId,
          Just Order {id = orderId, status = fromAPIOrderStatus status, filledSize = coerce filledSize}
        )

  getOrder (OrderId orderId)
    | Just uuid <- fromASCIIBytes orderId = Coinbase . lift $
      do
        API.Order {..} <- API.getOrder . API.ClientOrderId $ uuid
        let receivedOrderId = coerce . toASCIIBytes . coerce $ id
        return . Just $
          Order
            { id = receivedOrderId,
              status = fromAPIOrderStatus status,
              filledSize = coerce filledSize
            }
    | otherwise = Coinbase . lift . return $ Nothing

  getAsset :: forall asset. KnownSymbol asset => Coinbase (Maybe (Asset asset))
  getAsset = Coinbase . lift . expectNotFound $
    do
      let AssetId c = assetId @asset
      API.CurrencyResponse {details = API.CurrencyResponseDetails {..}, ..} <- API.getCurrency . API.CurrencyId $ c
      return
        Asset
          { id = coerce id,
            maxPrecision = coerce maxPrecision,
            minWithdrawalAmount = fromMaybe 0 $ coerce minWithdrawalAmount,
            maxWithdrawalAmount = coerce maxWithdrawalAmount
          }

  withdraw :: forall asset. KnownSymbol asset => Address -> Money asset -> Coinbase WithdrawId
  withdraw address funds = Coinbase . lift $ do
    API.WithdrawCryptoResponse {..} <-
      API.withdrawCrypto $
        API.WithdrawCryptoRequest
          { currency = coerce $ assetId @asset,
            cryptoAddress = coerce address,
            amount = coerce funds
          }
    return . coerce . toASCIIBytes . coerce $ id

  getWithdraw (WithdrawId withdrawId)
    | Just uuid <- fromASCIIBytes withdrawId = Coinbase . lift . expectNotFound $ do
      API.Withdraw {..} <- API.getTransfer . API.WithdrawId $ uuid
      let receivedWithdrawId = coerce . toASCIIBytes . coerce $ id
      return
        Withdraw
          { id = receivedWithdrawId,
            status = case (completedAt, cancelledAt) of
              (Just _, _) -> WithdrawComplete
              (_, Just _) -> WithdrawFailed
              (_, _) -> WithdrawPending
          }
    | otherwise = Coinbase . lift . return $ Nothing

instance MonadIO m => RunnableExchange m Coinbase where
  runExchange config =
    unCoinbase
      >>> flip runReaderT config
      >>> API.authenticateAndRunClientM config
      >>> liftIO

instance ForkableMonad Coinbase where
  forkIO m = do
    config <- Coinbase ask
    liftIO . forkIO . void . runExchange config $ m
