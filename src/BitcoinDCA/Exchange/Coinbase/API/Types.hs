{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module BitcoinDCA.Exchange.Coinbase.API.Types where

import BitcoinDCA.Common
import Data.Aeson (FromJSON (..), Options (..), SumEncoding (tagFieldName), ToJSON (..), Value (Object), camelTo2, defaultOptions, defaultTaggedObject, genericParseJSON, genericToJSON)
import qualified Data.Aeson as Data.Aeson.Types.Generic
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified GHC.Generics
import Money
  ( Dense,
  )
import Servant.API
  ( ToHttpApiData (toUrlPiece),
  )
import Servant.API.Generic (Generic)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Https))

data Config = Config
  { baseUrl :: BaseUrl,
    accessKey :: AccessKey,
    secretKey :: SecretKey,
    passphrase :: AccessPassphrase
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON Config where
  parseJSON =
    genericParseJSON jsonConfigOptions
      . ("baseUrl" ?:. toJSON (BaseUrl Https "api.exchange.coinbase.com" 443 ""))

newtype SecretKey = SecretKey ByteString
  deriving newtype (IsString, ByteArrayAccess)
  deriving Show via Secret

instance FromJSON SecretKey where
  parseJSON = byteStringParseJSON

instance ToJSON SecretKey where
  toJSON = byteStringToJSON

newtype AccessKey = AccessKey ByteString
  deriving newtype (IsString)
  deriving (Show) via Secret

instance FromJSON AccessKey where
  parseJSON = byteStringParseJSON

instance ToJSON AccessKey where
  toJSON = byteStringToJSON

newtype AccessPassphrase = AccessPassphrase ByteString
  deriving newtype (IsString)
  deriving (Show) via Secret

instance FromJSON AccessPassphrase where
  parseJSON = byteStringParseJSON

instance ToJSON AccessPassphrase where
  toJSON = byteStringToJSON

data LimitOrderRequest = LimitOrderRequest {productId :: ProductId, side :: Side, price :: Price}
  deriving (Show, Generic)

instance ToJSON LimitOrderRequest where
  toJSON = genericToJSON jsonApiOptions

data LimitOrder = LimitOrder
  { id :: UUID,
    productId :: ProductId,
    side :: Side,
    price :: Price
  }
  deriving (Show, Generic)

instance FromJSON LimitOrder where
  parseJSON = genericParseJSON jsonApiOptions

data SizeMarketOrderRequest = SizeMarketOrderRequest {side :: Side, productId :: ProductId, size :: Size}
  deriving (Show, Generic)

instance ToJSON SizeMarketOrderRequest where
  toJSON = marketOrderToJSON

data SizeMarketOrder = SizeMarketOrder
  { id :: UUID,
    side :: Side,
    productId :: ProductId,
    size :: Size
  }
  deriving (Show, Generic)

instance FromJSON SizeMarketOrder where
  parseJSON = genericParseJSON jsonApiOptions

data FundsMarketOrderRequest = FundsMarketOrderRequest
  { side :: Side,
    productId :: ProductId,
    funds :: Funds
  }
  deriving (Show, Generic)

instance ToJSON FundsMarketOrderRequest where
  toJSON = marketOrderToJSON

data FundsMarketOrder = FundsMarketOrder
  { id :: ClientOrderId,
    side :: Side,
    productId :: ProductId,
    funds :: Funds,
    status :: OrderStatus,
    filledSize :: Size
  }
  deriving (Show, Generic)

instance FromJSON FundsMarketOrder where
  parseJSON = genericParseJSON jsonApiOptions

data Product = Product
  { id :: ProductId,
    displayName :: Text,
    baseCurrency :: CurrencyId,
    quoteCurrency :: CurrencyId,
    quoteIncrement :: Funds
  }
  deriving (Show, Generic)

instance FromJSON Product where
  parseJSON = genericParseJSON jsonApiOptions

data Order = Order
  { id :: ClientOrderId,
    status :: OrderStatus,
    filledSize :: Size
  }
  deriving (Show, Generic)

instance FromJSON Order where
  parseJSON = genericParseJSON jsonApiOptions

data OrderStatus
  = Open
  | Pending
  | Active
  | Done
  deriving (Show, Generic)

instance FromJSON OrderStatus where
  parseJSON = genericParseJSON jsonApiOptions

data CurrencyResponse = CurrencyResponse
  { id :: CurrencyId,
    maxPrecision :: Size,
    details :: CurrencyResponseDetails
  }
  deriving (Show, Generic)

instance FromJSON CurrencyResponse where
  parseJSON = genericParseJSON jsonApiOptions

data CurrencyResponseDetails = CurrencyResponseDetails
  { minWithdrawalAmount,
    maxWithdrawalAmount ::
      Maybe Size
  }
  deriving (Show, Generic)

instance FromJSON CurrencyResponseDetails where
  parseJSON = genericParseJSON jsonApiOptions

data WithdrawCryptoRequest = WithdrawCryptoRequest
  { amount :: Funds,
    currency :: CurrencyId,
    cryptoAddress :: Address
  }
  deriving (Show, Generic)

instance ToJSON WithdrawCryptoRequest where
  toJSON = genericToJSON jsonApiOptions

data WithdrawCryptoResponse = WithdrawCryptoResponse
  { id :: WithdrawId,
    fee :: Funds
  }
  deriving (Show, Generic)

instance FromJSON WithdrawCryptoResponse where
  parseJSON = genericParseJSON jsonApiOptions

data Transfer = Withdraw
  { id :: WithdrawId,
    completedAt :: Maybe UTCTime,
    cancelledAt :: Maybe UTCTime
  }
  deriving (Show, Generic)

instance FromJSON Transfer where
  parseJSON = genericParseJSON jsonApiOptions

marketOrderToJSON :: (Generic a, Data.Aeson.Types.Generic.GToJSON' Value Data.Aeson.Types.Generic.Zero (GHC.Generics.Rep a)) => a -> Value
marketOrderToJSON x = Object $ HM.insert "type" "market" o
  where
    Object o = genericToJSON jsonApiOptions x

jsonApiOptions :: Options
jsonApiOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_',
      constructorTagModifier = camelTo2 '_',
      omitNothingFields = True,
      sumEncoding = defaultTaggedObject {tagFieldName = "type"},
      unwrapUnaryRecords = True
    }

data Side
  = Buy
  | Sell
  deriving (Show, Generic)

instance FromJSON Side where
  parseJSON = genericParseJSON jsonApiOptions

instance ToJSON Side where
  toJSON = genericToJSON jsonApiOptions

newtype Size = Size (Dense "Base")
  deriving stock (Show, Generic)

instance FromJSON Size where
  parseJSON = decimalParseJSON

instance ToJSON Size where
  toJSON = decimalToJSON

newtype Funds = Funds (Dense "Quote")
  deriving stock (Show, Generic)
  deriving newtype (Num, Fractional)

instance FromJSON Funds where
  parseJSON = decimalParseJSON

instance ToJSON Funds where
  toJSON = decimalToJSON

newtype Price = Price (Dense "Quote")
  deriving stock (Show, Generic)

instance FromJSON Price where
  parseJSON = decimalParseJSON

instance ToJSON Price where
  toJSON = decimalToJSON

newtype ClientOrderId = ClientOrderId UUID
  deriving stock (Show, Generic)
  deriving newtype (FromJSON, ToHttpApiData)

newtype ProductId = ProductId ByteString
  deriving (Show, Generic)

instance ToJSON ProductId where
  toJSON = byteStringToJSON

instance FromJSON ProductId where
  parseJSON = byteStringParseJSON

instance ToHttpApiData ProductId where
  toUrlPiece = decodeUtf8 . coerce

newtype CurrencyId = CurrencyId ByteString
  deriving (Show, Generic)

instance ToHttpApiData CurrencyId where
  toUrlPiece = decodeUtf8 . coerce

instance ToJSON CurrencyId where
  toJSON = byteStringToJSON

instance FromJSON CurrencyId where
  parseJSON = byteStringParseJSON

newtype WithdrawId = WithdrawId UUID
  deriving stock (Show, Generic)
  deriving newtype (FromJSON, ToHttpApiData)

newtype Address = Address ByteString
  deriving stock (Show, Generic)

instance ToJSON Address where
  toJSON = byteStringToJSON
