{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BitcoinDCA.Explorer.Esplora.API.Types where

import BitcoinDCA.Common
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), ToJSON (toJSON), camelTo2, defaultOptions, genericParseJSON)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text.Encoding (decodeUtf8)
import Servant.API (ToHttpApiData (toUrlPiece))
import Servant.API.Generic (Generic)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Https))

newtype Config = Config
  { baseUrl :: BaseUrl
  }
  deriving (Generic, Show, ToJSON)

instance FromJSON Config where
  parseJSON =
    genericParseJSON jsonConfigOptions
      . ("baseUrl" ?:. toJSON (BaseUrl Https "blockstream.info" 443 "api"))

newtype Address = Address ByteString

instance ToHttpApiData Address where
  toUrlPiece = decodeUtf8 . coerce

data GetAddressResponse = GetAddressResponse
  { chainStats :: ChainStats,
    mempoolStats :: ChainStats
  }
  deriving (Generic)

instance FromJSON GetAddressResponse where
  parseJSON = genericParseJSON jsonApiOptions

newtype ChainStats = ChainStats
  { txCount :: Integer
  }
  deriving (Generic)

instance FromJSON ChainStats where
  parseJSON = genericParseJSON jsonApiOptions

jsonApiOptions :: Options
jsonApiOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    }
