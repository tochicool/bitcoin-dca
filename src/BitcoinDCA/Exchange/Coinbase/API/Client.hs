{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module BitcoinDCA.Exchange.Coinbase.API.Client where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import BitcoinDCA.Exchange.Coinbase.API.Types
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.ByteString.Base64
  ( decodeBase64,
    encodeBase64',
  )
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Either
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Typeable (Proxy (..))
import Network.HTTP.Client
  ( ManagerSettings (managerModifyRequest),
    Request,
    RequestBody (RequestBodyBS, RequestBodyLBS),
  )
import Network.HTTP.Client.Internal (Request (..))
import Network.HTTP.Client.TLS
  ( newTlsManagerWith,
    tlsManagerSettings,
  )
import Servant.API
  ( Capture,
    Get,
    JSON,
    Post,
    ReqBody,
    (:<|>) (..),
    (:>),
  )
import Servant.Client
  ( ClientError (),
    ClientM,
    client,
    mkClientEnv,
    runClientM,
  )


type CoinbaseAPI =
  "orders"
    :> (ReqBody '[JSON] FundsMarketOrderRequest :> Post '[JSON] FundsMarketOrder)
    :<|> "orders"
      :> Capture "client_oid" ClientOrderId
      :> Get '[JSON] Order
    :<|> "products"
      :> Capture "product_id" ProductId
      :> Get '[JSON] Product
    :<|> "currencies"
      :> Capture "currency_id" CurrencyId
      :> Get '[JSON] CurrencyResponse
    :<|> "withdrawals" :> "crypto"
      :> (ReqBody '[JSON] WithdrawCryptoRequest :> Post '[JSON] WithdrawCryptoResponse)
    :<|> "transfers"
      :> Capture "transfer_id" WithdrawId
      :> Get '[JSON] Transfer

getProduct :: ProductId -> ClientM Product
placeFundsMarketOrder ::
  FundsMarketOrderRequest -> ClientM FundsMarketOrder
getOrder :: ClientOrderId -> ClientM Order
getCurrency :: CurrencyId -> ClientM CurrencyResponse
withdrawCrypto :: WithdrawCryptoRequest -> ClientM WithdrawCryptoResponse
getTransfer :: WithdrawId -> ClientM Transfer
placeFundsMarketOrder :<|> getOrder :<|> getProduct :<|> getCurrency :<|> withdrawCrypto :<|> getTransfer = client api

api :: Proxy CoinbaseAPI
api = Proxy

authenticateAndRunClientM :: Config -> ClientM a -> IO (Either ClientError a)
authenticateAndRunClientM config@Config {baseUrl} m = do
  manager <-
    newTlsManagerWith
      tlsManagerSettings
        { managerModifyRequest = authRequest config
        }
  runClientM m (mkClientEnv manager baseUrl)

authRequest :: Config -> Request -> IO Request
authRequest Config {..} request@Request {..}
  | Just _ <- lookup "CB-ACCESS-KEY" requestHeaders = return request
  | otherwise = do
    body <- case requestBody of
      RequestBodyLBS body -> pure $ BSL.toStrict body
      RequestBodyBS body -> pure body
      _ -> error "cannot authenticate request body"
    timestamp <- C.pack . show @Integer . round <$> getPOSIXTime
    let decodedSecretKey = fromRight "" $ decodeBase64 . coerce $ secretKey
        preimage = BS.concat [timestamp, method, path, queryString, body]
        image = hmac decodedSecretKey preimage :: HMAC SHA256
        signature = encodeBase64' . convert $ image
    return $
      request
        { requestHeaders =
            ("USER-AGENT", "bitcoin-dca/0.2.0.0") :
            ("CB-ACCESS-KEY", coerce accessKey) :
            ("CB-ACCESS-SIGN", signature) :
            ("CB-ACCESS-TIMESTAMP", timestamp) :
            ("CB-ACCESS-PASSPHRASE", coerce passphrase) :
            requestHeaders
        }
