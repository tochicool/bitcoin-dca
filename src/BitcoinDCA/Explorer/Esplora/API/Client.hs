{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module BitcoinDCA.Explorer.Esplora.API.Client where

import BitcoinDCA.Explorer.Esplora.API.Types
import Data.Data (Proxy (Proxy))
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Servant.API (Capture, Get, JSON, type (:>))
import Servant.Client (ClientError, ClientM, client, mkClientEnv, runClientM)

type EsploraAPI =
  "address"
    :> Capture "address" Address
    :> Get '[JSON] GetAddressResponse

getAddress :: Address -> ClientM GetAddressResponse
getAddress = client api

api :: Proxy EsploraAPI
api = Proxy

authenticateAndRunClientM :: Config -> ClientM a -> IO (Either ClientError a)
authenticateAndRunClientM Config {baseUrl} m = do
  manager <-
    newTlsManagerWith
      tlsManagerSettings
  runClientM m (mkClientEnv manager baseUrl)
