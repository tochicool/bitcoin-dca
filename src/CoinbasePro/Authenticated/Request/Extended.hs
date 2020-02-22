module CoinbasePro.Authenticated.Request.Extended
    ( module CoinbasePro.Authenticated.Request
    , runSandboxCbAuthT
    )
where

import           CoinbasePro.Authenticated.Request
import           CoinbasePro.Request            ( runSandbox )
import           Control.Monad.Reader           ( runReaderT )
import           Servant.Client                 ( ClientM )

runSandboxCbAuthT :: CoinbaseProCredentials -> CBAuthT ClientM a -> IO a
runSandboxCbAuthT cpc = runSandbox . flip runReaderT cpc . unCbAuth
