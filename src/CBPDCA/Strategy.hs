module CBPDCA.Strategy
    ( Context(..)
    , Strategy
    , runStrategyT
    , runStrategy
    )
where

import           CoinbasePro.Authenticated.Request.Extended
                                                ( CBAuthT(..)
                                                , CoinbaseProCredentials
                                                , runCbAuthT
                                                , runSandboxCbAuthT
                                                )
import           Control.Concurrent.Forkable    ( ForkableMonad(..) )
import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.Cont             ( MonadIO
                                                , MonadTrans(..)
                                                )
import           Control.Monad.Error.Class      ( MonadError )
import           Control.Monad.Except.Extended  ( ExceptT
                                                , runExceptT
                                                )
import           Control.Monad.RWS.Class        ( MonadReader
                                                , local
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , runReaderT
                                                )
import           Katip.Core                     ( Katip(..)
                                                , LogEnv
                                                , Namespace
                                                )
import           Katip.Monadic                  ( KatipContext(..)
                                                , LogContexts
                                                )
import           Servant.Client.Internal.HttpClient
                                                ( ClientM(..) )

data Context =
    Context
        { msKNamespace :: Namespace
        , msKContext :: LogContexts
        , msLogEnv :: LogEnv
        }

type Error = String

newtype StrategyT m a =
    StrategyT
        { unStrategyT :: ReaderT Context (ExceptT Error m) a
        }
    deriving ( MonadReader Context
             , Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError Error
             , ForkableMonad
             , MonadThrow
             , MonadCatch
             , MonadMask
             )

type Strategy = StrategyT (CBAuthT ClientM)

instance (MonadIO m) => Katip (StrategyT m) where
    getLogEnv = asks msLogEnv
    localLogEnv f (StrategyT m) =
        StrategyT (local (\s -> s { msLogEnv = f (msLogEnv s) }) m)

instance (MonadIO m) => KatipContext (StrategyT m) where
    getKatipContext = asks msKContext
    localKatipContext f (StrategyT m) =
        StrategyT (local (\s -> s { msKContext = f (msKContext s) }) m)
    getKatipNamespace = asks msKNamespace
    localKatipNamespace f (StrategyT m) =
        StrategyT (local (\s -> s { msKNamespace = f (msKNamespace s) }) m)

instance MonadTrans StrategyT where
    lift = StrategyT . lift . lift

instance ForkableMonad m => ForkableMonad (ExceptT e m) where
    forkIO m = lift $ forkIO (runExceptT m)

deriving instance ForkableMonad m => ForkableMonad (CBAuthT m)

deriving instance MonadError e m => MonadError e (CBAuthT m)

deriving instance MonadThrow m => MonadThrow (CBAuthT m)

deriving instance MonadCatch m => MonadCatch (CBAuthT m)

deriving instance MonadMask m => MonadMask (CBAuthT m)

deriving instance ForkableMonad ClientM

deriving instance MonadMask ClientM

runStrategyT :: Context -> StrategyT m a -> m (Either Error a)
runStrategyT logState = runExceptT . flip runReaderT logState . unStrategyT

runStrategy
    :: Bool
    -> CoinbaseProCredentials
    -> Context
    -> Strategy a
    -> IO (Either Error a)
runStrategy sandbox credentials logState = run credentials
    . runStrategyT logState
    where run = if sandbox then runSandboxCbAuthT else runCbAuthT
