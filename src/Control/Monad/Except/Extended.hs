module Control.Monad.Except.Extended
    ( module Control.Monad.Except
    , throwIfNothing
    ) where

import           Control.Monad.Except

throwIfNothing :: (MonadError e m, Applicative m) => Maybe a -> e -> m a
throwIfNothing m err = maybe (throwError err) pure m
