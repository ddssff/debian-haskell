{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Debian.Except
    (
    -- * Control.Exception addition
      withException
    , displaySomeExceptionType
    -- * Control.Monad.Except addition
    , tryExceptT
    -- * Constraints on the exception type in MonadError
    , HasIOException(fromIOException)
    , HasSomeException(fromSomeException)
    , liftEIO
    , liftSE
    -- * Re-exports from Control.Monad.Except
    , module Control.Monad.Except
    ) where

import Control.Exception (evaluate, Exception, IOException, SomeException(SomeException))
import Control.Monad.Catch (try)
import Control.Monad.Except (ExceptT, lift, liftIO, MonadError, MonadIO, runExceptT, throwError)
import Data.Typeable (typeOf)
import Language.Haskell.TH.Syntax (Loc)

-- | Apply a function to whatever @Exception@ type is inside a
-- @SomeException@.
withException :: forall r. (forall e. Exception e => e -> r) -> SomeException -> r
withException f (SomeException e) = f e

-- | Use 'withException' to obtain the exception's type name.
displaySomeExceptionType :: SomeException -> String
displaySomeExceptionType = withException (show . typeOf)

-- | MonadError analog to the 'try' function.
tryExceptT :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryExceptT = lift . runExceptT

-- | This class includes an instance for IOException itself, so we
-- don't know whether the exception has been caught.
class HasIOException e where fromIOException :: Loc -> IOException -> e
instance HasIOException IOException where fromIOException _loc = id

class HasSomeException e where fromSomeException :: Loc -> SomeException -> e
instance HasSomeException SomeException where fromSomeException _loc = id

-- | Evaluate an IO action, catching any IOException, and lift the
-- result into a MonadError instance.
liftEIO :: forall e m a. (MonadIO m, HasIOException e, MonadError e m) => Loc -> IO a -> m a
liftEIO loc action =
    -- This evaluate ensures that the action is fully evaluated and
    -- any resulting IOExceptions are thrown.
    (liftIO (try (action >>= evaluate)) :: m (Either IOException a)) >>= either handle return
    where handle :: IOException -> m a
          handle = throwError . fromIOException loc

-- | Evaluate an IO action, catching any exception, and lift the
-- result into a MonadError instance.
liftSE :: forall e m a. (MonadIO m, HasSomeException e, MonadError e m) => Loc -> IO a -> m a
liftSE loc action =
    liftIO (try action) >>= either (\(e :: SomeException) -> f e) return
    where f = throwError . fromSomeException loc
