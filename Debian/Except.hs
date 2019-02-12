{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}

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
    -- * A class for mainaging a call stack with the Reader monad
    , HasStack(stackLens)
    , pushStack
    -- * Re-exports from Control.Monad.Except
    , module Control.Monad.Except
    ) where

import Control.Exception (evaluate, Exception, IOException, SomeException(SomeException))
import Control.Lens (Lens', over)
import Control.Monad.Catch (try)
import Control.Monad.Except (ExceptT, lift, liftIO, MonadError, MonadIO, runExceptT, throwError)
import Control.Monad.Reader (local, MonadReader)
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

-- | A value that can be specified in a @MonadReader r m@ to maintain a
-- call stack of @loc@.  Examples of type @loc@ would be @Loc@ or @(Loc,
-- String)@
class HasStack loc r where stackLens :: Lens' r [loc]
instance HasStack loc [loc] where stackLens = id

pushStack :: forall loc r m a. (HasStack loc r, MonadReader r m) => loc -> m a -> m a
pushStack loc action = local (over stackLens (loc :) :: r -> r) action

-- | This class includes an instance for IOException itself, so we
-- don't know whether the exception has been caught.
class HasIOException e where fromIOException :: [Loc] -> IOException -> e
-- This instance unfortunately throws away the stack info.
instance HasIOException IOException where fromIOException _locs = id

class HasSomeException e where fromSomeException :: [Loc] -> SomeException -> e
instance HasSomeException SomeException where fromSomeException _locs = id

-- | Evaluate an IO action, catching any IOException, and lift the
-- result into a MonadError instance.
liftEIO :: forall e m a. (MonadIO m, HasIOException e, MonadError e m) => [Loc] -> IO a -> m a
liftEIO locs action =
    -- This evaluate ensures that the action is fully evaluated and
    -- any resulting IOExceptions are thrown.
    (liftIO (try (action >>= evaluate)) :: m (Either IOException a)) >>= either handle return
    where handle :: IOException -> m a
          handle = throwError . fromIOException locs

-- | Evaluate an IO action, catching any exception, and lift the
-- result into a MonadError instance.
liftSE :: forall e m a. (MonadIO m, HasSomeException e, MonadError e m) => [Loc] -> IO a -> m a
liftSE locs action =
    liftIO (try action) >>= either (\(e :: SomeException) -> f e) return
    where f = throwError . fromSomeException locs
