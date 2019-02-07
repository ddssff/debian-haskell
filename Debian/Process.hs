{-# LANGUAGE CPP, OverloadedStrings, PackageImports, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Debian.Process
    (
    -- * Lift IO operations into a MonadError instance
      HasIOException(fromIOException)
    , HasWrappedIOException(wrapIOException)
    , liftEIO
    , MonadIO, MonadError, IOException
    , run, run'
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (IOException, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.ByteString.Lazy.UTF8 as L hiding (fromString)
import qualified Data.ByteString.Lazy.Char8 as L
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)
import Debian.TH (here)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (Loc)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (CreateProcess)
import System.Process.Common (showCreateProcessForUser)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)

class HasIOException e where fromIOException :: IOException -> e
instance HasIOException IOException where fromIOException = id

-- | This class is like HasIOException but there is no instance for
-- IOException.  This is to avoid the IO monad's functional dependency
-- on IOException.  Thus we know we caught the bare IOException and
-- wrapped it up.  (This doesn't work.)
class HasWrappedIOException e where wrapIOException :: IOException -> e

-- | Lift an IO operation into ExceptT FileError IO
liftEIO :: forall e m a. (MonadIO m, HasIOException e, MonadError e m) => IO a -> m a
liftEIO action =
    liftIO (try action) >>= either (\(e :: IOException) -> f e) return
    where f = throwError . fromIOException

run :: ExpQ
run = [|run' $here|]

run' :: (MonadIO m, HasIOException e, MonadError e m) => Loc -> CreateProcess -> m L.ByteString
run' loc cp = do
  (code, out, err) <- liftEIO $ readCreateProcessWithExitCode cp L.empty
  case code of
    ExitSuccess -> return out
    ExitFailure _ -> throwError $ fromIOException $ userError $ unlines $
                                       [ show code
                                       , " command: " ++ showCreateProcessForUser cp
                                       , " stderr: " ++ unpack (decodeUtf8 (L.toStrict err))
                                       , " stdout: " ++ unpack (decodeUtf8 (L.toStrict out))
                                       , " location: " ++ show loc ]

instance Ord IOException where
    compare a b = compare (show a) (show b)
