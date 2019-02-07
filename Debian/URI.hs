{-# LANGUAGE CPP, OverloadedStrings, PackageImports, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Debian.URI
    ( module Network.URI
    , URIError(..)
    , uriSchemeLens
    , uriAuthorityLens
    , uriPathLens
    , uriQueryLens
    , uriFragmentLens
    -- * String known to parsable by parseURIReference.  Mainly
    -- useful because it has a Read instance.
    , URI'
    , toURI'
    , fromURI'
    , readURI'

    -- Show URI as a Haskell expression
    , showURI
    -- Monadic URI parsers
    , parseURIReference'
    , parseURI'
    , parseAbsoluteURI'
    , parseRelativeReference'
    -- URI appending
    , appendURI
    , appendURIs
    , parentURI
    , uriToString'
    , fileFromURI
    , fileFromURIStrict
    , dirFromURI
    -- * Lift IO operations into a MonadError instance
    , HasIOException(fromIOException)
    , HasParseError(fromParseError)
    , HasURIError(fromURIError)
    , liftEIO
    , MonadIO, MonadError, IOException
    , run, run'
    -- * URI, IO, or Parse Error
    , DebError(..)
    -- * QuickCheck properties
    , prop_print_parse
    , prop_append_singleton
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (catch, Exception, IOException, throw, try)
import Control.Lens (makeLensesFor, view)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.ByteString.Lazy.UTF8 as L hiding (fromString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Foldable (foldrM)
import Data.Maybe (catMaybes, fromJust)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text as T (isInfixOf, pack, Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Debian.Sources (VendorURI, vendorURI)
import Debian.TH (here)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (Loc)
import Network.URI (nullURI, parseURIReference, parseURI, parseAbsoluteURI, parseRelativeReference, URI(..), URIAuth(..), uriToString)
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath ((</>), dropTrailingPathSeparator, takeDirectory)
import System.Process (CreateProcess, proc)
import System.Process.Common (showCreateProcessForUser)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)
import Test.QuickCheck (Arbitrary)
import Text.Parsec (ParseError)
import Text.Regex (mkRegex, matchRegex)

$(makeLensesFor [("uriScheme", "uriSchemeLens"),
                 ("uriAuthority", "uriAuthorityLens"),
                 ("uriPath", "uriPathLens"),
                 ("uriQuery", "uriQueryLens"),
                 ("uriFragment", "uriFragmentLens")] ''URI)

showURI :: URI -> String
showURI (URI {..}) =
    "URI {uriScheme = " <> show uriScheme <>
       ", uriAuthority = " <> show uriAuthority <>
       ", uriPath = " <> show uriPath <>
       ", uriQuery = " <> show uriQuery <>
       ", uriFragment = " <> show uriFragment <> "}"

-- | parseURI with MonadError
parseURI' :: (HasURIError e, MonadError e m) => String -> m URI
parseURI' s = maybe (throwError $ fromURIError $ URIParseError "parseURI" s) return (parseURI s)
parseURIReference' :: (HasURIError e, MonadError e m) => String -> m URI
parseURIReference' s = maybe (throwError $ fromURIError $ URIParseError "parseURIReference" s) return (parseURIReference s)
parseAbsoluteURI' :: (HasURIError e, MonadError e m) => String -> m URI
parseAbsoluteURI' s = maybe (throwError $ fromURIError $ URIParseError "parseAbsoluteURI" s) return (parseAbsoluteURI s)
parseRelativeReference' :: (HasURIError e, MonadError e m) => String -> m URI
parseRelativeReference' s = maybe (throwError $ fromURIError $ URIParseError "parseRelativeReference" s) return (parseRelativeReference s)

--parseAbsoluteURI :: String -> Maybe URI
--parseRelativeReference :: String -> Maybe URI
--parseURI :: String -> Maybe URI
--parseURIReference :: String -> Maybe URI

data URIError =
    URIParseError String String
  | URIAppendError URI URI
  deriving (Eq, Ord, Show)

-- | Conservative appending of absolute and relative URIs.  There may
-- be other cases that can be implemented, lets see if they turn up.
appendURI :: MonadError URIError m => URI -> URI -> m URI
    -- Append the two paths
appendURI (URI scheme auth path1 "" "") (URI "" Nothing path2 query fragment) = return $ URI scheme auth (path1 </> path2) query fragment
    -- Use query from RHS
appendURI a b = throwError (URIAppendError a b)

-- | Append a list of URI
-- @@
-- λ> appendURIs (parseURI "http://host.com") (parseURIRelative "/bar")
appendURIs :: (Foldable t, MonadError URIError m) => t URI -> m URI
appendURIs uris = foldrM appendURI nullURI uris

parentURI :: URI -> URI
parentURI uri = uri {uriPath = takeDirectory (dropTrailingPathSeparator (uriPath uri))}

-- properties
-- appendURIs [x] == x

prop_append_singleton :: URI -> Bool
prop_append_singleton uri = appendURIs [uri] == Right uri

prop_print_parse :: URI -> Bool
prop_print_parse uri = parseURIReference (show uri) == Just uri

-- instance Arbitrary

-- | A wrapper around a String containing a known parsable URI.  Not
-- absolutely safe, because you could say read "URI' \"bogus string\""
-- :: URI'.  But enough to save me from myself.
newtype URI' = URI' String deriving (Read, Show, Eq, Ord)

readURI' :: String -> Maybe URI'
readURI' s = maybe Nothing (const (Just (URI' s))) (parseURIReference s)

fromURI' :: URI' -> URI
fromURI' (URI' s) = fromJust (parseURI s) -- this should provably parse

-- | Using the bogus Show instance of URI here.  If it ever gets fixed
-- this will stop working.  Worth noting that show will obscure any
-- password info embedded in the URI, so that's nice.
toURI' :: VendorURI -> URI'
toURI' = URI' . show . view vendorURI

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

instance Arbitrary URI where

fileFromURI :: (MonadIO m, HasIOException e, MonadError e m) => Loc -> URI -> m L.ByteString
fileFromURI loc uri = fileFromURIStrict loc uri

fileFromURIStrict :: (MonadIO m, HasIOException e, MonadError e m) => Loc -> URI -> m L.ByteString
fileFromURIStrict loc uri =
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> liftEIO $ L.readFile (uriPath uri)
      -- ("ssh:", Just auth) -> cmdOutputStrict ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " cat " ++ show (uriPath uri))
      ("ssh:", Just auth) ->
          run' loc (proc "ssh" [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "cat", uriPath uri])
      _ ->
          run' loc (proc "curl" ["-s", "-g", uriToString' uri])

-- | Parse the text returned when a directory is listed by a web
-- server.  This is currently only known to work with Apache.
-- NOTE: there is a second copy of this function in
-- Extra:Extra.Net. Please update both locations if you make changes.
webServerDirectoryContents :: Text -> IO [String]
webServerDirectoryContents text | isInfixOf "<title>404 Not Found</title>" text = fail "Bad URL"
webServerDirectoryContents text =
    return . catMaybes . map (second . matchRegex re) . Prelude.lines . T.unpack $ text
    where
      re = mkRegex "( <A HREF|<a href)=\"([^/][^\"]*)/\""
      second (Just [_, b]) = Just b
      second _ = Nothing


dirFromURI :: Loc -> URI -> IO (Either IOException [String])
dirFromURI loc uri = try $ do
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> getDirectoryContents (uriPath uri)
      ("ssh:", Just auth) ->
          (Prelude.lines . L.toString) <$>
            run' loc (proc "ssh" [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "ls", "-1", uriPath uri])
      _ ->
          (webServerDirectoryContents =<< (T.pack . L.toString) <$> run' loc (proc "curl" ["-s", "-g", uriToString' uri]))
            `catch` (\(e :: IOException) -> throw (userError (show e ++ ": " ++ show uri)))

class HasIOException e where fromIOException :: IOException -> e
instance HasIOException IOException where fromIOException = id

class HasParseError e where fromParseError :: ParseError -> e
instance HasParseError ParseError where fromParseError = id

class HasURIError e where fromURIError :: URIError -> e
instance HasURIError URIError where fromURIError = id

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

data DebError
    = IOException IOException
    | URIError URIError
    | ParseError ParseError
    deriving (Show, Eq, Ord)

instance Exception DebError

instance Ord IOException where
    compare a b = compare (show a) (show b)

instance Ord ParseError where
    compare a b = compare (show a) (show b)

instance HasIOException DebError where fromIOException = IOException
instance HasParseError DebError where fromParseError = ParseError
instance HasURIError DebError where fromURIError = URIError
