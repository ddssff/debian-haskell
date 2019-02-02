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
    -- * String known to parsable by parseURIReference
    , URI'
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
    , toURI'
    , fromURI'
    , readURI'
    , uriToString'
    , fileFromURI
    , fileFromURIStrict
    , dirFromURI
    -- * QuickCheck properties
    , prop_print_parse
    , prop_append_singleton
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (catch, IOException, throw, try)
import Control.Lens (makeLensesFor)
import Control.Monad.Except (MonadError, throwError)
import Data.ByteString.Lazy.UTF8 as L hiding (fromString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Foldable (foldrM)
import Data.Maybe (catMaybes, fromJust)
import Data.Text as T (isInfixOf, pack, Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.URI (nullURI, parseURIReference, parseURI, parseAbsoluteURI, parseRelativeReference, URI(..), URIAuth(..), uriToString)
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
-- import System.Process.ByteString (readProcessWithExitCode)
import System.Process (CreateProcess, proc)
import System.Process.Common (showCreateProcessForUser)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)
import Test.QuickCheck (Arbitrary)
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
parseURI' :: MonadError URIError m => String -> m URI
parseURI' s = maybe (throwError (URIParseError "parseURI" s)) return (parseURI s)
parseURIReference' :: MonadError URIError m => String -> m URI
parseURIReference' s = maybe (throwError (URIParseError "parseURIReference" s)) return (parseURIReference s)
parseAbsoluteURI' :: MonadError URIError m => String -> m URI
parseAbsoluteURI' s = maybe (throwError (URIParseError "parseAbsoluteURI" s)) return (parseAbsoluteURI s)
parseRelativeReference' :: MonadError URIError m => String -> m URI
parseRelativeReference' s = maybe (throwError (URIParseError "parseRelativeReference" s)) return (parseRelativeReference s)

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
fromURI' (URI' s) = fromJust (parseURI s)

-- | Using the bogus Show instance of URI here.  If it ever gets fixed
-- this will stop working.  Worth noting that show will obscure any
-- password info embedded in the URI, so that's nice.
toURI' :: URI -> URI'
toURI' = URI' . show

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

instance Arbitrary URI where

fileFromURI :: URI -> IO (Either IOException L.ByteString)
fileFromURI uri = fileFromURIStrict uri

fileFromURIStrict :: URI -> IO (Either IOException L.ByteString)
fileFromURIStrict uri = try $
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> L.readFile (uriPath uri)
      -- ("ssh:", Just auth) -> cmdOutputStrict ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " cat " ++ show (uriPath uri))
      ("ssh:", Just auth) ->
          run (proc "ssh" [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "cat", uriPath uri])
      _ ->
          run (proc "curl" ["-s", "-g", uriToString' uri])

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


dirFromURI :: URI -> IO (Either IOException [String])
dirFromURI uri = try $ do
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> getDirectoryContents (uriPath uri)
      ("ssh:", Just auth) ->
          (Prelude.lines . L.toString) <$>
            run (proc "ssh" [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "ls", "-1", uriPath uri])
      _ ->
          (webServerDirectoryContents =<< (T.pack . L.toString) <$> run (proc "curl" ["-s", "-g", uriToString' uri]))
            `catch` (\(e :: IOException) -> throw (userError (show e ++ ": " ++ show uri)))

run :: CreateProcess -> IO L.ByteString
run cp = do
  (code, out, err) <- readCreateProcessWithExitCode cp L.empty
  case code of
    ExitSuccess -> return out
    ExitFailure _ -> throw (userError (show code ++ "\n" ++
                                       " command: " ++ showCreateProcessForUser cp ++ "\n" ++
                                       " stderr: " ++ unpack (decodeUtf8 (L.toStrict err)) ++ "\n" ++
                                       " stdout: " ++ unpack (decodeUtf8 (L.toStrict out))))
