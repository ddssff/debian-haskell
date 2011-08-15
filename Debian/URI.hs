module Debian.URI
    ( module Network.URI
    , URIString
    , uriToString'
    , fileFromURI
    , fileFromURIStrict
    , dirFromURI
    ) where

import Control.Exception (SomeException, {-ErrorCall(ErrorCall),-} try)
--import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import Data.Maybe (catMaybes)
import Network.URI
import System.Directory (getDirectoryContents)
--import System.Exit
--import System.Unix.Process (lazyCommand, collectOutput)
import System.Unix.Process (collectStdout)
import System.Unix.Progress (lazyCommandSF, quieter)
--import System.Unix.ProcessExtra (cmdOutput, cmdOutputStrict)
import Text.Regex (mkRegex, matchRegex)

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

instance Ord URI where
    compare a b = compare (uriToString' a) (uriToString' b)

-- |If the URI type could be read and showed this wouldn't be necessary.
type URIString = String

fileFromURI :: URI -> IO (Either SomeException L.ByteString)
fileFromURI uri =
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> try (L.readFile (uriPath uri))
      -- ("ssh:", Just auth) -> cmdOutput ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " cat " ++ show (uriPath uri))
      ("ssh:", Just auth) -> try (lazyCommandSF ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " cat " ++ show (uriPath uri)) L.empty >>=
                                  return . fst . collectStdout)
      _ -> try (lazyCommandSF ("curl -s -g '" ++ uriToString' uri ++ "'") L.empty >>=
                return . fst . collectStdout)

fileFromURIStrict :: URI -> IO (Either SomeException B.ByteString)
fileFromURIStrict uri =
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> try (B.readFile (uriPath uri))
      -- ("ssh:", Just auth) -> cmdOutputStrict ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " cat " ++ show (uriPath uri))
      ("ssh:", Just auth) -> try (lazyCommandSF ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " cat " ++ show (uriPath uri)) L.empty >>=
                                  return . B.concat . L.toChunks . fst . collectStdout)
      _ -> try (lazyCommandSF ("curl -s -g '" ++ uriToString' uri ++ "'") L.empty >>=
                return . B.concat . L.toChunks . fst . collectStdout)
      -- _ -> cmdOutputStrict ("curl -s -g '" ++ uriToString' uri ++ "'")

-- | Parse the text returned when a directory is listed by a web
-- server.  This is currently only known to work with Apache.
-- NOTE: there is a second copy of this function in
-- Extra:Extra.Net. Please update both locations if you make changes.
webServerDirectoryContents :: L.ByteString -> [String]
webServerDirectoryContents text =
    catMaybes . map (second . matchRegex re) . lines . L.unpack $ text
    where
      re = mkRegex "( <A HREF|<a href)=\"([^/][^\"]*)/\""
      second (Just [_, b]) = Just b
      second _ = Nothing


dirFromURI :: URI -> IO (Either SomeException [String])
dirFromURI uri =
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> try (getDirectoryContents (uriPath uri))
      -- ("ssh:", Just auth) -> cmdOutput ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " ls -1 " ++ uriPath uri) >>=
      ("ssh:", Just auth) -> try (lazyCommandSF ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " ls -1 " ++ uriPath uri) L.empty >>=
                                  return . lines . L.unpack . fst . collectStdout)
      -- _ -> cmdOutput ("curl -s -g '" ++ uriToString' uri ++ "/'") >>= return . either Left (Right . webServerDirectoryContents)
      _ -> try (lazyCommandSF ("curl -s -g '" ++ uriToString' uri ++ "/'") L.empty >>= return . webServerDirectoryContents . fst . collectStdout)
