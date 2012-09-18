{-# OPTIONS -fno-warn-orphans #-}
module Debian.URI
    ( module Network.URI
    , URIString
    , uriToString'
    , fileFromURI
    , fileFromURIStrict
    , dirFromURI
    ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString as B
import Data.Maybe (catMaybes)
import Network.URI
import System.Directory (getDirectoryContents)
import System.Exit (ExitCode(..))
import System.Process (showCommandForUser)
import System.Unix.Process (readProcessWithExitCode)
-- import System.Unix.LazyProcess (collectStdout)
-- import System.Unix.Progress (lazyCommandF)
import Text.Regex (mkRegex, matchRegex)

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

-- |If the URI type could be read and showed this wouldn't be necessary.
type URIString = String

fileFromURI :: URI -> IO (Either SomeException L.ByteString)
fileFromURI uri = try $
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> L.readFile (uriPath uri)
      -- ("ssh:", Just auth) -> cmdOutput ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " cat " ++ show (uriPath uri))
      ("ssh:", Just auth) ->
          do let cmd  = "ssh"
                 args = [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "cat", uriPath uri]
             (code, out, _err) <- readProcessWithExitCode cmd args id L.empty
             case code of
               ExitFailure _ -> error (showCommandForUser cmd args ++ " -> " ++ show code)
               _ -> return out
      _ ->
          do let cmd = "curl"
                 args = ["-s", "-g", uriToString' uri]
             (_code, out, _err) <- readProcessWithExitCode cmd args id L.empty
             return out

fileFromURIStrict :: URI -> IO (Either SomeException B.ByteString)
fileFromURIStrict uri = try $
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> B.readFile (uriPath uri)
      -- ("ssh:", Just auth) -> cmdOutputStrict ("ssh " ++ uriUserInfo auth ++ uriRegName auth ++ uriPort auth ++ " cat " ++ show (uriPath uri))
      ("ssh:", Just auth) -> do
          let cmd = "ssh"
              args = [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "cat", uriPath uri]
          (_code, out, _err) <- readProcessWithExitCode cmd args id L.empty
          return . B.concat . L.toChunks $ out
      _ -> do
          let cmd = "curl"
              args = ["-s", "-g", uriToString' uri]
          (_code, out, _err) <- readProcessWithExitCode cmd args id L.empty
          return . B.concat . L.toChunks $ out

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
dirFromURI uri = try $
    case (uriScheme uri, uriAuthority uri) of
      ("file:", Nothing) -> getDirectoryContents (uriPath uri)
      ("ssh:", Just auth) ->
          do let cmd = "ssh"
                 args = [uriUserInfo auth ++ uriRegName auth ++ uriPort auth, "ls", "-1", uriPath uri]
             (_code, out, _err) <- readProcessWithExitCode cmd args id L.empty
             return . lines . L.unpack $ out
      _ ->
          do let cmd = "curl"
                 args = ["-s", "-g", uriToString' uri]
             (_code, out, _err) <- readProcessWithExitCode cmd args id L.empty
             return . webServerDirectoryContents $ out
