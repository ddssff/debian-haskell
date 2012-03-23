{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Debian.Deb where

import Control.Monad

import Data.ByteString.Lazy (empty)
import Debian.Control.Common
import System.Unix.Directory
import System.Unix.FilePath
import System.Unix.LazyProcess

fields :: (ControlFunctions a) => FilePath -> IO (Control' a)
fields debFP =
    withTemporaryDirectory ("fields.XXXXXX") $ \tmpdir ->
      do debFP <- realpath debFP
         withWorkingDirectory tmpdir $ 
           do (out, err, res) <- lazyProcess "ar" ["x",debFP,"control.tar.gz"] Nothing Nothing empty >>= return . collectOutputUnpacked 
              when (res /= ExitSuccess) (error $ "Dpkg.fields: " ++ out ++ "\n" ++ err ++ "\n" ++ show res)
              (out, err, res) <- lazyProcess "tar" ["xzf", "control.tar.gz", "./control"] Nothing Nothing empty >>= return . collectOutputUnpacked
              when (res /= ExitSuccess) (error $ "Dpkg.fields: " ++ out ++ "\n" ++ err ++ "\n" ++ show res)
              c <- parseControlFromFile "control" 
              case c of
                Left e -> error (show e)
                (Right c) -> return c -- I don't think we need seq because parsec will force everything from the file
