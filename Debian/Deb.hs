{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Debian.Deb where

import Control.Monad

import Data.ByteString.Lazy (empty)
import Debian.Control.Common
import System.Exit (ExitCode(..))
import System.Process (CmdSpec(RawCommand))
import System.Unix.Directory (withTemporaryDirectory, withWorkingDirectory)
import System.Unix.FilePath (realpath)
import System.Process.Read (readModifiedProcessWithExitCode)

fields :: (ControlFunctions a) => FilePath -> IO (Control' a)
fields debFP =
    withTemporaryDirectory ("fields.XXXXXX") $ \tmpdir ->
      do debFP <- realpath debFP
         withWorkingDirectory tmpdir $
           do (res, out, err) <- readModifiedProcessWithExitCode id (RawCommand "ar" ["x",debFP,"control.tar.gz"]) empty
              when (res /= ExitSuccess) (error $ "Dpkg.fields: " ++ show out ++ "\n" ++ show err ++ "\n" ++ show res)
              (res, out, err) <- readModifiedProcessWithExitCode id (RawCommand "tar" ["xzf", "control.tar.gz", "./control"]) empty
              when (res /= ExitSuccess) (error $ "Dpkg.fields: " ++ show out ++ "\n" ++ show err ++ "\n" ++ show res)
              c <- parseControlFromFile "control" 
              case c of
                Left e -> error (show e)
                (Right c) -> return c -- I don't think we need seq because parsec will force everything from the file
