module Main where

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Debian.Control  -- (Control(..),lookupP,parseControlFromFile)
import Debian.Relation
import System.Process
import System.Exit
import System.Environment

lookupBuildDeps :: FilePath -> IO [PkgName]
lookupBuildDeps fp =
    do control <- parseControlFromFile fp
       case control of 
         (Left e) -> error (show e)
         (Right (Control (p:_))) ->
             return $ ((lookupDepends "Build-Depends" p) ++
                       (lookupDepends "Build-Depends-Indep" p))

lookupDepends :: String -> Paragraph' String -> [PkgName]
lookupDepends key paragraph = 
    case fieldValue key paragraph of
                Nothing -> [] -- (Left $ "could not find key " ++ key)
                (Just relationString) -> 
                    case parseRelations relationString of
                      (Left e) -> error (show e)
                      (Right andRelations) ->
                          map pkgName (concatMap (take 1) andRelations)
    where
      pkgName :: Relation -> PkgName
      pkgName (Rel name _ _) = name


aptGetInstall :: [String] -> [PkgName] -> IO ExitCode
aptGetInstall options pkgnames =
    do (_,_,_,ph)
         <- createProcess $ proc "apt-get" $ ["install"] ++ options ++ pkgnames
       waitForProcess ph
       
main :: IO ()
main
  = do
    options <- getArgs
    lookupBuildDeps "debian/control" >>= aptGetInstall options >>= exitWith
