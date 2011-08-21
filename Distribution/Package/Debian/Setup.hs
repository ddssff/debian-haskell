-- |
-- Module      :  Distribution.Package.Debian.Setup
-- Copyright   :  David Fox 2008
--
-- Maintainer  :  David Fox <dsf@seereason.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Command line option processing for building RPM
-- packages.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Debian.Setup (
      Flags(..)
    , DebAction(..)
    , DebType(..)
    , parseArgs
    ) where

import Control.Monad (when)
import Data.Char (toLower)
import Data.Version (Version, parseVersion)
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.ReadE (readEOrFail)
import Distribution.PackageDescription (FlagName(..))
import Distribution.Verbosity (Verbosity, flagToVerbosity, normal)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..),
                              usageInfo, getOpt')
import System.Environment (getProgName)
import System.Exit (exitWith, ExitCode (..))
import System.IO (Handle, hPutStrLn, stderr, stdout)
import Text.ParserCombinators.ReadP (readP_to_S)

data Flags = Flags
    {
      rpmPrefix :: FilePath
    , rpmCompiler :: CompilerFlavor
    , rpmCompilerVersion :: Maybe Version
    , rpmConfigurationsFlags :: [(FlagName, Bool)]
    , rpmHaddock :: Bool
    , rpmHelp :: Bool
    , debLibProf :: Bool
    , rpmName :: Maybe String
    , rpmOptimisation :: Bool
    , rpmRelease :: Maybe String
    , rpmSplitObjs :: Bool
    , debOutputDir :: FilePath
    , buildRoot :: FilePath
    , rpmVerbosity :: Verbosity
    , rpmVersion :: Maybe String
    , debMaintainer :: Maybe String
    , debAction :: DebAction
    , buildDeps :: [String]
    }
    deriving (Eq, Show)

data DebType = Dev| Prof | Doc deriving (Eq, Read, Show)

data DebAction = Usage | Debianize | SubstVar DebType | UpdateDebianization deriving (Eq, Show)

emptyFlags :: Flags

emptyFlags = Flags
    {
      rpmPrefix = "/usr/lib/haskell-packages/ghc6"
    , rpmCompiler = GHC
    , rpmCompilerVersion = Nothing
    , rpmConfigurationsFlags = []
    , rpmHaddock = True
    , rpmHelp = False
    , debLibProf = True
    , rpmName = Nothing
    , rpmOptimisation = True
    , rpmRelease = Nothing
    , rpmSplitObjs = True
    , debOutputDir = "./debian"
    , buildRoot = "/"
    , rpmVerbosity = normal
    , rpmVersion = Nothing
    , debMaintainer = Nothing
    , debAction = Usage
    , buildDeps = []
    }

options :: [OptDescr (Flags -> Flags)]

options =
    [
      Option "" ["prefix"] (ReqArg (\ path x -> x { rpmPrefix = path }) "PATH")
             "Pass this prefix if we need to configure the package",
      Option "" ["ghc"] (NoArg (\x -> x { rpmCompiler = GHC }))
             "Compile with GHC",
      Option "" ["hugs"] (NoArg (\x -> x { rpmCompiler = Hugs }))
             "Compile with Hugs",
      Option "" ["jhc"] (NoArg (\x -> x { rpmCompiler = JHC }))
             "Compile with JHC",
      Option "" ["nhc"] (NoArg (\x -> x { rpmCompiler = NHC }))
             "Compile with NHC",
      Option "h?" ["help"] (NoArg (\x -> x { rpmHelp = True }))
             "Show this help text",
      Option "" ["ghc-version"] (ReqArg (\ ver x -> x { rpmCompilerVersion = Just (last (map fst (readP_to_S parseVersion ver)))}) "VERSION")
             "Version of GHC in build environment",
      Option "" ["name"] (ReqArg (\name x -> x { rpmName = Just name }) "NAME")
             "Override the default package name",
      Option "" ["disable-haddock"] (NoArg (\x -> x { rpmHaddock = False }))
             "Don't generate API docs",
      Option "" ["disable-library-profiling"] (NoArg (\x -> x { debLibProf = False }))
             "Don't generate profiling libraries",
      Option "" ["disable-optimization"] (NoArg (\x -> x { rpmOptimisation = False }))
             "Don't generate optimised code",
      Option "" ["disable-split-objs"] (NoArg (\x -> x { rpmSplitObjs = False }))
             "Don't split object files to save space",
      Option "f" ["flags"] (ReqArg (\flags x -> x { rpmConfigurationsFlags = rpmConfigurationsFlags x ++ flagList flags }) "FLAGS")
             "Set given flags in Cabal conditionals",
      Option "" ["release"] (ReqArg (\rel x -> x { rpmRelease = Just rel }) "RELEASE")
             "Override the default package release",
      Option "" ["debdir"] (ReqArg (\path x -> x { debOutputDir = path }) "DEBDIR")
             ("Override the default output directory (" ++ show (debOutputDir emptyFlags) ++ ")"),
      Option "" ["root"] (ReqArg (\ path x -> x { buildRoot = path }) "BUILDROOT")
             "Use the compiler information in the given build environment.",
      Option "v" ["verbose"] (ReqArg (\verb x -> x { rpmVerbosity = readEOrFail flagToVerbosity verb }) "n")
             "Change build verbosity",
      Option "" ["version"] (ReqArg (\vers x -> x { rpmVersion = Just vers }) "VERSION")
             "Override the default package version",
      Option "" ["maintainer"] (ReqArg (\maint x -> x { debMaintainer = Just maint }) "Maintainer Name <email addr>")
             "Override the Maintainer name and email in $DEBEMAIL/$EMAIL/$DEBFULLNAME/$FULLNAME",
      Option "" ["debianize"] (NoArg (\x -> x {debAction = Debianize}))
             "Generate a new debianization, replacing any existing one.  One of --debianize, --substvar, or --update-debianization is required.",
      Option "" ["build-dep"] (ReqArg (\ name x -> x {buildDeps = name : (buildDeps x)}) "Debian binary package name")
             "Specify a package to add to the build dependency list in debian/control, e.g. '--build-dep libglib2.0-dev'.",
      Option "" ["substvar"] (ReqArg (\ name x -> x {debAction = SubstVar (read name)}) "Doc, Prof, or Dev")
             (unlines ["Write out the list of dependencies required for the dev, prof or doc package depending",
                       "on the argument.  This value can be added to the appropriate substvars file."]),
      Option "" ["update-debianization"] (NoArg (\x -> x {debAction = UpdateDebianization}))
             "Update an existing debianization, or generate a new one."
    ]

-- Lifted from Distribution.Simple.Setup, since it's not exported.
flagList :: String -> [(FlagName, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (FlagName (map toLower name), False)
        tagWithValue name       = (FlagName (map toLower name), True)

printHelp :: Handle -> IO ()

printHelp h = do
    progName <- getProgName
    let info = "Usage: " ++ progName ++ " [FLAGS]\n"
    hPutStrLn h (usageInfo info options)

parseArgs :: [String] -> IO Flags

parseArgs args = do
     let (os, args', unknown, errs) = getOpt' RequireOrder options args
         opts = foldl (flip ($)) emptyFlags os
     when (rpmHelp opts || debAction opts == Usage) $ do
       printHelp stdout
       exitWith ExitSuccess
     when (not (null errs)) $ do
       hPutStrLn stderr "Errors:"
       mapM_ (hPutStrLn stderr) errs
       exitWith (ExitFailure 1)
     when (not (null unknown)) $ do
       hPutStrLn stderr "Unrecognised options:"
       mapM_ (hPutStrLn stderr) unknown
       exitWith (ExitFailure 1)
     when (not (null args')) $ do
       hPutStrLn stderr "Unrecognised arguments:"
       mapM_ (hPutStrLn stderr) args'
       exitWith (ExitFailure 1)
     return opts
