-- |
-- Module      :  Distribution.Package.Debian.Bundled
-- Copyright   :  David Fox 2008
--
-- Maintainer  :  David Fox <dsf@seereason.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Debian.Bundled
    (
      Bundled 
    , bundledWith
    , isBundled
    , isLibrary
    , docPrefix
    -- , builtIns
    -- , ghc6BuiltIns
    , PackageType(..)
    , debianName
    , ghcBuiltIns
    , sourcePackageName
    , docPackageName
    , utilsPackageName
    , basePackageName
    , libPackageName
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.Function (on)
import Data.List (find, isPrefixOf, sortBy, groupBy)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, fromJust)
import Data.Version (Version(..))
import Debian.Control(Control'(Control), fieldValue, parseControlFromFile)
import Debian.Relation.ByteString()
import Debian.Relation(Relation(Rel),parseRelations)
import Distribution.InstalledPackageInfo(InstalledPackageInfo, libraryDirs, sourcePackageId)
import Distribution.Simple.Compiler (Compiler(..), CompilerId(..), CompilerFlavor(..), PackageDB(GlobalPackageDB), compilerFlavor)
import Distribution.Simple.Configure (getInstalledPackages)
-- import Distribution.Simple.GHC  (getInstalledPackages)
import Distribution.Simple.PackageIndex (PackageIndex, SearchResult(None, Unambiguous, Ambiguous), allPackages, searchByName)
import Distribution.Simple.Program (configureAllKnownPrograms, defaultProgramConfiguration)
import Distribution.Package (PackageIdentifier(..), PackageName(..), Dependency(..))
import Distribution.Verbosity(normal)
import Distribution.Version (withinRange, VersionRange(..))
import System.Unix.Chroot (fchroot)
import System.Unix.Process (lazyProcess, collectStdout)
import Data.ByteString.Lazy.Char8 (empty, unpack)
import Text.ParserCombinators.Parsec(ParseError)
import Text.Regex.TDFA ((=~))

-- | List the packages bundled with this version of the given
-- compiler.  If the answer is not known, return the empty list.
bundledWith :: [(CompilerFlavor, Version, [PackageIdentifier])] -> Compiler -> Maybe [PackageIdentifier]
bundledWith builtIns c =
    let cv = (compilerFlavor c, (\ (CompilerId _ v) -> v) $ compilerId c)
    in thd `fmap` find (\(n,v,_) -> (n,v) == cv) builtIns
  where thd (_,_,x) = x

-- | Determine whether a specific version of a Haskell package is
-- bundled with into this particular version of the given compiler.
isBundled :: [(CompilerFlavor, Version, [PackageIdentifier])] -> Compiler -> Dependency -> Bool
isBundled builtIns c (Dependency pkg version) =
    let cv = (compilerFlavor c, (\ (CompilerId _ v) -> v) (compilerId c))
    in
      case find (\(n, k, _) -> (n,k) == cv) builtIns of
        Nothing -> False
        (Just (_, _, cb)) ->
          any checkVersion $ pkgVersion `fmap` filter ((== pkg) . pkgName) cb
  where checkVersion = flip withinRange version

type Bundled = (CompilerFlavor, Version, [PackageIdentifier])

-- |Return a list of built in packages for the compiler in an environment.
-- ghcBuiltIns :: FilePath -> IO [PackageIdentifier]
-- ghcBuiltIns root =
--     fchroot root (lazyProcess "ghc-pkg" ["list", "--simple-output"] Nothing Nothing empty) >>=
--     return . map parsePackageIdentifier . words . unpack . fst . collectStdout
--     where
--       parsePackageIdentifier s =
--           let (v', n') = break (== '-') (reverse s)
--               (v, n) = (reverse (tail n'), reverse v') in
--           PackageIdentifier (PackageName n) (Version (map read (filter (/= ".") (groupBy (\ a b -> (a == '.') == (b == '.')) v))) [])

ghcBuiltIns :: Compiler -> Bundled
ghcBuiltIns compiler@(Compiler {compilerId = CompilerId GHC compilerVersion}) =
    fromJust $
    Map.lookup compilerVersion (Map.fromList [ (Version [7,2,1] [], (GHC, Version [7,2,1] [], ghc721BuiltIns))
                                             , (Version [7,0,3] [], (GHC, Version [7,0,3] [], ghc701BuiltIns))
                                             , (Version [7,0,1] [], (GHC, Version [7,0,1] [], ghc701BuiltIns))
                                             , (Version [6,8,3] [], (GHC, Version [6,8,3] [], ghc683BuiltIns))
                                             , (Version [6,8,2] [], (GHC, Version [6,8,2] [], ghc682BuiltIns))
                                             , (Version [6,8,1] [], (GHC, Version [6,8,1] [], ghc681BuiltIns))
                                             , (Version [6,6,1] [], (GHC, Version [6,6,1] [], ghc661BuiltIns))
                                             , (Version [6,6] [], (GHC, Version [6,6] [], ghc66BuiltIns)) ])

builtIns :: Compiler -> IO [Bundled]
builtIns compiler = 
    do ghc6 <- fmap maybeToList $ ghc6BuiltIns compiler
       return $ ghc6 ++ [ (GHC, Version [7,2,1] [], ghc721BuiltIns)
                        , (GHC, Version [7,0,3] [], ghc701BuiltIns)
                        , (GHC, Version [7,0,1] [], ghc701BuiltIns)
                        , (GHC, Version [6,8,3] [], ghc683BuiltIns)
                        , (GHC, Version [6,8,2] [], ghc682BuiltIns)
                        , (GHC, Version [6,8,1] [], ghc681BuiltIns)
                        , (GHC, Version [6,6,1] [], ghc661BuiltIns)
                        , (GHC, Version [6,6] [], ghc66BuiltIns)
                        ]

ghc6BuiltIns :: Compiler -> IO (Maybe (CompilerFlavor, Version, [PackageIdentifier]))
ghc6BuiltIns compiler@(Compiler {compilerId = CompilerId GHC compilerVersion}) =
#ifdef CABAL19
    do installedPackages <- getInstalledPackageIndex compiler
       ghc6Files <- fmap lines $ readFile "/var/lib/dpkg/info/ghc.list"
       let ghcProvides = filter (\package -> any (\dir -> elem dir ghc6Files) (libraryDirs package)) (allPackages installedPackages)
       return (Just (GHC, compilerVersion, map sourcePackageId ghcProvides))
#else
    do mInstalledPackages <- getInstalledPackageIndex compiler
       case mInstalledPackages of
         Nothing -> error "Could not find the installed package database."
         (Just installedPackages) ->
             do ghc6Files <- fmap lines $ readFile "/var/lib/dpkg/info/ghc.list"
                let ghcProvides = filter (\package -> any (\dir -> elem dir ghc6Files) (libraryDirs package)) (allPackages installedPackages)
                return (Just (GHC, compilerVersion, map sourcePackageId ghcProvides))
#endif
ghc6BuiltIns _ = return Nothing

ghc6BuiltIns' :: Compiler -> IO (Maybe (CompilerFlavor, Version, [PackageIdentifier]))
ghc6BuiltIns' compiler@(Compiler {compilerId = CompilerId GHC compilerVersion}) =
    do eDebs <- ghc6Provides
       case eDebs of
         Left e -> error e
         Right debNames ->
#ifdef CABAL19
             do installedPackages <- getInstalledPackageIndex compiler
                let packages = concatMap (\n -> fromRight $ installedVersions (fromRight $ extractBaseName n) installedPackages) debNames
                return $ Just (GHC, compilerVersion, packages)
#else
             do mInstalledPackages <- getInstalledPackageIndex compiler
                case mInstalledPackages of
                  Nothing -> error "Could not find the installed package database."
                  (Just installedPackages) ->
                      let packages = concatMap (\n -> fromRight $ installedVersions (fromRight $ extractBaseName n) installedPackages) debNames
                      in
                        return $ Just (GHC, compilerVersion, packages)
#endif
    where
      fromRight (Right r) = r
      fromRight (Left e) = error e
ghc6BuiltIns' compiler@(Compiler {}) = return Nothing

ghc6Provides :: IO (Either String [String])
ghc6Provides = 
    do eC <- parseControlFromFile "/var/lib/dpkg/status" :: IO (Either ParseError (Control' B.ByteString))
       case eC of
         Left e  -> return $ Left (show e)
         Right (Control c) ->
             case find (\p -> fieldValue "Package" p == Just (B.pack "ghc")) c of
               Nothing -> return $ Left "You do not seem to have ghc installed."
               (Just p) ->
                   case fieldValue "Provides" p of
                     Nothing -> return $ Left "Your ghc package does not seem to Provide anything."
                     (Just p) -> 
                         case parseRelations p of
                           (Left e) -> return (Left (show e))
                           (Right relations) ->
                               return $ Right $ filter (isPrefixOf "libghc-") $ map (\ (Rel pkgName _ _) -> pkgName) (concat relations)


extractBaseName :: String -> Either String String
extractBaseName name =
    let (_,_,_,subs) = (name =~ "^libghc-(.*)-.*$") :: (String, String, String, [String])
    in case subs of
         [base] -> Right base
         _ -> Left ("When attempt to extract the base name of " ++ name ++ " I found the following matches: " ++ show subs)
                 
--getInstalledPackageIndex :: Compiler -> IO (Maybe PackageIndex)
getInstalledPackageIndex compiler =
    do pc  <- configureAllKnownPrograms normal  defaultProgramConfiguration
       getInstalledPackages normal compiler [GlobalPackageDB] pc

installedVersions :: String -> PackageIndex -> Either String [PackageIdentifier]
installedVersions name packageIndex = 
    case searchByName packageIndex name of
      None -> Left $ "The package " ++ name ++ " does not seem to be installed."
      Unambiguous pkgs -> 
          case sortBy (compare `on` (pkgVersion . sourcePackageId)) pkgs of
            [] -> Left $ "Odd. searchByName returned an empty Unambiguous match for " ++ name
            ps -> Right (map sourcePackageId ps)
                                   
v :: String -> [Int] -> PackageIdentifier
v n x = PackageIdentifier (PackageName n) (Version x [])

ghc721BuiltIns :: [PackageIdentifier]
ghc721BuiltIns = [
    v "Cabal" [1,12,0],
    v "array" [0,3,0,3],
    v "base" [4,4,0,0],
    v "bin-package-db" [0,0,0,0],
    v "binary" [0,5,0,2],
    v "bytestring" [0,9,2,0],
    v "containers" [0,4,1,0],
    v "directory" [1,1,0,1],
    v "extensible-exceptions" [0,1,1,3],
    v "filepath" [1,2,0,1],
    v "ghc" [7,2,1],
    -- ghc-binary renamed to binary
    v "ghc-prim" [0,2,0,0],
    v "haskell2010" [1,1,0,0],
    v "haskell98" [2,0,0,0],
    v "hoopl" [3,8,7,1], -- new
    v "hpc" [0,5,1,0],
    v "integer-gmp" [0,3,0,0],
    v "old-locale" [1,0,0,3],
    v "old-time" [1,0,0,7],
    v "pretty" [1,1,0,0],
    v "process" [1,1,0,0], 
    -- random removed
    v "rts" [1,0],
    v "template-haskell" [2,6,0,0],
    v "time" [1,2,0,5],
    v "unix" [2,5,0,0] ]

ghc701BuiltIns :: [PackageIdentifier]
ghc701BuiltIns = [
    v "Cabal" [1,10,0,0],
    v "array" [0,3,0,2],
    v "base" [4,3,0,0],
    v "bin-package-db" [0,0,0,0],
    v "bytestring" [0,9,1,8],
    v "containers" [0,4,0,0],
    v "directory" [1,1,0,0],
    v "extensible-exceptions" [0,1,1,2],
    v "filepath" [1,2,0,0],
    v "ghc" [7,0,1],
    v "ghc-binary" [0,5,0,2],
    v "ghc-prim" [0,2,0,0],
    v "haskell2010" [1,0,0,0],
    v "haskell98" [1,1,0,0],
    v "hpc" [0,5,0,6],
    v "integer-gmp" [0,2,0,2],
    v "old-locale" [1,0,0,2],
    v "old-time" [1,0,0,6],
    v "pretty" [1,0,1,2],
    v "process" [1,0,1,4],
    v "random" [1,0,0,3],
    v "rts" [1,0],
    v "template-haskell" [2,5,0,0],
    v "time" [1,2,0,3],
    v "unix" [2,4,1,0]
  ]

ghc683BuiltIns :: [PackageIdentifier]
ghc683BuiltIns = ghc682BuiltIns

ghc682BuiltIns :: [PackageIdentifier]
ghc682BuiltIns = [
    v "Cabal" [1,2,3,0],
    v "array" [0,1,0,0],
    v "base" [3,0,1,0],
    v "bytestring" [0,9,0,1],
    v "containers" [0,1,0,1],
    v "directory" [1,0,0,0],
    v "filepath" [1,1,0,0],
    v "ghc" [6,8,2,0],
    v "haskell98" [1,0,1,0],
    v "hpc" [0,5,0,0],
    v "old-locale" [1,0,0,0],
    v "old-time" [1,0,0,0],
    v "packedstring" [0,1,0,0],
    v "pretty" [1,0,0,0],
    v "process" [1,0,0,0],
    v "random" [1,0,0,0],
    v "readline" [1,0,1,0],
    v "template-haskell" [2,2,0,0],
    v "unix" [2,3,0,0]
    ]

ghc681BuiltIns :: [PackageIdentifier]
ghc681BuiltIns = [
    v "base" [3,0,0,0],
    v "Cabal" [1,2,2,0],
    v "GLUT" [2,1,1,1],
    v "HGL" [3,2,0,0],
    v "HUnit" [1,2,0,0],
    v "OpenAL" [1,3,1,1],
    v "OpenGL" [2,2,1,1],
    v "QuickCheck" [1,1,0,0],
    v "X11" [1,2,3,1],
    v "array" [0,1,0,0],
    v "bytestring" [0,9,0,1],
    v "cgi" [3001,1,5,1],
    v "containers" [0,1,0,0],
    v "directory" [1,0,0,0],
    v "fgl" [5,4,1,1],
    v "filepatch" [1,1,0,0],
    v "ghc" [6,8,1,0],
    v "haskell-src" [1,0,1,1],
    v "haskell98" [1,0,1,0],
    v "hpc" [0,5,0,0],
    v "html" [1,0,1,1],
    v "mtl" [1,1,0,0],
    v "network" [2,1,0,0],
    v "old-locale" [1,0,0,0],
    v "old-time" [1,0,0,0],
    v "packedstring" [0,1,0,0],
    v "parallel" [1,0,0,0],
    v "parsec" [2,1,0,0],
    v "pretty" [1,0,0,0],
    v "process" [1,0,0,0],
    v "random" [1,0,0,0],
    v "readline" [1,0,1,0],
    v "regex-base" [0,72,0,1],
    v "regex-compat" [0,71,0,1],
    v "regex-posix" [0,72,0,1],
    v "stm" [2,1,1,0],
    v "template-haskell" [2,2,0,0],
    v "time" [1,1,2,0],
    v "unix" [2,2,0,0],
    v "xhtml" [3000,0,2,1]
    ]

ghc661BuiltIns :: [PackageIdentifier]
ghc661BuiltIns = [
    v "base" [2,1,1],
    v "Cabal" [1,1,6,2],
    v "cgi" [3001,1,1],
    v "fgl" [5,4,1],
    v "filepath" [1,0],
    v "ghc" [6,6,1],
    v "GLUT" [2,1,1],
    v "haskell98" [1,0],
    v "haskell-src" [1,0,1],
    v "HGL" [3,1,1],
    v "html" [1,0,1],
    v "HUnit" [1,1,1],
    v "mtl" [1,0,1],
    v "network" [2,0,1],
    v "OpenAL" [1,3,1],
    v "OpenGL" [2,2,1],
    v "parsec" [2,0],
    v "QuickCheck" [1,0,1],
    v "readline" [1,0],
    v "regex-base" [0,72],
    v "regex-compat" [0,71],
    v "regex-posix" [0,71],
    v "rts" [1,0],
    v "stm" [2,0],
    v "template-haskell" [2,1],
    v "time" [1,1,1],
    v "unix" [2,1],
    v "X11" [1,2,1],
    v "xhtml" [3000,0,2]
    ]

ghc66BuiltIns :: [PackageIdentifier]
ghc66BuiltIns = [
    v "base" [2,0],
    v "Cabal" [1,1,6],
    v "cgi" [2006,9,6],
    v "fgl" [5,2],
    v "ghc" [6,6],
    v "GLUT" [2,0],
    v "haskell98" [1,0],
    v "haskell-src" [1,0],
    v "HGL" [3,1],
    v "html" [1,0],
    v "HTTP" [2006,7,7],
    v "HUnit" [1,1],
    v "mtl" [1,0],
    v "network" [2,0],
    v "OpenAL" [1,3],
    v "OpenGL" [2,1],
    v "parsec" [2,0],
    v "QuickCheck" [1,0],
    v "readline" [1,0],
    v "regex-base" [0,71],
    v "regex-compat" [0,71],
    v "regex-posix" [0,71],
    v "rts" [1,0],
    v "stm" [2,0],
    v "template-haskell" [2,0],
    v "time" [1,0],
    v "unix" [1,0],
    v "X11" [1,1],
    v "xhtml" [2006,9,13]
    ]

-- |Some dependencies are libraries, some are executables.
isLibrary :: Compiler -> Dependency -> Bool
isLibrary _ (Dependency (PackageName "happy") _ ) = False
isLibrary _ _ = True

docPrefix :: String -> VersionRange -> String
docPrefix _ _ = "libghc-"

data PackageType = Development | Profiling | Documentation | Extra deriving (Eq, Show)

debianName :: PackageType -> String -> Maybe Version -> String
debianName Extra name range = name
debianName typ name@"parsec" (Just version) | version < Version [3] [] =  "libghc-parsec2" ++ suffix typ
debianName typ name@"parsec" (Just version) | version >= Version [3] [] = "libghc-parsec3" ++ suffix typ
debianName typ name _ =                                                   "libghc-" ++ map toLower name ++ suffix typ

suffix Documentation = "-doc"
suffix Development = "-dev"
suffix Profiling = "-prof"
suffix _ = ""

-- This is intended to help enforce the requirements on debian package names when building
-- them from cabal package names - no upper case, no underscores, etc.  Its semi redundant with
-- the stuff just above.

sourcePackageName :: Maybe String -> PackageName -> String
sourcePackageName base p = "haskell-" ++ basePackageName base p

docPackageName :: Maybe String -> PackageName -> String
docPackageName base p = "libghc-" ++ basePackageName base p ++ "-doc"

libPackageName :: PackageType -> Maybe String -> PackageName -> String
libPackageName typ base p = "libghc-" ++ basePackageName base p ++ suffix typ

devPackageName :: Maybe String -> PackageName -> String
devPackageName base p = "libghc-" ++ basePackageName base p ++ "-dev"

profPackageName :: Maybe String -> PackageName -> String
profPackageName base p = "libghc-" ++ basePackageName base p ++ "-prof"

utilsPackageName :: Maybe String -> PackageName -> String
utilsPackageName base p = "haskell-" ++ basePackageName base p ++ "-utils"

basePackageName :: Maybe String -> PackageName -> String
basePackageName base (PackageName p) = maybe (map (fixChars . toLower) p) id base

fixChars :: Char -> Char
fixChars '_' = '-'
fixChars c = c
