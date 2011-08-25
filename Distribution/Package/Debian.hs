{-# LANGUAGE CPP, ScopedTypeVariables, TupleSections, TypeSynonymInstances #-}

-- |
-- Module      :  Distribution.Package.Debian
-- Copyright   :  David Fox 2008
--
-- Maintainer  :  David Fox <dsf@seereason.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Support for generating Debianization from Cabal data.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Distribution.Package.Debian
    ( debian
    ) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Arrow (second)
import Control.Exception (SomeException, try, bracket, IOException)
import Control.Monad (when,mplus)
import Control.Monad.Reader (ReaderT(runReaderT), ask)
import Control.Monad.Trans (lift)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char (toLower, isSpace)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set, member, fromList)
import qualified Data.Set as Set
import qualified Data.Set (fromList)
import Data.Version (showVersion)
import Debian.Control
import qualified Debian.Relation as D
import Debian.Release (parseReleaseName)
import Debian.Changes (ChangeLogEntry(..), prettyEntry)
import Debian.Time (getCurrentLocalRFC822Time)
import Debian.Version ()
import Debian.Version.String
import Debian.Version.Internal (DebianVersion (DebianVersion))
import System.Cmd (system)
import System.Directory
import System.FilePath ((</>), dropExtension)
import System.IO (IOMode (ReadMode), hGetContents, hPutStrLn, hSetBinaryMode, openFile, stderr, withFile)
import System.IO.Error (ioeGetFileName, isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)
import System.Posix.Files (setFileCreationMask)
import System.Process (readProcessWithExitCode)
import System.Unix.Chroot (fchroot)
import System.Unix.Process
import System.Environment
--import Text.ParserCombinators.ReadP

import Distribution.Text (display)
import Distribution.Simple.Compiler (CompilerFlavor(..), compilerFlavor, Compiler(..), CompilerId(..))
--import Distribution.Simple.Configure (localBuildInfoFile)
import Distribution.System (Platform(..), buildOS, buildArch)
import Distribution.License (License(..))
import Distribution.Package (Package(..), PackageIdentifier(..), PackageName(..), Dependency(..))
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Configure (configCompiler, maybeGetPersistBuildConfig)
import Distribution.Simple.InstallDirs (InstallDirs(..), InstallDirTemplates, toPathTemplate)
--import Distribution.Simple.Register (writeInstalledConfig)
--import Distribution.Simple.Setup (defaultRegisterFlags)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.PackageIndex (PackageIndex,fromList)
import Distribution.Simple.Utils (die, setupMessage)
import Distribution.PackageDescription (GenericPackageDescription(..),
                                        PackageDescription(..),
                                        allBuildInfo, buildTools, pkgconfigDepends,
                                        exeName, extraLibs)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.ParseUtils (parseQuoted)
import Distribution.Verbosity (Verbosity)
import Distribution.Version (Version(..),VersionRange(..))
import Distribution.Simple.Setup (defaultDistPref)
import Distribution.Package.Debian.Setup (Flags(..), DebAction(..), DebType(..))
import Distribution.Package.Debian.Bundled (Bundled, isBundled, PackageType(..), debianName, versionSplits, ghcBuiltIns {-, builtIns, ghc6BuiltIns-})
import qualified Distribution.Package.Debian.Bundled as D
import qualified Distribution.Compat.ReadP as ReadP
import Distribution.Text ( Text(parse) )
import Text.PrettyPrint.HughesPJ

parsePackageId' :: ReadP.ReadP PackageIdentifier PackageIdentifier
parsePackageId' = parseQuoted parse ReadP.<++ parse

type DebMap = Map.Map String (Maybe DebianVersion)

buildDebVersionMap :: IO DebMap
buildDebVersionMap =
    readFile "/var/lib/dpkg/status" >>=
    return . either (const []) unControl . parseControl "/var/lib/dpkg/status" >>=
    mapM (\ p -> case (lookupP "Package" p, lookupP "Version" p) of
                   (Just (Field (_, name)), Just (Field (_, version))) ->
                       return (Just (stripWS name, Just (parseDebianVersion (stripWS version))))
                   _ -> return Nothing) >>=
    return . Map.fromList . catMaybes

m ! k =
    maybe (error ("No version number for " ++ show k ++ " in " ++ show m)) id (Map.findWithDefault Nothing k m)

{-
trim s =
    case matchRegex re s of 
      Just [x, _, _] -> x
      _ -> error $ "Internal error: trim " ++ show s
    where
      re = mkRegex ("^[ \t\n]*(" ++ trimmed ++ ")[ \t\n]*")
      trimmed = "([^ \t\n](.*[^ \t\n])?)?"
-}

trim = dropWhile isSpace

--installedPackages :: Package pkg => IO [PackageIndex]
{-
installedPackages =
    do (out, err, code) <- lazyCommand cmd L.empty >>=  return . collectOutputUnpacked
       case code of
         [ExitSuccess] -> return . fromList $ map (last . map fst . ReadP.readP_to_S parsePackageId') (words out)
         result -> error $ "Failure: " ++ cmd ++ " -> " ++ show result ++ " (" ++ err ++ ")"
    where
      cmd = "ghc-pkg list --simple-output"
-}

simplePackageDescription :: GenericPackageDescription -> Flags
                         -> IO (Compiler, PackageDescription)
simplePackageDescription genPkgDesc flags = do
    (compiler', _) <- {- fchroot (buildRoot flags) -} (configCompiler (Just (rpmCompiler flags)) Nothing Nothing
                                                               defaultProgramConfiguration
                                                               (rpmVerbosity flags))
    let compiler = case (rpmCompilerVersion flags, rpmCompiler flags) of
                     (Just v, ghc) -> compiler' {compilerId = CompilerId ghc v}
                     _ -> compiler'
    --installed <- installedPackages
    case finalizePackageDescription (rpmConfigurationsFlags flags)
          (const True) (Platform buildArch buildOS) (compilerId compiler)
          {- (Nothing :: Maybe PackageIndex) -}
          [] genPkgDesc of
      Left e -> die $ "finalize failed: " ++ show e
      Right (pd, _) -> return (compiler, pd)
    
debian :: GenericPackageDescription	-- ^ info from the .cabal file
       -> Flags				-- ^ command line flags
       -> IO ()

debian genPkgDesc flags =
    case rpmCompiler flags of
      GHC ->
          do (compiler, pkgDesc) <- simplePackageDescription genPkgDesc flags
             let verbose = rpmVerbosity flags
             createDirectoryIfMissing True (debOutputDir flags)
             --lbi <- localBuildInfo pkgDesc flags
             debVersions <- buildDebVersionMap
             cabalPackages <- libPaths compiler debVersions >>= return . Map.fromList . map (\ p -> (cabalName p, p))
             bracket (setFileCreationMask 0o022) setFileCreationMask $ \ _ -> do
               autoreconf verbose pkgDesc
               case debAction flags of
                 SubstVar name ->
                     do control <- readFile "debian/control" >>= either (error . show) return . parseControl "debian/control"
                        substvars pkgDesc compiler debVersions control cabalPackages name
                 Debianize ->
                     debianize True pkgDesc flags compiler (debOutputDir flags)
                 UpdateDebianization ->
                     updateDebianization True pkgDesc flags compiler (debOutputDir flags)
      c -> die ("the " ++ show c ++ " compiler is not yet supported")

autoreconf :: Verbosity -> PackageDescription -> IO ()

autoreconf verbose pkgDesc = do
    ac <- doesFileExist "configure.ac"
    when ac $ do
        c <- doesFileExist "configure"
        when (not c) $ do
            setupMessage verbose "Running autoreconf" (packageId pkgDesc)
            ret <- system "autoreconf"
            case ret of
              ExitSuccess -> return ()
              ExitFailure n -> die ("autoreconf failed with status " ++ show n)

{-
localBuildInfo :: PackageDescription -> Flags -> IO LocalBuildInfo
localBuildInfo pkgDesc flags =
  maybeGetPersistBuildConfig defaultDistPref >>=
  maybe setup return >>=
  writeInstalledConfig'
    where
      -- Warning - passing the --ghc flag explicitly here probably
      -- disables support for other compilers.
      setup = system ("runhaskell Setup configure --ghc --prefix " ++ rpmPrefix flags) >> 
              maybeGetPersistBuildConfig defaultDistPref >>= 
              return . maybe (error ("Failed to create " ++ defaultDistPref)) id
      writeInstalledConfig' lbi =
          if isNothing (library pkgDesc)
          then error "cabal-debian - Unsupported: package without a library section"
          else writeInstalledConfig defaultDistPref pkgDesc lbi False Nothing >> return lbi
-}

data PackageInfo = PackageInfo { libDir :: FilePath
                               , cabalName :: String
                               , cabalVersion :: String
                               , devDeb :: Maybe (String, DebianVersion)
                               , profDeb :: Maybe (String, DebianVersion)
                               , docDeb :: Maybe (String, DebianVersion) }  

-- |Each cabal package corresponds to a directory <name>-<version>,
-- either in /usr/lib or in /usr/lib/haskell-packages/ghc/lib.
-- In that directory is a compiler subdirectory such as ghc-6.8.2.
-- In the ghc subdirectory is one or two library files of the form
-- libHS<name>-<version>.a and libHS<name>-<version>_p.a.  We can
-- determine the debian package names by running dpkg -S on these
-- names, or examining the /var/lib/dpkg/info/\*.list files.  From
-- these we can determine the source package name, and from that
-- the documentation package name.
substvars :: PackageDescription		-- ^info from the .cabal file
          -> Compiler   		-- ^compiler details
          -> DebMap
          -> Control			-- ^The debian/control file
          -> Map.Map String PackageInfo	-- ^The list of installed cabal packages
          -> DebType			-- ^The type of deb we want to write substvars for
          -> IO ()
substvars pkgDesc _compiler _debVersions control cabalPackages debType =
    case (missingBuildDeps, path) of
      -- There should already be a .substvars file produced by dh_haskell_prep,
      -- keep the relations listed there.  They will contain something like this:
      -- libghc-cabal-debian-prof.substvars:
      --    haskell:Depends=ghc-prof (<< 6.8.2-999), ghc-prof (>= 6.8.2), libghc-cabal-debian-dev (= 0.4)
      -- libghc-cabal-debian-dev.substvars:
      --    haskell:Depends=ghc (<< 6.8.2-999), ghc (>= 6.8.2)
      -- haskell-cabal-debian-doc.substvars:
      --    haskell:Depends=ghc-doc, haddock (>= 2.1.0), haddock (<< 2.1.0-999)
      ([], Just path') ->
          do old <- try (readFile path') >>= return . either (\ (_ :: SomeException) -> "") id
             let new = addDeps old
             hPutStrLn stderr (if new /= old
                               then ("cabal-debian - Updated " ++ show path' ++ ":\n " ++ old ++ "\n   ->\n " ++ new)
                               else ("cabal-debian - No updates found for " ++ show path'))
             maybe (return ()) (\ _x -> replaceFile path' new) name
      ([], Nothing) -> return ()
      (missing, _) -> 
          die ("These debian packages need to be added to the build dependency list so the required cabal packages are available:\n  " ++ intercalate "\n  " (map fst missing) ++
               "\nIf this is an obsolete package you may need to withdraw the old versions from the\n" ++
               "upstream repository, and uninstall and purge it from your local system.")
    where
      addDeps old =
          case partition (isPrefixOf "haskell:Depends=") (lines old) of
            ([], other) -> unlines (("haskell:Depends=" ++ showDeps deps) : other)
            (hdeps, more) ->
                case deps of
                  [] -> unlines (hdeps ++ more)
                  _ -> unlines (map (++ (", " ++ showDeps deps)) hdeps ++ more)
      path = maybe Nothing (\ x -> Just ("debian/" ++ x ++ ".substvars")) name
      name = case debType of Dev -> devDebName; Prof -> profDebName; Doc -> docDebName
      deps = case debType of Dev -> devDeps; Prof -> profDeps; Doc -> docDeps
      -- We must have build dependencies on the profiling and documentation packages
      -- of all the cabal packages.
      missingBuildDeps =
          let requiredDebs =
                  concat (map (\ (Dependency (PackageName name) _) ->
                               case Map.lookup name cabalPackages :: Maybe PackageInfo of
                                 Just info ->
                                     let prof = maybe (devDeb info) Just (profDeb info) in
                                     let doc = docDeb info in
                                     catMaybes [prof, doc]
                                 Nothing -> []) cabalDeps) in
          filter (not . (`elem` buildDepNames) . fst) requiredDebs
      -- Make a list of the debian devel packages corresponding to cabal packages
      -- which are build dependencies
      devDeps :: D.Relations
      devDeps =
          catMaybes (map (\ (Dependency (PackageName name) _) ->
                          case Map.lookup name cabalPackages :: Maybe PackageInfo of
                            Just package -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (devDeb package)
                            Nothing -> Nothing) cabalDeps)
      profDeps :: D.Relations
      profDeps =
          maybe [] (\ name -> [[D.Rel name Nothing Nothing]]) devDebName ++
          catMaybes (map (\ (Dependency (PackageName name) _) ->
                          case Map.lookup name cabalPackages :: Maybe PackageInfo of
                            Just package -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (profDeb package)
                            Nothing -> Nothing) cabalDeps)
      docDeps :: D.Relations
      docDeps =
          catMaybes (map (\ (Dependency (PackageName name) _) ->
                              case Map.lookup name cabalPackages :: Maybe PackageInfo of
                                Just package -> maybe Nothing (\ (s, v) -> Just [D.Rel s (Just (D.GRE v)) Nothing]) (docDeb package)
                                Nothing -> Nothing) cabalDeps)
      cabalDeps :: [Dependency]
      cabalDeps = map unboxDependency $ allBuildDepends pkgDesc
      buildDepNames :: [String]
      buildDepNames = concat (map (map (\ (D.Rel s _ _) -> s)) buildDeps)
      buildDeps :: D.Relations
      buildDeps = (either (error . show) id . D.parseRelations $ bd) ++ (either (error . show) id . D.parseRelations $ bdi)
      --sourceName = maybe (error "Invalid control file") (\ (Field (_, s)) -> stripWS s) (lookupP "Source" (head (unControl control)))
      devDebName = listToMaybe (filter (isSuffixOf "-dev") debNames)
      profDebName = listToMaybe (filter (isSuffixOf "-prof") debNames)
      docDebName = listToMaybe (filter (isSuffixOf "-doc") debNames)
      debNames = map (\ (Field (_, s)) -> stripWS s) (catMaybes (map (lookupP "Package") (tail (unControl control))))
      bd = maybe "" (\ (Field (_a, b)) -> stripWS b) . lookupP "Build-Depends" . head . unControl $ control
      bdi = maybe "" (\ (Field (_a, b)) -> stripWS b) . lookupP "Build-Depends-Indep" . head . unControl $ control

-- |Write a file which we might still be reading from in
-- order to compute the text argument.
replaceFile :: FilePath -> String -> IO ()
replaceFile path text =
    try (removeFile back) >>= either chk1 return >>		-- This may not exist
    try (renameFile path back) >>= either chk2 return >>	-- This may not exist
    writeFile path text			-- This must succeed
    where
      back = path ++ "~"
      chk1 :: IOException -> IO ()
      chk1 e =
          if ioeGetFileName e == Just back && isDoesNotExistError e
          then return ()
          else ioError e
      chk2 :: IOException -> IO ()
      chk2 e =
          if ioeGetFileName e == Just path && isDoesNotExistError e
          then return ()
          else ioError e

libPaths :: Compiler -> DebMap -> IO [PackageInfo]
libPaths compiler debVersions
    | compilerFlavor compiler == GHC =
        do a <- getDirPaths "/usr/lib"
           b <- getDirPaths "/usr/lib/haskell-packages/ghc/lib"
           -- Build a map from names of installed debs to version numbers
           dpkgFileMap >>= runReaderT (mapM (packageInfo compiler debVersions) (a ++ b)) >>= return . catMaybes
    | True = error $ "Can't handle compiler flavor: " ++ show (compilerFlavor compiler)
    where
      getDirPaths path = try (getDirectoryContents path) >>= return . map (\ x -> (path, x)) . either (\ (_ :: SomeException) -> []) id

packageInfo :: Compiler ->  DebMap -> (FilePath, String) -> ReaderT (Map.Map FilePath (Set.Set String)) IO (Maybe PackageInfo)
packageInfo compiler debVersions (d, f) =
    case parseNameVersion f of
      Nothing -> return Nothing
      Just (p, v) -> lift (doesDirectoryExist (d </> f </> cdir)) >>= cond (return Nothing) (info (d, p, v))
    where
      cdir = display (compilerId compiler)
      info (d, p, v) = 
          do dev <- debOfFile ("^" ++ d </> p ++ "-" ++ v </> cdir </> "libHS" ++ p ++ "-" ++ v ++ ".a$")
             prof <- debOfFile ("^" ++ d </> p ++ "-" ++ v </> cdir </> "libHS" ++ p ++ "-" ++ v ++ "_p.a$")
             doc <- debOfFile ("/" ++ p ++ ".haddock$")
             return (Just (PackageInfo { libDir = d
                                       , cabalName = p
                                       , cabalVersion = v
                                       , devDeb = maybe Nothing (\ x -> Just (x, debVersions ! x)) dev
                                       , profDeb = maybe Nothing (\ x -> Just (x, debVersions ! x)) prof
                                       , docDeb = maybe Nothing (\ x -> Just (x, debVersions ! x)) doc }))
      parseNameVersion s =
          case (break (== '-') (reverse s)) of
            (_a, "") -> Nothing
            (a, b) -> Just (reverse (tail b), reverse a) 

-- |Create a map from pathname to the names of the packages that contains that pathname.
-- We need to make sure we consume all the files, so 
dpkgFileMap :: IO (Map.Map FilePath (Set.Set String))
dpkgFileMap =
    do
      let fp = "/var/lib/dpkg/info"
      names <- getDirectoryContents fp >>= return . filter (isSuffixOf ".list")
      let paths = map (fp </>) names
      files <- mapM (strictReadF lines) paths
      return $ Map.fromList $ zip (map dropExtension names) (map Set.fromList files)

strictReadF f path = withFile path ReadMode (\h -> hGetContents h >>= (\x -> return $! f x))
strictRead = strictReadF id

-- |Given a path, return the name of the package that owns it.
debOfFile :: FilePath -> ReaderT (Map.Map FilePath (Set.Set String)) IO (Maybe String)
debOfFile path =
    do mp <- ask
       return $ testPath (Map.lookup path mp)
    where
      testPath :: Maybe (Set.Set FilePath) -> Maybe FilePath
      testPath Nothing = Nothing
      testPath (Just s) =
          case Set.size s of
            1 -> Just (Set.findMin s)
            _ -> Nothing

takePackageName :: String -> Maybe String
takePackageName s =
    f "" s
    where
      f name ('.':'l':'i':'s':'t':':':_) = Just (reverse name)
      f name (x : xs) = f (x : name) xs
      f _ [] = Nothing

-- Determine the documentation package name.  Look for <cabalname>.haddock
{-
devToDoc :: Compiler -> String -> IO (Maybe String)
devToDoc compiler name
    | compilerFlavor compiler == GHC =
        case maybe Nothing (dropSuffix "-dev") (dropPrefix "libghc-" name) of
          Just base -> 
              do let name1 = "libghc-" ++ base ++ "-doc"
                 let name2 = "libghc-" ++ base ++ "-doc"
                 exists1 <- doesFileExist ("/var/lib/dpkg/info/" ++ name1 ++ ".list")
                 case exists1 of
                   True -> return (Just name1)
                   False ->
                       do exists2 <- doesFileExist ("/var/lib/dpkg/info/" ++ name2 ++ ".list")
                          case exists2 of
                            True -> return (Just name2)
                            False -> return Nothing
    where
      dropSuffix :: String -> String -> Maybe String
      dropSuffix suff s = case isSuffixOf suff s of
                            True -> Just (take (length s - length suff) s)
                            False -> Nothing
      dropPrefix :: String -> String -> Maybe String
      dropPrefix pref s = case isPrefixOf pref s of
                            True -> Just (drop (length pref) s)
                            False -> Nothing
-}

cond ifF _ifT False = ifF
cond _ifF ifT True = ifT

debianize force pkgDesc flags compiler tgtPfx =
    mapM_ removeIfExists ["debian/control", "debian/changelog"] >>
    updateDebianization force pkgDesc flags compiler tgtPfx

removeFileIfExists x = doesFileExist x >>= (`when` (removeFile x))
removeDirectoryIfExists x = doesDirectoryExist x >>= (`when` (removeDirectory x))
removeIfExists x = removeFileIfExists x >> removeDirectoryIfExists x

updateDebianization :: Bool                -- ^whether to forcibly create file
                    -> PackageDescription  -- ^info from the .cabal file
                    -> Flags		 -- ^command line flags
                    -> Compiler            -- ^compiler details
                    -> FilePath            -- ^directory in which to create files
                    -> IO ()
updateDebianization _force pkgDesc flags compiler tgtPfx =
    do createDirectoryIfMissing True "debian"
       date <- getCurrentLocalRFC822Time
       copyright <- try (readFile' (licenseFile pkgDesc)) >>=
                    return . either (\ (_ :: SomeException) -> showLicense . license $ pkgDesc) id
       debianMaintainer <- getDebianMaintainer flags >>= maybe (error "Missing value for --maintainer") return
       controlUpdate (tgtPfx </> "control") flags compiler debianMaintainer pkgDesc
       changelogUpdate flags (tgtPfx </> "changelog") debianMaintainer pkgDesc date
       replaceFile (tgtPfx </> "rules") (cdbsRules pkgDesc)
       getPermissions "debian/rules" >>= setPermissions "debian/rules" . (\ p -> p {executable = True})
       replaceFile (tgtPfx </> "compat") "7" -- should this be hardcoded, or automatically read from /var/lib/dpkg/status?
       replaceFile (tgtPfx </> "copyright") copyright
       -- The dev postinst and prerm files are generated by haskell-devscripts via cdbs.
       return ()

readFile' :: FilePath -> IO String
readFile' path
  = do
    file <- openFile path ReadMode
    hSetBinaryMode file True
    hGetContents file

{-
Create a debian maintainer field from the environment variables:

  DEBFULLNAME (preferred) or NAME
  DEBEMAIL (preferred) or EMAIL

More work could be done to match dch, but this is sufficient for
now. Here is what the man page for dch has to say:

 If the environment variable DEBFULLNAME is set, this will be used for
 the maintainer full name; if not, then NAME will be checked.  If the
 environment variable DEBEMAIL is set, this will be used for the email
 address.  If this variable has the form "name <email>", then the
 maintainer name will also be taken from here if neither DEBFULLNAME
 nor NAME is set.  If this variable is not set, the same test is
 performed on the environment variable EMAIL.  Next, if the full name
 has still not been determined, then use getpwuid(3) to determine the
 name from the pass‐word file.  If this fails, use the previous
 changelog entry.  For the email address, if it has not been set from
 DEBEMAIL or EMAIL, then look in /etc/mailname, then attempt to build
 it from the username and FQDN, otherwise use the email address in the
 previous changelog entry.  In other words, it’s a good idea to set
 DEBEMAIL and DEBFULLNAME when using this script.

-}
getDebianMaintainer :: Flags -> IO (Maybe String)
getDebianMaintainer flags =
    case debMaintainer flags of
      Nothing -> envMaintainer
      maint -> return maint
    where
      envMaintainer :: IO (Maybe String)
      envMaintainer =
          do env <- map (second decodeString) `fmap` getEnvironment
             return $ do fullname <- lookup "DEBFULLNAME" env `mplus` lookup "NAME" env
                         email    <- lookup "DEBEMAIL" env `mplus` lookup "EMAIL" env
                         return (fullname ++ " <" ++ email ++ ">")

cdbsRules :: PackageDescription -> String
cdbsRules pkgDesc =
    unlines (intercalate [""] ([header, execs, comments] {- ++ devrules ++ profrules -} ))
    where
      header =
          ["#!/usr/bin/make -f",
           "include /usr/share/cdbs/1/rules/debhelper.mk",
           "include /usr/share/cdbs/1/class/hlibrary.mk"]
      execs =
          case executables pkgDesc of
            [] -> []
            [e] ->
                let exe = exeName e
                    src = "dist-ghc/build/" ++ exe ++ "/" ++ exe
                    dst = "debian/" ++ exeDeb e ++ "/usr/bin/" ++ exe in
                [ -- Magic rule required to get binaries to build in packages that have no libraries
                  "build/" ++ exeDeb e ++ ":: build-ghc-stamp"
                , "binary-fixup/" ++ exeDeb e ++ "::"
                , "\tinstall -m 755 -s -D " ++ src ++ " " ++ dst ++ " || true"]
            _ ->
                [ -- Magic rule required to get binaries to build in packages that have no libraries
                  "build/" ++ utilsDeb ++ ":: build-ghc-stamp"
                , "binary-fixup/" ++ utilsDeb ++ "::" ] ++
                map (\ executable ->
                         let exe = exeName executable
                             src = "dist-ghc/build/" ++ exe ++ "/" ++ exe
                             dst = "debian/" ++ utilsDeb ++ "/usr/bin/" ++ exe in
                         "\tinstall -m 755 -s -D " ++ src ++ " " ++ dst ++ " || true") (executables pkgDesc)
      comments =
          ["# How to install an extra file into the documentation package",
           "#binary-fixup/" ++ docDeb ++ "::",
           "#\techo \"Some informative text\" > debian/" ++ docDeb ++ "/usr/share/doc/" ++ docDeb ++ "/AnExtraDocFile"]
      p = pkgName . package $ pkgDesc
      libName = unPackageName p
      docDeb = debianName Documentation (pkgName (package pkgDesc)) (Just (pkgVersion (package pkgDesc)))
      utilsDeb = debianName Utilities (pkgName (package pkgDesc)) (Just (pkgVersion (package pkgDesc)))
      exeDeb e = debianName Extra (PackageName (exeName e)) Nothing

list :: b -> ([a] -> b) -> [a] -> b
list d f l = case l of [] -> d; _ -> f l

controlUpdate :: FilePath -> Flags -> Compiler -> String -> PackageDescription -> IO ()
controlUpdate path flags compiler debianMaintainer pkgDesc =
    return (ghcBuiltIns compiler) >>= \bundled ->
    try (readFile path) >>=
    either (\ (_ :: SomeException) -> writeFile path (show (newCtl bundled))) (\ s -> writeFile (path ++ ".new") $! show (merge (newCtl bundled) (oldCtl s)))
    where
      newCtl bundled = control flags bundled compiler debianMaintainer pkgDesc
      oldCtl s = either (const (Control [])) id (parseControl "debian/control" s)
      merge (Control new) (Control old) =
          case (new, old) of
            (_newSource : _new', []) -> Control new
            (newSource : new', oldSource : old') ->
                Control (mergeParagraphs newSource oldSource : mergeOther new' old')
      -- Merge a list of binary package paragraphs
      mergeOther new old =
          map mergePackages allNames
          where
            mergePackages name =
                case (findPackage name new, findPackage name old) of
                  (Just x, Nothing) -> x
                  (Nothing, Just x) -> x
                  (Just x, Just y) -> mergeParagraphs x y
            findPackage name paras = listToMaybe (filter (hasName name) paras)
                where hasName name para = lookupP "Package" para == Just name
            allNames = newNames ++ (oldNames \\ newNames)
            newNames = catMaybes $ map (lookupP "Package") new
            oldNames = catMaybes $ map (lookupP "Package") old

mergeParagraphs new@(Paragraph newFields) old@(Paragraph oldFields) =
    Paragraph (map mergeField fieldNames)
    where
      fieldNames = map fieldName oldFields ++ (map fieldName newFields \\ map fieldName oldFields)
      fieldName (Field (name, _)) = name
      mergeField :: String -> Field
      mergeField name =
          case (lookupP name new, lookupP name old) of
            (Just (Field (_, x)), Nothing) -> Field (name, x)
            (Nothing, Just (Field (_, x))) -> Field (name, x)
            (Just (Field (_, x)), Just (Field (_, y))) -> Field (name, mergeValues name x y)
            _ -> error $ "Internal error"
      mergeValues :: String -> String -> String -> String
      mergeValues "Build-Depends" x y =
          " " ++ (showDeps' "Build-Depends:" $ mergeDeps (parseDeps x) (parseDeps y))
      mergeValues "Depends" x y =
          " " ++ (showDeps' "Depends:" $ mergeDeps (parseDeps x) (parseDeps y))
      mergeValues _ x _ = x
      parseDeps s = either (error . show) id (D.parseRelations s)

mergeDeps :: D.Relations -> D.Relations -> D.Relations
mergeDeps x y = 
    -- foldr :: (a -> b -> b) -> b -> [a] -> b
    nub $ foldr insertDep x y
    where
      insertDep :: [D.Relation] -> D.Relations -> D.Relations
      insertDep ys xss =
          case depPackageNames ys of
            [name] -> case break (\ xs -> depPackageNames xs == [name]) xss of
                        (a, b : c) -> a ++ [b, ys] ++ c
                        (a, []) -> a ++ [ys]
            _ -> xss ++ [ys]
      depPackageNames xs = nub (map depPackageName xs)
      depPackageName (D.Rel x _ _) = x

control :: Flags -> Bundled -> Compiler -> String -> PackageDescription -> Control
control flags bundled compiler debianMaintainer pkgDesc =
    Control {unControl =
             ([sourceSpec] ++
              develLibrarySpecs ++
              profileLibrarySpecs ++ 
              docLibrarySpecs ++
              (case executables pkgDesc of
                 [] -> []
                 [e] -> [utilsSpec (debianName Extra (PackageName (exeName e)) Nothing)]
                 _ -> [utilsSpec (debianName Utilities (pkgName (package pkgDesc)) (Just (pkgVersion (package pkgDesc))))]))}
    where
      --buildDepsIndep = ""
      sourceSpec =
          Paragraph
          ([Field ("Source", " " ++ debianName Source (pkgName . package $ pkgDesc) (Just (pkgVersion (package pkgDesc)))),
            Field ("Priority", " " ++ "extra"),
            Field ("Section", " " ++ "haskell"),
            Field ("Maintainer", " " ++ debianMaintainer),
            Field ("Build-Depends", " " ++ showDeps' "Build-Depends:" (debianBuildDeps ++ map rel (buildDeps flags))),
            Field ("Build-Depends-Indep", " " ++ showDeps' "Build-Depends-Indep:" debianBuildDepsIndep),
            --Field ("Build-Depends-Indep", " " ++ buildDepsIndep),
            Field ("Standards-Version", " " ++ "3.9.1"),
            Field ("Homepage",
                   " " ++
                   if homepage pkgDesc == ""
                   then "http://hackage.haskell.org/package/" ++
                        unPackageName (pkgName $ package pkgDesc)
                   else homepage pkgDesc)])
      rel x = [D.Rel x Nothing Nothing]
      utilsSpec p =
          Paragraph
          [Field ("Package", " " ++ p),
           Field ("Architecture", " " ++ "any"),
           Field ("Section", " " ++ "misc"),
           -- No telling what the dependencies of an executable might
           -- be.  The developer will have to fill them in
           Field ("Depends", " " ++ showDeps [[D.Rel "${shlibs:Depends}" Nothing Nothing], 
                                              [D.Rel "${haskell:Depends}" Nothing Nothing],
                                              [D.Rel "${misc:Depends}" Nothing Nothing]]),
           Field ("Description", " " ++ maybe debianDescription (const executableDescription) (library pkgDesc))]
      develLibrarySpecs = if isJust (library pkgDesc) then [librarySpec "any" Development] else []
      profileLibrarySpecs = if debLibProf flags && isJust  (library pkgDesc) then [librarySpec "any" Profiling] else []
      docLibrarySpecs = if isJust (library pkgDesc) then [docSpecsParagraph] else []
      docSpecsParagraph =
          Paragraph
          [Field ("Package", " " ++ debianName Documentation (pkgName (package pkgDesc)) (Just (pkgVersion (package pkgDesc)))),
           Field ("Architecture", " " ++ "all"),
           Field ("Section", " " ++ "doc"),
           Field ("Depends", " " ++ showDeps' "Depends:" [[D.Rel "${haskell:Depends}" Nothing Nothing],
                                                           [D.Rel "${misc:Depends}" Nothing Nothing]]),
           Field ("Recommends", " " ++ "${haskell:Recommends}"),
           Field ("Suggests", " " ++ "${haskell:Suggests}"),
           Field ("Description", " " ++ libraryDescription Documentation)]
      librarySpec arch typ =
          Paragraph
          [Field ("Package", " " ++ debianName typ (pkgName (package pkgDesc)) (Just (pkgVersion (package pkgDesc)))),
           Field ("Architecture", " " ++ arch),
           Field ("Depends", " " ++ showDeps' "Depends:" (
                     (if typ == Development
                      then [[D.Rel "${shlibs:Depends}" Nothing Nothing]]
                      else []) ++
                     [[D.Rel "${haskell:Depends}" Nothing Nothing],
                      [D.Rel "${misc:Depends}" Nothing Nothing]])),
           Field ("Recommends", " " ++ "${haskell:Recommends}"),
           Field ("Suggests", " " ++ "${haskell:Suggests}"),
           Field ("Provides", " " ++ "${haskell:Provides}"),
           Field ("Description", " " ++ libraryDescription typ)]
      -- The haskell-cdbs package contains the hlibrary.mk file with
      -- the rules for building haskell packages.
      debianBuildDeps :: D.Relations
      debianBuildDeps = 
          nub $
          [[D.Rel "debhelper" (Just (D.GRE (parseDebianVersion "7.0"))) Nothing],
           [D.Rel "haskell-devscripts" (Just (D.GRE (parseDebianVersion "0.8"))) Nothing],
           [D.Rel "cdbs" Nothing Nothing],
           [D.Rel "ghc" Nothing Nothing]] ++
          (if debLibProf flags then [[D.Rel "ghc-prof" Nothing Nothing]] else []) ++
          (concat . map (debianDependencies bundled compiler buildDependencies) . allBuildDepends $ pkgDesc)
      debianBuildDepsIndep :: D.Relations
      debianBuildDepsIndep =
          nub $
          [[D.Rel "ghc-doc" Nothing Nothing]] ++
          (concat . map (debianDependencies bundled compiler docDependencies) . allBuildDepends $ pkgDesc)
      debianDescription = 
          (synopsis pkgDesc) ++
          case description pkgDesc of
            "" -> ""
            text ->
                let text' = text ++ "\n" ++
                            list "" ("\n Author: " ++) (author pkgDesc) ++
                            list "" ("\n Upstream-Maintainer: " ++) (maintainer pkgDesc) ++
                            list "" ("\n Url: " ++) (pkgUrl pkgDesc) in
                "\n " ++ (trim . intercalate "\n " . map addDot . lines $ text')
      addDot line = if all (flip elem " \t") line then "." else line
      executableDescription = " " ++ "An executable built with the " ++ display (package pkgDesc) ++ " library."
      libraryDescription Profiling = debianDescription ++ "\n .\n This package contains the libraries compiled with profiling enabled."
      libraryDescription Development = debianDescription ++ "\n .\n This package contains the normal library files."
      libraryDescription Documentation = debianDescription ++ "\n .\n This package contains the documentation files."
      libraryDescription x = error $ "Unexpected library package name suffix: " ++ show x

showDeps xss = intercalate ", " (map (intercalate " | " . map show) xss)
showDeps' prefix xss =
    intercalate (",\n " ++ prefix') (map (intercalate " | " . map show) xss)
    where prefix' = map (\ _ -> ' ') prefix

data Dependency_
  = BuildDepends Dependency
      | BuildTools Dependency
      | PkgConfigDepends Dependency
      | ExtraLibs String
    deriving (Eq, Show)

unboxDependency :: Dependency_ -> Dependency
unboxDependency (BuildDepends d) = d
unboxDependency (BuildTools d) = d
unboxDependency (PkgConfigDepends d) = d
unboxDependency (ExtraLibs d) = Dependency (PackageName d) AnyVersion

-- |Debian packages don't have per binary package build dependencies,
-- so we just gather them all up here.
allBuildDepends pkgDesc =
    nub $ map BuildDepends (buildDepends pkgDesc) ++
          concat (map (map BuildTools . buildTools) (allBuildInfo pkgDesc) ++
                  map
                    (map PkgConfigDepends . pkgconfigDepends)
                    (allBuildInfo pkgDesc) ++
                  map (map ExtraLibs . extraLibs) (allBuildInfo pkgDesc))

-- Turn a cabal dependency into a list of debian relations.  If a
-- library is required as a build dependency we need the profiling
-- version, which pulls in the regular version, and we need the
-- documentation so the cross references can be resolved.
debianDependencies :: Bundled -> Compiler -> (Compiler -> Dependency_ -> D.Relations) -> Dependency_ -> D.Relations
debianDependencies bundled compiler toDebRels dep
  | isBundled [bundled] compiler $ unboxDependency dep = []
debianDependencies _ compiler toDebRels dep = toDebRels compiler dep

changelogUpdate :: Flags -> FilePath -> String -> PackageDescription -> String -> IO ()
changelogUpdate flags path debianMaintainer pkgDesc date =
    try (readFile path) >>= either (\ (_ :: SomeException) -> writeFile path log) (const (writeFile (path ++ ".new") log))
    where
      log = changelog flags debianMaintainer pkgDesc date

changelog :: Flags -> String -> PackageDescription -> String -> String
changelog flags debianMaintainer pkgDesc date =
    render (prettyEntry
            (Entry { logPackage = debianName Source (pkgName . package $ pkgDesc) (Just (pkgVersion (package pkgDesc)))
                   , logVersion = updateOriginal f $ debianVersionNumber pkgDesc
                   , logDists = [parseReleaseName "unstable"]
                   , logUrgency = "low"
                   , logComments = "  * Debianization generated by cabal-debian\n\n"
                   , logWho = debianMaintainer
                   , logDate = date }))
    where
      f s = maybe (g s) id (debVersion flags)
      g s = maybe "" (\ n -> show n ++ ":") (epoch flags) ++ s ++ "-1~hackage1"

updateOriginal :: (String -> String) -> DebianVersion -> DebianVersion
updateOriginal f (DebianVersion str dv) = DebianVersion (f str) dv

unPackageName :: PackageName -> String
unPackageName (PackageName s) = s

--debianDevelPackageName' (Dependency (PackageName name) _) = debianDevelPackageName name

-- debianPackageName prefix name suffix = prefix ++ (map toLower name) ++ suffix

debianVersionNumber :: PackageDescription -> DebianVersion
debianVersionNumber pkgDesc = parseDebianVersion . showVersion . pkgVersion . package $ pkgDesc

-- The profiling packages depend on the other profiling packages.
-- FIXME: These should have version dependencies.
profilingDependencies :: Compiler -> Dependency_ -> D.Relations
profilingDependencies
   compiler
   (BuildDepends (Dependency name ranges))
  = concat
    (map
        (\ x -> debianRelations Profiling x ranges)
      $ filter (not . flip member (base compiler)) [name])
profilingDependencies _ _ = []

-- The development packages depend on the other development packages,
-- the ones they were built with.  FIXME: These should have version
-- dependencies.
develDependencies :: Compiler -> Dependency_ -> D.Relations
develDependencies compiler (BuildDepends (Dependency name ranges)) | member name (base compiler) = []
develDependencies _ (BuildDepends (Dependency name ranges)) =
    debianRelations Development name ranges
develDependencies _ dep@(ExtraLibs _)
  = concat (map (\ x -> debianRelations Extra x AnyVersion) $ adapt dep)
develDependencies _ _ = []

-- The build dependencies for a package include the profiling
-- libraries and the documentation packages, used for creating cross
-- references.
buildDependencies :: Compiler -> Dependency_ -> D.Relations
buildDependencies compiler (BuildDepends (Dependency name ranges)) | member name (base compiler) = []
buildDependencies _ (BuildDepends (Dependency name ranges)) =
    debianRelations Development name ranges ++ debianRelations Profiling name ranges
buildDependencies _ dep@(ExtraLibs _) =
    concat (map (\ x -> debianRelations Extra x AnyVersion) $ adapt dep)
buildDependencies _ dep =
    concat (map (\ x -> debianRelations Extra x ranges) $ adapt dep)
    where (Dependency name ranges) = unboxDependency dep

-- The documentation dependencies for a package include the documentation
-- package for any libraries which are build dependencies, so we have access
-- to all the cross references.
docDependencies :: Compiler -> Dependency_ -> D.Relations
docDependencies
    compiler
    (BuildDepends (Dependency name ranges))
  = concat
    (map
        (\ x -> debianRelations Documentation x ranges)
      $ filter (not . flip member (base compiler)) [name])
docDependencies _ _ = []

-- generated with:
-- apt-cache show ghc \
--   | grep ^Provides: \
--   | cut -d\  -f2-
--   | sed 's/, /\n/g' \
--   | grep libghc- \
--   | cut -d- -f2- \
--   | grep dev$ \
--   | sed 's/-dev//;s/$/",/;s/^/"/'

base compiler =
    Data.Set.fromList (let {- (Just (_, _, xs)) = unsafePerformIO (ghc6BuiltIns compiler) -} (_, _, xs) = ghcBuiltIns compiler in map pkgName xs)

{-
base :: Set String
base
  = Data.Set.fromList
    ["array",
      "base",
      "bin-package-db",
      "bytestring",
      "cabal",
      "containers",
      "directory",
      "extensible-exceptions",
      "filepath",
      "ghc-binary",
      "ghc-prim",
      "haskell2010",
      "haskell98",
      "hpc",
      "integer-gmp",
      "old-locale",
      "old-time",
      "pretty",
      "process",
      "random",
      "rts",
      "template-haskell",
      "time",
      "unix"]
-}

debianRelations :: PackageType -> PackageName -> VersionRange -> D.Relations
debianRelations typ name range =
    map (merge . concat . map (debianRelation typ name)) (canon range)
    where
      -- A debian dependency list is always a list of or-relations
      -- which are anded.  This function turns the more freeform cabal
      -- relations into that format.
      canon :: VersionRange -> [[VersionRange]]
      canon (IntersectVersionRanges a b) = canon a ++ canon b
      canon (UnionVersionRanges a b) = map concat (cartesianProduct [canon a, canon b])
      canon x = [[x]]
      -- Merge some combinations that frequently show up.
      merge (D.Rel name1 (Just (D.EEQ ver1)) arch1 : D.Rel name2 (Just (D.SLT ver2)) arch2 : xs)
          | name1 == name2 && ver1 == ver2 && arch1 == arch2
              = merge (D.Rel name1 (Just (D.LTE ver1)) arch1 : xs)
      merge (D.Rel name1 (Just (D.EEQ ver1)) arch1 : D.Rel name2 (Just (D.SGR ver2)) arch2 : xs)
          | name1 == name2 && ver1 == ver2 && arch1 == arch2
              = merge (D.Rel name1 (Just (D.GRE ver1)) arch1 : xs)
      merge (x : xs) = x : merge xs
      merge [] = []

-- |Turn simple Cabal relations into Debian relations.  There are some
-- special cases in here where we have to split a range because the
-- debian package name changes at a certain version number
-- (specifically, parsec and quickcheck.)
debianRelation :: PackageType -> PackageName -> VersionRange -> [D.Relation]
debianRelation typ name range@AnyVersion =
    [D.Rel (debianName typ name Nothing) Nothing Nothing]
debianRelation typ name range@(ThisVersion version) =
    [D.Rel (debianName typ name (Just version)) (Just (D.EEQ (parseDebianVersion (showVersion version)))) Nothing]
debianRelation typ name (EarlierVersion version) =
    foldr split [D.Rel (debianName typ name (Just version))  (Just (D.SLT (parseDebianVersion (showVersion version)))) Nothing] versionSplits
    where split (n, v, _, _) rels
              | n == name && version < v =
                  [D.Rel (debianName typ name (Just version)) (Just (D.SLT (parseDebianVersion (showVersion version)))) Nothing]
              | n == name && version >= v =
                  [D.Rel (debianName typ name (Just version)) (Just (D.SLT (parseDebianVersion (showVersion version)))) Nothing,
                   D.Rel (debianName typ name (Just v)) Nothing Nothing]
              | True = rels
{-
debianRelation typ name@(PackageName "parsec") range@(EarlierVersion version)
    | version < Version [3] [] =
        [D.Rel (debianName typ name (Just version)) (Just (D.SLT (parseDebianVersion (showVersion version)))) Nothing]
    | version >= Version [3] [] =
        -- Put the parsec3 package in front of parsec2 so things are
        -- more likely to get built with parsec3.
        [D.Rel (debianName typ name (Just version)) (Just (D.SLT (parseDebianVersion (showVersion version)))) Nothing,
         D.Rel (debianName typ name (Just (Version [2] []))) Nothing Nothing]
    | True =
        [D.Rel (debianName typ name (Just version))  (Just (D.SLT (parseDebianVersion (showVersion version)))) Nothing]
-}
debianRelation typ name (LaterVersion version) =
    foldr split [D.Rel (debianName typ name (Just version)) (Just (D.SGR (parseDebianVersion (showVersion version)))) Nothing] versionSplits
    where split (n, v, _, _) rels
              | n == name && version < v =
                  [D.Rel (debianName typ name (Just v)) Nothing Nothing,
                   D.Rel (debianName typ name (Just version)) (Just (D.SGR (parseDebianVersion (showVersion version)))) Nothing]
              | n == name && version >= v =
                  [D.Rel (debianName typ name (Just version)) (Just (D.GRE (parseDebianVersion (showVersion version)))) Nothing]
              | True = rels
{-
debianRelation typ name@(PackageName "parsec") range@(LaterVersion version)
    | version < Version [3] [] =
        [D.Rel (debianName typ name (Just (Version [3] []))) Nothing Nothing,
         D.Rel (debianName typ name (Just version)) (Just (D.SGR (parseDebianVersion (showVersion version)))) Nothing]
    | version >= Version [3] [] =
        [D.Rel (debianName typ name (Just version)) (Just (D.GRE (parseDebianVersion (showVersion version)))) Nothing]
    | True =
        [D.Rel (debianName typ name (Just version)) (Just (D.SGR (parseDebianVersion (showVersion version)))) Nothing]
-}
debianRelation typ name range@(WildcardVersion version) =
    [D.Rel (debianName typ name (Just version)) (Just (D.SLT (parseDebianVersion (showVersion (upperBound version))))) Nothing,
     D.Rel (debianName typ name (Just version)) (Just (D.GRE (parseDebianVersion (showVersion version)))) Nothing]
    where upperBound v = v { versionBranch = bump (versionBranch v) }
          bump = reverse . (zipWith (+) (1:(repeat 0))) . reverse

debianRelation _ _ ranges = error $ "Invalid argument to debianRelation: " ++ show ranges

adapt :: Dependency_ -> [PackageName]
adapt (BuildTools (Dependency (PackageName "gtk2hsC2hs") _))
  = [PackageName "gtk2hs-buildtools"]
adapt (BuildTools (Dependency (PackageName "gtk2hsHookGenerator") _))
  = [PackageName "gtk2hs-buildtools"]
adapt (BuildTools (Dependency (PackageName "gtk2hsTypeGen") _))
  = [PackageName "gtk2hs-buildtools"]
adapt (PkgConfigDepends (Dependency (PackageName pkg) _))
  = unsafePerformIO
    $ do
      ret
        <- readProcessWithExitCode "apt-file" ["-l", "search", pkg ++ ".pc"] ""
      return
        $ case ret of
            (ExitSuccess, out, _) -> [PackageName (takeWhile (not . isSpace) out)]
            _ -> []
adapt (ExtraLibs "gcrypt") = [PackageName "libgcrypt11-dev"]
adapt (ExtraLibs x) = [PackageName  ("lib" ++ x ++ "-dev")]
adapt dep
  = [name]
  where  (Dependency name _) = unboxDependency dep

-- | cartesianProduct [[1,2,3], [4,5],[6]] -> [[1,4,6],[1,5,6],[2,4,6],[2,5,6],[3,4,6],[3,5,6]]
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct = sequence

-- | Convert from license to RPM-friendly description.  The strings are
-- taken from TagsCheck.py in the rpmlint distribution.

showLicense :: License -> String
showLicense (GPL _) = "GPL"
showLicense (LGPL _) = "LGPL"
showLicense BSD3 = "BSD"
showLicense BSD4 = "BSD-like"
showLicense PublicDomain = "Public Domain"
showLicense AllRightsReserved = "Proprietary"
showLicense OtherLicense = "Non-distributable"
