#!/usr/bin/runhaskell

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.Program
import System.Directory
import System.Exit
import System.FilePath ((</>))
import System.Process

main = copyFile "debian/changelog" "changelog" >> defaultMainWithHooks simpleUserHooks
