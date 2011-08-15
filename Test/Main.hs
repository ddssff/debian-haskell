module Main where

import Test.HUnit
import System.Exit
import Test.Changes
import Test.Versions
--import Test.VersionPolicy
import Test.SourcesList
import Test.Dependencies

main =
    do (c,st) <- runTestText putTextToShowS (TestList (versionTests ++ sourcesListTests ++ dependencyTests ++ changesTests))
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         n -> exitFailure
                                            
