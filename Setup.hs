#!/usr/bin/runhaskell

import Distribution.Simple
import Distribution.Simple.Program
import System.Cmd
import System.Exit

main = defaultMainWithHooks simpleUserHooks {
         runTests = runTestScript
       }

runTestScript _args _flag _pd _lbi =
    system "runhaskell Test/Main.hs" >>=
    \ code -> if code == ExitSuccess then return () else error "Test Failure"
