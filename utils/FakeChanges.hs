module Main where

import Debian.Util.FakeChanges
import System.Environment
import System.Console.GetOpt
import System.FilePath
import System.Unix.FilePath (realpath)

data Flag 
    = OutputDir FilePath
       deriving Show
    
options :: [OptDescr Flag]
options =
     [ Option ['o']     ["output"]  (ReqArg OutputDir "DIRECTORY")  "output DIRECTORY"
     ]


fakeChangesOpts :: [String] -> IO ([Flag], [FilePath])
fakeChangesOpts argv = 
    case getOpt Permute options argv of
      (o,files,[]) | not (null files) -> return (o, files)
      (_,_,errs) -> 
          do h <- header
             error $ (concat errs ++ usageInfo h options)
    where header =
              do pn <- getProgName
                 return $ "\nUsage: " ++ pn ++ " [OPTION...] files..."
main =
    do args <- getArgs
       (opts, files) <- fakeChangesOpts args
       (changesFP, contents) <- fakeChanges files
       outdir <-
           case opts of
             [OutputDir dir] -> realpath dir
             _ -> return "."
       writeFile (outdir </> changesFP) $! contents

