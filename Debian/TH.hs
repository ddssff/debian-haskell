{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Debian.TH
    ( run
    , run'
    , here
    , Loc
    ) where

import Control.Exception (throw)
import Data.ByteString.Lazy.UTF8 as L hiding (fromString)
import qualified Data.ByteString.Lazy.Char8 as L
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.TH (ExpQ, Loc(..), location)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess)
import System.Process.Common (showCreateProcessForUser)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)
import Text.PrettyPrint (Doc, text)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

here :: ExpQ
here = lift =<< location

run :: ExpQ
run = [|run' $here|]

run' :: Loc -> CreateProcess -> IO L.ByteString
run' loc cp = do
  (code, out, err) <- readCreateProcessWithExitCode cp L.empty
  case code of
    ExitSuccess -> return out
    ExitFailure _ -> throw (userError (unlines
                                       [ show code
                                       , " command: " ++ showCreateProcessForUser cp
                                       , " stderr: " ++ unpack (decodeUtf8 (L.toStrict err))
                                       , " stdout: " ++ unpack (decodeUtf8 (L.toStrict out))
                                       , " location: " ++ show loc ]))

instance Pretty Loc where
    pPrint = prettyLoc

prettyLoc :: Loc -> Doc
prettyLoc (Loc _filename _package modul (line, col) _) = text (modul <> ":" ++ show line ++ ":" ++ show col)
