{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Debian.TH
    ( here
    , Loc
    ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Language.Haskell.TH (ExpQ, Loc(..), location)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Text.PrettyPrint (Doc, text)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

here :: ExpQ
here = lift =<< location

instance Pretty Loc where
    pPrint = prettyLoc

prettyLoc :: Loc -> Doc
prettyLoc (Loc _filename _package modul (line, col) _) = text (modul <> ":" ++ show line ++ ":" ++ show col)
