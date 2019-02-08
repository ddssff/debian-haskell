{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Debian.TH
    ( here
    , Loc
    ) where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Distribution.Pretty (Pretty(..), {-prettyShow,-} showFreeText)
import Language.Haskell.TH (ExpQ, Loc(..), location)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
--import Text.PrettyPrint (Doc, text)
import Text.PrettyPrint.HughesPJClass (Doc)

here :: ExpQ
here = lift =<< location

instance Pretty Loc where
    pretty = prettyLoc

prettyLoc :: Loc -> Doc
prettyLoc (Loc _filename _package modul (line, col) _) = showFreeText (modul <> ":" ++ show line ++ ":" ++ show col)
