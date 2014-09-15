{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
module Debian.Pretty
    ( PP(PP, unPP)
    , display
    , display'
    , ppPrint
    , ppDisplay
    , ppDisplay'
    ) where

import Data.Text (Text, unpack, pack)
import Text.PrettyPrint.HughesPJClass (Doc, Pretty(pPrint), text)

-- | This type is wrapped around values before we pretty print them so
-- we can write our own Pretty instances for common types without
-- polluting the name space of clients of this package with instances
-- they don't want.
newtype PP a = PP {unPP :: a} deriving (Functor)

instance Pretty (PP Text) where
    pPrint = text . unpack . unPP

instance Pretty (PP String) where
    pPrint = text . unPP

display :: Pretty a => a -> String
display = show . pPrint

display' :: Pretty a => a -> Text
display' = pack .show . pPrint

ppPrint :: Pretty (PP a) => a -> Doc
ppPrint = pPrint . PP

ppDisplay :: Pretty (PP a) => a -> String
ppDisplay = display . PP

ppDisplay' :: Pretty (PP a) => a -> Text
ppDisplay' = pack . display . PP
