{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
module Debian.Pretty
    ( PP(PP, unPP)
    ) where

import Data.Text (Text, unpack)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

-- | This type is wrapped around values before we pretty print them so
-- we can write our own Pretty instances for common types without
-- polluting the name space of clients of this package with instances
-- they don't want.
newtype PP a = PP {unPP :: a}

instance Pretty (PP Text) where
    pPrint = text . unpack . unPP

instance Pretty (PP String) where
    pPrint = text . unPP
