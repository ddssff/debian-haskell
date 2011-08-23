-- |This is intended to help enforce the requirements on debian package names when building
-- them from cabal package names - no upper case, no underscores, etc.
module Debian.PackageName
    ( PackageName(..)
    , sourcePackageName
    , docPackageName
    , devPackageName
    , profPackageName
    , utilsPackageName
    , basePackageName
    ) where

import Data.Char (toLower)
import qualified Distribution.Package as Cabal (PackageName(..))

newtype PackageName = PackageName {unPackageName :: String}

sourcePackageName :: Maybe String -> Cabal.PackageName -> PackageName
sourcePackageName base p = PackageName $ "haskell-" ++ (unPackageName . basePackageName base $ p)

docPackageName :: Maybe String -> Cabal.PackageName -> PackageName
docPackageName base p = PackageName $ "libghc-" ++ (unPackageName . basePackageName base $ p) ++ "-doc"

devPackageName :: Maybe String -> Cabal.PackageName -> PackageName
devPackageName base p = PackageName $ "libghc-" ++ (unPackageName . basePackageName base $ p) ++ "-dev"

profPackageName :: Maybe String -> Cabal.PackageName -> PackageName
profPackageName base p = PackageName $ "libghc-" ++ (unPackageName . basePackageName base $ p) ++ "-prof"

utilsPackageName :: Maybe String -> Cabal.PackageName -> PackageName
utilsPackageName base p = PackageName $ "haskell-" ++ (unPackageName . basePackageName base $ p) ++ "-utils"

basePackageName :: Maybe String -> Cabal.PackageName -> PackageName
basePackageName base (Cabal.PackageName p) = PackageName $ maybe (map (fixChars . toLower) p) id base

fixChars :: Char -> Char
fixChars '_' = '-'
fixChars c = c
