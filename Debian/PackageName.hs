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

sourcePackageName :: Cabal.PackageName -> PackageName
sourcePackageName p = PackageName $ "haskell-" ++ (unPackageName . basePackageName $ p)

docPackageName :: Cabal.PackageName -> PackageName
docPackageName p = PackageName $ "libghc-" ++ (unPackageName . basePackageName $ p) ++ "-doc"

devPackageName :: Cabal.PackageName -> PackageName
devPackageName p = PackageName $ "libghc-" ++ (unPackageName . basePackageName $ p) ++ "-dev"

profPackageName :: Cabal.PackageName -> PackageName
profPackageName p = PackageName $ "libghc-" ++ (unPackageName . basePackageName $ p) ++ "-prof"

utilsPackageName :: Cabal.PackageName -> PackageName
utilsPackageName p = PackageName $ "haskell-" ++ (unPackageName . basePackageName $ p) ++ "-utils"

basePackageName :: Cabal.PackageName -> PackageName
basePackageName (Cabal.PackageName p) = PackageName $ map (fixChars . toLower) p

fixChars :: Char -> Char
fixChars '_' = '-'
fixChars c = c
