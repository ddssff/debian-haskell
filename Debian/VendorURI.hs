{-# LANGUAGE TemplateHaskell #-}

module Debian.VendorURI
    ( VendorURI(..)
    , vendorURI
    , parseVendorURI
    ) where

import Control.Lens (makeLenses, review)
import Debian.URI (parseURI, URI(uriPath))
import Language.Haskell.TH.Syntax (Loc)
import System.FilePath (splitDirectories)

newtype VendorURI = VendorURI {_vendorURI :: URI} deriving (Eq, Ord, Show)

$(makeLenses ''VendorURI)

parseVendorURI :: Loc -> String -> Maybe VendorURI
parseVendorURI loc s =
    case parseURI s of
      Nothing -> Nothing
      Just u -> case splitDirectories (uriPath u) of
                  ["/", _vendor] -> Just (review vendorURI u)
                  ["/", "hvr", "ghc", "ubuntu"] -> Just (review vendorURI u)
                  ["/", "srv", "deb", "ubuntu"] -> Just (review vendorURI u)
                  ["/", "srv", "deb86", "ubuntu"] -> Just (review vendorURI u)
                  ["/", "srv", "deb-private", "ubuntu"] -> Just (review vendorURI u)
                  ["/", "srv", "deb86-private", "ubuntu"] -> Just (review vendorURI u)
                  ["/", "work", "localpool"] -> Just (review vendorURI u)
                  _ -> error $ "parseVendorURI " ++ show loc ++ " - bad VendorURI path: " ++ show (uriPath u)

-- toURI' :: VendorURI -> URI'
-- toURI' = URI' . show . view vendorURI
