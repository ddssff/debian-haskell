module Debian.Release
    ( ReleaseName(..)
    , parseReleaseName
    , releaseName'
    , Arch(..)
    , archName
    , Section(..)
    , SubSection(..)
    , sectionName
    , sectionName'
    , sectionNameOfSubSection
    , parseSection
    , parseSection'
    ) where

import Debian.URI

-- |A distribution (aka release) name.  This type is expected to refer
-- to a subdirectory of the dists directory which is at the top level
-- of a repository.
data ReleaseName = ReleaseName { relName :: String } deriving (Eq, Ord, Read, Show)

parseReleaseName :: String -> ReleaseName
parseReleaseName name = ReleaseName {relName = unEscapeString name}

releaseName' :: ReleaseName -> String
releaseName' (ReleaseName {relName = s}) = escapeURIString isAllowedInURI s

-- |The types of architecture that a package can have, either Source
-- or some type of binary architecture.
data Arch = Source | Binary String deriving (Read, Show, Eq, Ord)

archName :: Arch -> String
archName Source = "source"
archName (Binary arch) = arch

-- |A section of a repository such as main, contrib, non-free,
-- restricted.  The indexes for a section are located below the
-- distribution directory.
newtype Section = Section String deriving (Read, Show, Eq, Ord)

-- |A package's subsection is only evident in its control information,
-- packages from different subsections all reside in the same index.
data SubSection = SubSection { section :: Section, subSectionName :: String } deriving (Read, Show, Eq, Ord)

sectionName :: SubSection -> String
sectionName (SubSection (Section "main") y) = y
sectionName (SubSection x y) = sectionName' x ++ "/" ++ y

sectionName' :: Section -> String
sectionName' (Section s) = escapeURIString isAllowedInURI s

sectionNameOfSubSection :: SubSection -> String
sectionNameOfSubSection = sectionName' . section

-- |Parse the value that appears in the @Section@ field of a .changes file.
-- (Does this need to be unesacped?)
parseSection :: String -> SubSection
parseSection s =
    case span (/= '/') s of
      (x, "") -> SubSection (Section "main") x
      ("main", y) -> SubSection (Section "main") y
      (x, y) -> SubSection (Section x) (tail y)

parseSection' :: String -> Section
parseSection' name =
    Section (unEscapeString name)
