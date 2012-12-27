-- |A module for working with debian relationships <http://www.debian.org/doc/debian-policy/ch-relationships.html>
module Debian.Relation
    ( -- * Types
      PkgName(..)
    , SrcPkgName(..)
    , BinPkgName(..)
    , Relations
    , AndRelation
    , prettyRelations
    , OrRelation
    , prettyOrRelation
    , Relation(..)
    , prettyRelation
    , ArchitectureReq(..)
    , VersionReq(..)
    -- * Helper Functions
    , checkVersionReq
    -- * Relation Parser
    , RelParser
    , ParseRelations(..)
    ) where 

import Debian.Relation.Common (prettyRelation, SrcPkgName(..), BinPkgName(..), PkgName(prettyPkgName, pkgNameFromString))
import Debian.Relation.String
