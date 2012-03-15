-- |A module for working with debian relationships <http://www.debian.org/doc/debian-policy/ch-relationships.html>
module Debian.Relation
    ( -- * Types
      PkgName(..)
    , SrcPkgName(..)
    , BinPkgName(..)
    , prettyPkgName
    , prettySrcPkgName
    , prettyBinPkgName
    , AndRelation
    , OrRelation
    , Relations
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

import Debian.Relation.Common (prettyRelation, SrcPkgName(..), BinPkgName(..), PkgName(..), prettyPkgName, prettySrcPkgName, prettyBinPkgName)
import Debian.Relation.String
