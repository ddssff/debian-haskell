-- |A module for working with debian relationships <http://www.debian.org/doc/debian-policy/ch-relationships.html>
module Debian.Relation.ByteString
    ( -- * Types
      PkgName
    , AndRelation
    , OrRelation
    , Relations
    , Relation(..)
    , ArchitectureReq(..)
    , VersionReq(..)
    -- * Helper Functions
    , checkVersionReq
    -- * Relation Parser
    , RelParser
    , ParseRelations(..)
    ) where 

-- Standard GHC Modules

import Data.List
import Text.ParserCombinators.Parsec

-- 3rd Party Modules

import qualified Data.ByteString.Char8 as C

-- Local Modules

--import Debian.Relation.Common
import Debian.Relation.String
--import Debian.Version

-- * ParseRelations

-- For now we just wrap the string version
instance ParseRelations C.ByteString where
    parseRelations byteStr = 
        let str = C.unpack byteStr in
        case parse pRelations str str of
          Right relations -> Right (filter (/= []) relations)
          x -> x
