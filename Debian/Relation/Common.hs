module Debian.Relation.Common where

-- Standard GHC Modules

import Data.List
import Text.ParserCombinators.Parsec
import Data.Function
import Text.PrettyPrint.ANSI.Leijen (Doc, text)

-- Local Modules

import Debian.Version

-- Datatype for relations

type Relations = AndRelation
type AndRelation = [OrRelation]
type OrRelation = [Relation]

data Relation = Rel BinPkgName (Maybe VersionReq) (Maybe ArchitectureReq) deriving Eq

newtype PkgName = PkgName {unPkgName :: String} deriving (Show, Eq, Ord)

newtype SrcPkgName = SrcPkgName {unSrcPkgName :: PkgName} deriving (Show, Eq, Ord)
newtype BinPkgName = BinPkgName {unBinPkgName :: PkgName} deriving (Show, Eq, Ord)

prettySrcPkgName :: SrcPkgName -> Doc
prettySrcPkgName = prettyPkgName . unSrcPkgName

prettyBinPkgName :: BinPkgName -> Doc
prettyBinPkgName = prettyPkgName . unBinPkgName

prettyPkgName :: PkgName -> Doc
prettyPkgName = text . unPkgName

class ParseRelations a where
    -- |'parseRelations' parse a debian relation (i.e. the value of a
    -- Depends field). Return a parsec error or a value of type
    -- 'Relations'
    parseRelations :: a -> Either ParseError Relations


prettyRelation :: Relation -> Doc
prettyRelation (Rel name ver arch) =
    text (unPkgName (unBinPkgName name) ++ maybe "" (show . prettyVersionReq) ver ++ maybe "" (show . prettyArchitectureReq) arch)

instance Ord Relation where
    compare (Rel pkgName1 mVerReq1 _mArch1) (Rel pkgName2 mVerReq2 _mArch2) =
	case compare pkgName1 pkgName2 of
	     LT -> LT
	     GT -> GT
	     EQ -> compare mVerReq1 mVerReq2

data ArchitectureReq
    = ArchOnly [String]
    | ArchExcept [String]
      deriving Eq

prettyArchitectureReq :: ArchitectureReq -> Doc
prettyArchitectureReq (ArchOnly arch) = text $ " [" ++ intercalate " " arch ++ "]"
prettyArchitectureReq (ArchExcept arch) = text $ " [!" ++ intercalate " !" arch ++ "]"

data VersionReq
    = SLT DebianVersion
    | LTE DebianVersion
    | EEQ  DebianVersion
    | GRE  DebianVersion
    | SGR DebianVersion
      deriving Eq

prettyVersionReq :: VersionReq -> Doc
prettyVersionReq (SLT v) = text $ " (<< " ++ show (prettyDebianVersion v) ++ ")"
prettyVersionReq (LTE v) = text $ " (<= " ++ show (prettyDebianVersion v) ++ ")"
prettyVersionReq (EEQ v) = text $ " (= " ++ show (prettyDebianVersion v) ++ ")"
prettyVersionReq (GRE v) = text $ " (>= " ++ show (prettyDebianVersion v) ++ ")"
prettyVersionReq (SGR v) = text $ " (>> " ++ show (prettyDebianVersion v) ++ ")"

-- |The sort order is based on version number first, then on the kind of
-- relation, sorting in the order <<, <= , ==, >= , >>
instance Ord VersionReq where
    compare = compare `on` extr
      where extr (SLT v) = (v,0)
            extr (LTE v) = (v,1)
            extr (EEQ v) = (v,2)
            extr (GRE v) = (v,3)
            extr (SGR v) = (v,4)

-- |Check if a version number satisfies a version requirement.
checkVersionReq :: Maybe VersionReq -> Maybe DebianVersion -> Bool
checkVersionReq Nothing _ = True
checkVersionReq _ Nothing = False
checkVersionReq (Just (SLT v1)) (Just v2) = v2 < v1
checkVersionReq (Just (LTE v1)) (Just v2) = v2 <= v1
checkVersionReq (Just (EEQ v1)) (Just v2) = v2 == v1
checkVersionReq (Just (GRE v1)) (Just v2) = v2 >= v1
checkVersionReq (Just (SGR v1)) (Just v2) = v2 > v1
