{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Debian.Relation.Common where

-- Standard GHC Modules

import Data.List
import Data.Monoid (mconcat)
import Data.Function
import Text.ParserCombinators.Parsec
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), Doc, text, empty, (<>))

-- Local Modules

import Debian.Version

-- Datatype for relations

type Relations = AndRelation
type AndRelation = [OrRelation]
type OrRelation = [Relation]

data Relation = Rel BinPkgName (Maybe VersionReq) (Maybe ArchitectureReq) deriving (Eq, Show)

newtype SrcPkgName = SrcPkgName {unSrcPkgName :: String} deriving (Show, Eq, Ord)
newtype BinPkgName = BinPkgName {unBinPkgName :: String} deriving (Show, Eq, Ord)

class Pretty a => PkgName a where
    pkgNameFromString :: String -> a

instance PkgName BinPkgName where
    pkgNameFromString = BinPkgName

instance PkgName SrcPkgName where
    pkgNameFromString = SrcPkgName

class ParseRelations a where
    -- |'parseRelations' parse a debian relation (i.e. the value of a
    -- Depends field). Return a parsec error or a value of type
    -- 'Relations'
    parseRelations :: a -> Either ParseError Relations

-- | This needs to be indented for use in a control file: intercalate "\n     " . lines . show
prettyRelations :: [[Relation]] -> Doc
prettyRelations xss = mconcat . intersperse (text "\n, ") . map prettyOrRelation $ xss

prettyOrRelation :: [Relation] -> Doc
prettyOrRelation xs = mconcat . intersperse (text " | ") . map prettyRelation $ xs

prettyRelation :: Relation -> Doc
prettyRelation (Rel name ver arch) =
    pretty name <> maybe empty prettyVersionReq ver <> maybe empty prettyArchitectureReq arch

instance Ord Relation where
    compare (Rel pkgName1 mVerReq1 _mArch1) (Rel pkgName2 mVerReq2 _mArch2) =
	case compare pkgName1 pkgName2 of
	     LT -> LT
	     GT -> GT
	     EQ -> compare mVerReq1 mVerReq2

data ArchSpec
    = ArchOnly (Maybe ArchOS) ArchCPU
    | ArchExcept  (Maybe ArchOS) ArchCPU
    deriving (Eq, Show)

prettyArchSpec :: ArchSpec -> Doc
prettyArchSpec (ArchOnly Nothing cpu) = prettyArchCPU cpu
prettyArchSpec (ArchOnly (Just os) cpu) = prettyArchOS os <> text "-" <> prettyArchCPU cpu
prettyArchSpec (ArchExcept os cpu) = text "!" <> prettyArchSpec (ArchOnly os cpu)

data ArchOS = ArchOS String | ArchOSAny deriving (Eq, Show)

prettyArchOS :: ArchOS -> Doc
prettyArchOS (ArchOS s) = text s
prettyArchOS ArchOSAny = text "any"

data ArchCPU = ArchCPU String | ArchCPUAny deriving (Eq, Show)

prettyArchCPU :: ArchCPU -> Doc
prettyArchCPU (ArchCPU s) = text s
prettyArchCPU ArchCPUAny = text "any"

newtype ArchitectureReq
    = ArchitectureReq [ArchSpec]
    deriving (Eq, Show)

prettyArchitectureReq :: ArchitectureReq -> Doc
prettyArchitectureReq (ArchitectureReq arch) = text " [" <> mconcat (map prettyArchSpec arch) <> text "]"

data VersionReq
    = SLT DebianVersion
    | LTE DebianVersion
    | EEQ  DebianVersion
    | GRE  DebianVersion
    | SGR DebianVersion
      deriving (Eq, Show)

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

instance Pretty BinPkgName where
    pretty = text . unBinPkgName

instance Pretty SrcPkgName where
    pretty = text . unSrcPkgName

-- Unfortunately, the ansi-wl-pprint package has an instance @Pretty a
-- => Pretty [a]@, so we can't create an instance for a list of one
-- particular type.

-- instance Pretty Relations where
--     pretty = prettyRelations
--
-- instance Pretty OrRelation where
--     pretty = prettyOrRelation

instance Pretty Relation where
    pretty = prettyRelation

instance Pretty VersionReq where
    pretty = prettyVersionReq

instance Pretty ArchitectureReq where
    pretty = prettyArchitectureReq