{-# LANGUAGE CPP, FlexibleContexts, FunctionalDependencies, MultiParamTypeClasses,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
-- | Access to things that Debian policy says should be in a valid
-- control file.
module Debian.Control.Policy
    ( HasDebianControl(debianControl)
    , debianSourceParagraph
    , debianBinaryParagraphs
    , debianPackageParagraphs
    , debianPackageNames
    , debianSourcePackageName
    , debianBinaryPackageNames
    , removeCommentParagraphs
    ) where

import Data.ListLike (ListLike, toList)
import Debian.Relation (SrcPkgName(..), BinPkgName(..))
import Debian.Control.Common (Control'(..), Paragraph'(..), Field'(..), fieldValue, ControlFunctions)

class ControlFunctions text => HasDebianControl control text | control -> text where
    debianControl :: control -> Control' text

debianPackageParagraphs :: HasDebianControl control text =>
                           control -> (Paragraph' text, [Paragraph' text])
debianPackageParagraphs c =
    case removeCommentParagraphs c of
      (Control [_]) -> error "Target control information missing"
      (Control []) -> error "Target control information missing"
      (Control (sourceParagraph : binParagraphs)) -> (sourceParagraph, binParagraphs)

debianSourceParagraph :: HasDebianControl control text => control -> Paragraph' text
debianSourceParagraph = fst . debianPackageParagraphs

debianBinaryParagraphs :: HasDebianControl control text => control -> [Paragraph' text]
debianBinaryParagraphs = snd . debianPackageParagraphs

debianPackageNames :: (HasDebianControl control text, ListLike text Char) => control -> (SrcPkgName, [BinPkgName])
debianPackageNames c =
    ((mapFieldValue (SrcPkgName . toList) "Source" srcParagraph),
     map (mapFieldValue (BinPkgName . toList) "Package") binParagraphs)
    where
      (srcParagraph, binParagraphs) = debianPackageParagraphs c

debianSourcePackageName :: (HasDebianControl control text, ListLike text Char) => control -> SrcPkgName
debianSourcePackageName = fst . debianPackageNames

debianBinaryPackageNames ::(HasDebianControl control text, ListLike text Char) => control -> [BinPkgName]
debianBinaryPackageNames = snd . debianPackageNames

mapFieldValue :: ControlFunctions text => (text -> a) -> String -> Paragraph' text -> a
mapFieldValue f fieldName paragraph = f $ fieldValue' fieldName $ paragraph

fieldValue' :: ControlFunctions text => String -> Paragraph' text -> text
fieldValue' fieldName paragraph =
    maybe (error "Missing Source field") id $ fieldValue fieldName paragraph

-- | Comment paragraphs are rare, but they happen.
removeCommentParagraphs :: HasDebianControl control text => control -> Control' text
removeCommentParagraphs c =
    Control (filter (not . isCommentParagraph) (unControl (debianControl c)))
    where
      isCommentParagraph (Paragraph fields) = all isCommentField fields
      isCommentField (Comment _) = True
      isCommentField _ = False
