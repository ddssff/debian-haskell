{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
-- | Access to things that Debian policy says should be in a valid
-- control file.
module Debian.Control.Policy
    ( HasDebianControl(debianControl)
    , ControlFileError(..)
    , debianSourceParagraph
    , debianBinaryParagraphs
    , debianPackageParagraphs
    , debianPackageNames
    , debianSourcePackageName
    , debianBinaryPackageNames
    , debianBuildDeps
    , debianBuildDepsIndep
    , removeCommentParagraphs
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Trans.Either (EitherT, left, right)
import Data.ListLike (ListLike, toList)
import Debian.Control.Common (Control'(..), Paragraph'(..), Field'(..), fieldValue, ControlFunctions)
import Debian.Relation (SrcPkgName(..), BinPkgName(..), Relations, ParseRelations, parseRelations)
import Text.Parsec.Error (ParseError)

class ControlFunctions text => HasDebianControl control text | control -> text where
    debianControl :: control -> Control' text

instance ControlFunctions text => HasDebianControl (Control' text) text where
    debianControl = id

data ControlFileError
    = NoParagraphs
    | NoBinaryParagraphs
    | MissingField String
    | ParseRelationsError ParseError
    | ParseControlError String
    deriving Show

instance Eq ControlFileError where
    _ == _ = False

debianPackageParagraphs :: (Monad m, HasDebianControl control text) =>
                           control -> EitherT ControlFileError m (Paragraph' text, [Paragraph' text])
debianPackageParagraphs ctl =
    case removeCommentParagraphs ctl of
      (Control [_]) -> left NoBinaryParagraphs
      (Control []) -> left NoParagraphs
      (Control (sourceParagraph : binParagraphs)) -> right (sourceParagraph, binParagraphs)

debianSourceParagraph :: (Monad m, HasDebianControl control text) => control -> EitherT ControlFileError m (Paragraph' text)
debianSourceParagraph ctl = fst <$> debianPackageParagraphs ctl

debianBinaryParagraphs :: (Monad m, HasDebianControl control text) => control -> EitherT ControlFileError m [Paragraph' text]
debianBinaryParagraphs ctl = snd <$> debianPackageParagraphs ctl

debianPackageNames :: (Monad m, HasDebianControl control text, ListLike text Char) => control -> EitherT ControlFileError m (SrcPkgName, [BinPkgName])
debianPackageNames c = do
  (srcParagraph, binParagraphs) <- debianPackageParagraphs c
  (,) <$> mapFieldValue (SrcPkgName . toList) "Source" srcParagraph <*> mapM (mapFieldValue (BinPkgName . toList) "Package") binParagraphs

debianSourcePackageName :: (Monad m, HasDebianControl control text, ListLike text Char) => control -> EitherT ControlFileError m SrcPkgName
debianSourcePackageName ctl = fst <$> debianPackageNames ctl

debianBinaryPackageNames :: (Monad m, HasDebianControl control text, ListLike text Char) => control -> EitherT ControlFileError m [BinPkgName]
debianBinaryPackageNames ctl = snd <$> debianPackageNames ctl

mapFieldValue :: (Monad m, ControlFunctions text) => (text -> a) -> String -> Paragraph' text -> EitherT ControlFileError m a
mapFieldValue f fieldName paragraph = f <$> (fieldValue' fieldName paragraph)

debianRelations :: (Monad m, HasDebianControl control text, ListLike text Char, ParseRelations text) => String -> control -> EitherT ControlFileError m Relations
debianRelations fieldName ctl =
    debianSourceParagraph ctl >>= fieldValue' fieldName >>= either (left . ParseRelationsError) right . parseRelations
    -- either (\ e -> error $ "Error parsing Build-Depends-Indep for" ++ display (debianSourcePackageName ctl) ++ ": " ++ show e) id . parseRelations . fieldValue' fieldName . debianSourceParagraph $ ctl

debianBuildDepsIndep :: (Monad m, HasDebianControl control text, ListLike text Char, ParseRelations text) => control -> EitherT ControlFileError m Relations
debianBuildDepsIndep = debianRelations "Build-Depends-Indep"

debianBuildDeps :: (Monad m, HasDebianControl control text, ListLike text Char, ParseRelations text) => control -> EitherT ControlFileError m Relations
debianBuildDeps = debianRelations "Build-Depends"

fieldValue' :: (Monad m, ControlFunctions text) => String -> Paragraph' text -> EitherT ControlFileError m text
fieldValue' fieldName paragraph =
    maybe (left $ MissingField fieldName) right $ fieldValue fieldName paragraph

-- | Comment paragraphs are rare, but they happen.
removeCommentParagraphs :: HasDebianControl control text => control -> Control' text
removeCommentParagraphs c =
    Control (filter (not . isCommentParagraph) (unControl (debianControl c)))
    where
      isCommentParagraph (Paragraph fields) = all isCommentField fields
      isCommentField (Comment _) = True
      isCommentField _ = False
