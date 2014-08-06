{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
-- | Access to things that Debian policy says should be in a valid
-- control file.  The versions ending in E may throw ControlFileError
-- exceptions, the ones without return values in the EitherT
-- ControlFileError m monad.
module Debian.Control.Policy
    ( HasDebianControl(debianControl)
    , ControlFileError(..)
    -- * Functions in EitherT ControlFileError m
    , debianSourceParagraph
    , debianBinaryParagraphs
    , debianPackageParagraphs
    , debianPackageNames
    , debianSourcePackageName
    , debianBinaryPackageNames
    , debianRelations
    , debianBuildDeps
    , debianBuildDepsIndep
    , removeCommentParagraphs
    , fieldValue'
    -- * Pure functions that throw ControlFileError exceptions
    , tryEitherT
    , tryEither
    , debianSourceParagraphE
    , debianBinaryParagraphsE
    , debianPackageParagraphsE
    , debianPackageNamesE
    , debianSourcePackageNameE
    , debianBinaryPackageNamesE
    , debianRelationsE
    , debianBuildDepsE
    , debianBuildDepsIndepE
    ) where

import Control.Exception (Exception, throw)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (EitherT, hoistEither)
import Data.Typeable (Typeable)
import Data.ListLike (ListLike, toList)
import Debian.Control.Common (Control'(..), Paragraph'(..), Field'(..), fieldValue, ControlFunctions)
import Debian.Relation (SrcPkgName(..), BinPkgName(..), Relations, ParseRelations, parseRelations)
import Text.Parsec.Error (ParseError)

fieldValueE' :: ControlFunctions text => String -> Paragraph' text -> text
fieldValueE' fieldName paragraph = maybe (throw $ MissingField fieldName) id $ fieldValue fieldName paragraph

-- | Comment paragraphs are rare, but they happen.
removeCommentParagraphs :: HasDebianControl control text => control -> Control' text
removeCommentParagraphs c =
    Control (filter (not . isCommentParagraph) (unControl (debianControl c)))
    where
      isCommentParagraph (Paragraph fields) = all isCommentField fields
      isCommentField (Comment _) = True
      isCommentField _ = False

-- Functions that throw ControlFileError

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
    deriving (Show, Typeable)

instance Exception ControlFileError

instance Eq ControlFileError where
    _ == _ = False

debianPackageParagraphsE :: HasDebianControl control text => control -> (Paragraph' text, [Paragraph' text])
debianPackageParagraphsE ctl =
    case removeCommentParagraphs ctl of
      (Control [_]) -> throw NoBinaryParagraphs
      (Control []) -> throw NoParagraphs
      (Control (sourceParagraph : binParagraphs)) -> (sourceParagraph, binParagraphs)

debianSourceParagraphE :: HasDebianControl control text => control -> Paragraph' text
debianSourceParagraphE = fst . debianPackageParagraphsE

debianBinaryParagraphsE :: HasDebianControl control text => control -> [Paragraph' text]
debianBinaryParagraphsE = snd . debianPackageParagraphsE

debianPackageNamesE :: (HasDebianControl control text, ListLike text Char) => control -> (SrcPkgName, [BinPkgName])
debianPackageNamesE c =
  let (srcParagraph, binParagraphs) = debianPackageParagraphsE c in
  (mapFieldValue (SrcPkgName . toList) "Source" srcParagraph, map (mapFieldValue (BinPkgName . toList) "Package") binParagraphs)

debianSourcePackageNameE :: (HasDebianControl control text, ListLike text Char) => control -> SrcPkgName
debianSourcePackageNameE = fst . debianPackageNamesE

debianBinaryPackageNamesE :: (HasDebianControl control text, ListLike text Char) => control -> [BinPkgName]
debianBinaryPackageNamesE = snd . debianPackageNamesE

mapFieldValue :: ControlFunctions text => (text -> a) -> String -> Paragraph' text -> a
mapFieldValue f fieldName paragraph = f $ fieldValueE' fieldName paragraph

debianRelationsE :: (HasDebianControl control text, ListLike text Char, ParseRelations text) => String -> control -> Maybe Relations
debianRelationsE fieldName ctl = maybe Nothing (either (throw . ParseRelationsError) Just . parseRelations) $ fieldValue fieldName (debianSourceParagraphE ctl)

debianBuildDepsIndepE :: (HasDebianControl control text, ListLike text Char, ParseRelations text) => control -> Maybe Relations
debianBuildDepsIndepE ctl = debianRelationsE "Build-Depends-Indep" ctl

debianBuildDepsE :: (HasDebianControl control text, ListLike text Char, ParseRelations text) => control -> Maybe Relations
debianBuildDepsE ctl = debianRelationsE "Build-Depends" ctl

-- EitherT versions

tryEitherT :: (MonadCatch m, Exception e) => m b -> EitherT e m b
tryEitherT x = lift (try x) >>= hoistEither

tryEither :: (MonadCatch m, Exception e) => b -> EitherT e m b
tryEither x = tryEitherT (return x)

debianPackageParagraphs :: forall m control text. (MonadCatch m, HasDebianControl control text) => control -> EitherT ControlFileError m (Paragraph' text, [Paragraph' text])
debianPackageParagraphs = tryEitherT . return . debianPackageParagraphsE

debianSourceParagraph :: (MonadCatch m, HasDebianControl control text) => control -> EitherT ControlFileError m (Paragraph' text)
debianSourceParagraph = tryEitherT . return . debianSourceParagraphE

debianBinaryParagraphs :: (MonadCatch m, HasDebianControl control text) => control -> EitherT ControlFileError m [Paragraph' text]
debianBinaryParagraphs = tryEitherT . return . debianBinaryParagraphsE

debianPackageNames :: (MonadCatch m, HasDebianControl control text, ListLike text Char) => control -> EitherT ControlFileError m (SrcPkgName, [BinPkgName])
debianPackageNames = tryEitherT . return . debianPackageNamesE

debianSourcePackageName :: (MonadCatch m, HasDebianControl control text, ListLike text Char) => control -> EitherT ControlFileError m SrcPkgName
debianSourcePackageName = tryEitherT . return . debianSourcePackageNameE

debianBinaryPackageNames :: (MonadCatch m, HasDebianControl control text, ListLike text Char) => control -> EitherT ControlFileError m [BinPkgName]
debianBinaryPackageNames = tryEitherT . return . debianBinaryPackageNamesE

debianRelations :: (MonadCatch m, HasDebianControl control text, ListLike text Char, ParseRelations text) => String -> control -> EitherT ControlFileError m (Maybe Relations)
debianRelations fieldName ctl = tryEitherT $ return $ debianRelationsE fieldName ctl

debianBuildDepsIndep :: (MonadCatch m, HasDebianControl control text, ListLike text Char, ParseRelations text) => control -> EitherT ControlFileError m (Maybe Relations)
debianBuildDepsIndep = tryEitherT . return . debianBuildDepsIndepE

debianBuildDeps :: (MonadCatch m, HasDebianControl control text, ListLike text Char, ParseRelations text) => control -> EitherT ControlFileError m (Maybe Relations)
debianBuildDeps = tryEitherT . return . debianBuildDepsE

fieldValue' :: (MonadCatch m, ControlFunctions text) => String -> Paragraph' text -> EitherT ControlFileError m text
fieldValue' fieldName paragraph = tryEitherT $ return $ fieldValueE' fieldName paragraph
