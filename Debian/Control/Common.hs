{-# LANGUAGE OverloadedStrings #-}
module Debian.Control.Common
    ( -- * Types
      Control'(..)
    , Paragraph'(..)
    , Field'(..)
    , ControlFunctions(..)
    , mergeControls
    , fieldValue
    , removeField
    , prependFields
    , appendFields
    , renameField
    , modifyField
    , raiseFields
    , parseControlFromCmd
    , md5sumField
    )
    where

import Data.List (partition, intersperse)
import Data.Monoid ((<>))
import Debian.Pretty (Doc, text, Pretty(pretty), cat, vcat)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (Handle)
import System.Process (runInteractiveCommand, waitForProcess)
import Text.ParserCombinators.Parsec (ParseError)

newtype Control' a
    = Control { unControl :: [Paragraph' a] } deriving (Eq, Ord, Read, Show)

newtype Paragraph' a
    = Paragraph [Field' a]
    deriving (Eq, Ord, Read, Show)

-- |NOTE: we do not strip the leading or trailing whitespace in the
-- name or value
data Field' a
    = Field (a, a)
    | Comment a     -- ^ Lines beginning with #
      deriving (Eq, Ord, Read, Show)

class ControlFunctions a where
    -- |'parseControlFromFile' @filepath@ is a simple wrapper function
    -- that parses @filepath@ using 'pControl'
    parseControlFromFile :: FilePath -> IO (Either ParseError (Control' a))
    -- |'parseControlFromHandle' @sourceName@ @handle@ - @sourceName@ is only used for error reporting
    parseControlFromHandle :: String -> Handle -> IO (Either ParseError (Control' a))
    -- |'parseControlFromString' @sourceName@ @text@ - @sourceName@ is only used for error reporting
    parseControl :: String -> a -> (Either ParseError (Control' a))
    -- | 'lookupP' @fieldName paragraph@ looks up a 'Field' in a 'Paragraph'.
    -- @N.B.@ trailing and leading whitespace is /not/ stripped.
    lookupP :: String -> (Paragraph' a) -> Maybe (Field' a)
    -- |Strip the trailing and leading space and tab characters from a
    -- string. Folded whitespace is /not/ unfolded. This should probably
    -- be moved to someplace more general purpose.
    stripWS :: a -> a
    asString :: a -> String

-- |This may have bad performance issues (why?)
instance Pretty a => Pretty (Control' a) where
    pretty = ppControl
    -- pretty (Control paragraphs) = vcat (map pretty paragraphs)
instance Pretty a => Pretty (Paragraph' a) where
    pretty = ppParagraph
    -- pretty (Paragraph fields) = vcat (map pretty fields ++ [empty])

instance Pretty a => Pretty (Field' a) where
    pretty = ppField
    -- pretty (Field (name,value)) = pretty name <> text ":" <> pretty value
    -- pretty (Comment s) = pretty s

ppControl :: (Pretty a) => Control' a -> Doc
ppControl (Control paragraph) =
    cat (intersperse (text "\n") (map ppParagraph paragraph))

ppParagraph :: (Pretty a) => Paragraph' a -> Doc
ppParagraph (Paragraph fields) =
    vcat (map ppField fields)

ppField :: (Pretty a) => Field' a -> Doc
ppField (Field (n,v)) = pretty n <> text ":" <> pretty v
ppField (Comment c) = pretty c

mergeControls :: [Control' a] -> Control' a
mergeControls controls =
    Control (concatMap unControl controls)

fieldValue :: (ControlFunctions a) => String -> Paragraph' a -> Maybe a
fieldValue fieldName paragraph =
    case lookupP fieldName paragraph of
      Just (Field (_, val)) -> Just $ stripWS val
      _ -> Nothing

removeField :: (Eq a) => a -> Paragraph' a -> Paragraph' a
removeField toRemove (Paragraph fields) =
    Paragraph (filter remove fields)
    where
      remove (Field (name,_)) = name == toRemove
      remove (Comment _) = False

prependFields :: [Field' a] -> Paragraph' a -> Paragraph' a
prependFields newfields (Paragraph fields) = Paragraph (newfields ++ fields)

appendFields :: [Field' a] -> Paragraph' a -> Paragraph' a
appendFields newfields (Paragraph fields) = Paragraph (fields ++ newfields)

renameField :: (Eq a) => a -> a -> Paragraph' a -> Paragraph' a
renameField oldname newname (Paragraph fields) =
    Paragraph (map rename fields)
    where
      rename (Field (name, value)) | name == oldname = Field (newname, value)
      rename field = field

modifyField :: (Eq a) => a -> (a -> a) -> Paragraph' a -> Paragraph' a
modifyField name f (Paragraph fields) =
    Paragraph (map modify fields)
    where
      modify (Field (name', value)) | name' == name = Field (name, f value)
      modify field = field

-- | Move selected fields to the beginning of a paragraph.
raiseFields :: (Eq a) => (a -> Bool) -> Paragraph' a -> Paragraph' a
raiseFields f (Paragraph fields) =
    let (a, b) = partition f' fields in Paragraph (a ++ b)
    where f' (Field (name, _)) = f name
          f' (Comment _) = False

-- | Run a command and parse its output as a control file.
parseControlFromCmd :: ControlFunctions a => String -> IO (Either String (Control' a))
parseControlFromCmd cmd =
    do
      (_, outh, _, handle) <- runInteractiveCommand cmd
      result <- parseControlFromHandle cmd outh
      either (return . Left . show) (finish handle) result
    where
      finish handle control = 
          do
            exitCode <- waitForProcess handle
            case exitCode of
              ExitSuccess -> return $ Right control
              ExitFailure n -> return $ Left ("Failure: " ++ cmd ++ " -> " ++ show n)

-- |look up the md5sum file in a paragraph
-- Tries several different variations:
--  MD5Sum:
--  Md5Sum:
--  MD5sum:
md5sumField :: (ControlFunctions a) => Paragraph' a -> Maybe a
md5sumField p =
    case fieldValue "MD5Sum" p of
      m@(Just _) -> m
      Nothing -> 
          case fieldValue "Md5Sum" p of
            m@(Just _) -> m
            Nothing -> fieldValue "MD5sum" p
