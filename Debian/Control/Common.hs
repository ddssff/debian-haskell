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

import Text.ParserCombinators.Parsec
import System.Exit
import System.IO
import System.Process
import Data.List

newtype Control' a
    = Control { unControl :: [Paragraph' a] }

newtype Paragraph' a
    = Paragraph [Field' a]
    deriving Eq

-- |NOTE: we do not strip the leading or trailing whitespace in the
-- name or value
data Field' a
    = Field (a, a)
    | Comment a     -- ^ Lines beginning with #
      deriving Eq

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
