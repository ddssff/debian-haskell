{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Debian.Control.String
    ( -- * Types
      Control'(..)
    , Paragraph'(..)
    , Field'(..)
    , Control
    , Paragraph
    , Field
    , ControlParser
    , ControlFunctions(..)
    -- * Control File Parser
    , pControl
    -- * Helper Functions
    , mergeControls
    , fieldValue
    , removeField
    , prependFields
    , appendFields
    , renameField
    , modifyField
    , raiseFields
    ) where

import qualified Control.Exception as E    
import Data.Char (toLower)
import Data.List
import Text.ParserCombinators.Parsec
import System.IO
import Debian.Control.Common

-- |This may have bad performance issues 
instance Show (Control' String) where
    show (Control paragraph) = intercalate "\n" (map show paragraph)

instance Show (Paragraph' String) where
    show (Paragraph fields) = unlines (map show fields)

instance Show (Field' String) where
    show (Field (name,value)) = name ++":"++ value
    show (Comment text) = text

type Field = Field' String
type Control = Control' String
type Paragraph = Paragraph' String

-- * ControlFunctions

instance ControlFunctions String where
    parseControlFromFile filepath = 
        parseFromFile pControl filepath 
    parseControlFromHandle sourceName handle = 
        E.try (hGetContents handle) >>=
        either (\ (e :: E.SomeException) -> error ("parseControlFromHandle String: Failure parsing " ++ sourceName ++ ": " ++ show e)) (return . parseControl sourceName)
    parseControl sourceName c = 
        parse pControl sourceName c
    lookupP fieldName (Paragraph paragraph) = 
        find (hasFieldName (map toLower fieldName)) paragraph
        where hasFieldName name (Field (fieldName',_)) = name == map toLower fieldName'
              hasFieldName _ _ = False
    stripWS = reverse . strip . reverse . strip
        where strip = dropWhile (flip elem " \t")
    asString = id

-- * Control File Parser

type ControlParser a = CharParser () a

-- |A parser for debian control file. This parser handles control files
-- that end without a newline as well as ones that have several blank
-- lines at the end. It is very liberal and does not attempt validate
-- the fields in any way. All trailing, leading, and folded whitespace
-- is preserved in the field values. See 'stripWS'.
pControl :: ControlParser Control
pControl =
    do many $ char '\n'
       sepEndBy pParagraph pBlanks >>= return . Control

pParagraph :: ControlParser Paragraph
pParagraph = many1 (pComment <|> pField) >>= return . Paragraph

-- |We are liberal in that we allow *any* field to have folded white
-- space, even though the specific restricts that to a few fields.
pField :: ControlParser Field
pField =
    do c1 <- noneOf "#\n"
       fieldName <-  many1 $ noneOf ":\n"
       char ':'
       fieldValue <- many fcharfws
       (char '\n' >> return ()) <|> eof
       return $ Field (c1 : fieldName, fieldValue)

pComment :: ControlParser Field
pComment =
    do char '#'
       text <- many (satisfy (not . ((==) '\n')))
       char '\n'
       return $ Comment ("#" ++ text ++ "\n")

fcharfws :: ControlParser Char
fcharfws = fchar <|> (try $ lookAhead (string "\n ") >> char '\n') <|> (try $ lookAhead (string "\n\t") >> char '\n') <|> (try $ lookAhead (string "\n#") >> char '\n')

fchar :: ControlParser Char
fchar = satisfy (/='\n')

_fws :: ControlParser String
_fws =
    try $ do char '\n'
             ws <- many1 (char ' ')
             c <- many1 (satisfy (not . ((==) '\n')))
             return $ '\n' : (ws ++ c)

-- |We go with the assumption that 'blank lines' mean lines that
-- consist of entirely of zero or more whitespace characters.
pBlanks :: ControlParser String
pBlanks = many1 (oneOf " \n")
