{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
module Debian.Control.Text
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
import Data.List (find)
import Data.Monoid ((<>))
import qualified Data.Text as T (Text, cons, pack, unpack, map, strip, reverse)
import Data.Text.IO as T (hGetContents, readFile)
import Text.Parsec (try)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Text (Parser)
import Text.Parsec.Prim (Parsec, runP)
import Text.ParserCombinators.Parsec (parse, sepEndBy, satisfy, oneOf, string, lookAhead, {-try,-} many, many1, (<|>), noneOf, char, eof)
import Debian.Control.Common (ControlFunctions(parseControlFromFile, parseControlFromHandle, parseControl, lookupP, stripWS, asString),
                              Control'(Control), Paragraph'(Paragraph), Field'(Field, Comment),
                              mergeControls, fieldValue, removeField, prependFields, appendFields,
                              renameField, modifyField, raiseFields)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), text, vcat, empty)

-- | @parseFromFile p filePath@ runs a string parser @p@ on the
-- input read from @filePath@ using 'Prelude.readFile'. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile numbers "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- T.readFile fname
         return (runP p () fname input)

-- |This may have bad performance issues (why?)
instance Pretty (Control' T.Text) where
    pretty (Control paragraphs) = vcat (map (\ p -> pretty p) paragraphs)
instance Pretty (Paragraph' T.Text) where
    pretty (Paragraph fields) = vcat (map pretty fields ++ [empty])

instance Pretty (Field' T.Text) where
    pretty (Field (name,value)) = text . T.unpack $ name <>":"<> value
    pretty (Comment s) = text (T.unpack s)

type Field = Field' T.Text
type Control = Control' T.Text
type Paragraph = Paragraph' T.Text

-- * ControlFunctions

instance ControlFunctions T.Text where
    parseControlFromFile filepath =
        parseFromFile pControl filepath
    parseControlFromHandle sourceName handle =
        E.try (hGetContents handle) >>=
        either (\ (e :: E.SomeException) -> error ("parseControlFromHandle String: Failure parsing " ++ sourceName ++ ": " ++ show e)) (return . parseControl sourceName)
    parseControl sourceName c =
        parse pControl sourceName c
    lookupP fieldName (Paragraph paragraph) =
        find (hasFieldName (map toLower fieldName)) paragraph
        where hasFieldName :: String -> Field' T.Text -> Bool
              hasFieldName name (Field (fieldName',_)) = T.pack name == T.map toLower fieldName'
              hasFieldName _ _ = False
    stripWS = T.reverse . T.strip . T.reverse . T.strip
    asString = T.unpack

-- * Control File Parser

-- type ControlParser = GenParser T.Text
type ControlParser a = Parsec T.Text () a

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
       return $ Field (T.cons c1 (T.pack fieldName), T.pack fieldValue)

pComment :: ControlParser Field
pComment =
    do char '#'
       text <- many (satisfy (not . ((==) '\n')))
       char '\n'
       return $ Comment (T.pack ("#" <> text <> "\n"))

fcharfws :: ControlParser Char
fcharfws = fchar <|> (try $ lookAhead (string "\n ") >> char '\n') <|> (try $ lookAhead (string "\n\t") >> char '\n') <|> (try $ lookAhead (string "\n#") >> char '\n')

fchar :: ControlParser Char
fchar = satisfy (/='\n')

_fws :: ControlParser T.Text
_fws =
    try $ do char '\n'
             ws <- many1 (char ' ')
             c <- many1 (satisfy (not . ((==) '\n')))
             return $ T.cons '\n' (T.pack ws <> T.pack c)

-- |We go with the assumption that 'blank lines' mean lines that
-- consist of entirely of zero or more whitespace characters.
pBlanks :: ControlParser T.Text
pBlanks =
    do s <- many1 (oneOf " \n")
       return . T.pack $ s
