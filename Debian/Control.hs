-- |A module for working with Debian control files <http://www.debian.org/doc/debian-policy/ch-controlfields.html>
module Debian.Control 
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
    , packParagraph
    , packField
    , formatControl
    , formatParagraph
    , formatField
    ) where

--import Control.Monad
--import Data.List
--import Text.ParserCombinators.Parsec
--import System.IO
import Debian.Control.String
import Data.List
import qualified Debian.Control.ByteString as B
import qualified Debian.Control.String as S
import qualified Data.ByteString.Char8 as B

packParagraph :: S.Paragraph -> B.Paragraph
packParagraph (S.Paragraph s) = B.Paragraph (map packField s)
packField (S.Field (name, value)) = B.Field (B.pack name, B.pack value)
packField (S.Comment s) = B.Comment (B.pack s)

formatControl (B.Control paragraphs) = intersperse (B.pack "\n") . map formatParagraph $ paragraphs
formatParagraph (B.Paragraph fields) = B.concat . map formatField $ fields
formatField (B.Field (name, value)) = B.concat [name, B.pack ":", value, B.pack "\n"]
formatField (B.Comment s) = s
