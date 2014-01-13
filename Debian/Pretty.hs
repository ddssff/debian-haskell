{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
module Debian.Pretty
    ( Doc
    , text
    , empty
    , cat
    , vcat
    , Pretty(pretty)
    , render
    , (<>)
    ) where

-- import qualified Data.ByteString.Char8 as C
import Data.List (intersperse)
import Data.Monoid (Monoid(mempty, mappend), (<>))
import Data.Text (Text, pack, unpack)

data Doc = Doc {unDoc :: Text}

instance Monoid Doc where
    mempty = Doc mempty
    mappend (Doc a) (Doc b) = Doc (a <> b)

empty :: Doc
empty = Doc mempty

text :: Text -> Doc
text = Doc

cat :: [Doc] -> Doc
cat = foldl (<>) empty

vcat :: [Doc] -> Doc
vcat xs = cat (intersperse (Doc "\n") xs) <> Doc "\n"

class Pretty a where
    pretty :: a -> Doc

instance Pretty String where
    pretty = text . pack

instance Pretty Text where
    pretty = text

-- instance ToText C.ByteString where
--     totext = text . C.unpack

render :: Doc -> Text
render = unDoc

-- I'm keeping this for backwards compatibility, though it doesn't seem like
-- a proper use of the Show class to me.
instance Show Doc where
    show = unpack . render
