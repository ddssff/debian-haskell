{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
module Debian.Control.ByteString
    ( Control'(..)
    , Paragraph'(..)
    , Field'(..)
    , Control
    , Paragraph
    , Field
    , ControlFunctions(..)
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

-- Standard GHC modules

import qualified Control.Exception as E
import Control.Monad.State

import Data.Char(chr,ord,toLower)
import Data.List
import Data.Word

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

import System.IO.Unsafe

import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

-- Third Party Modules

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BB
import qualified Data.ByteString.Internal as BB
import qualified Data.ByteString.Char8 as C

import Debian.Control.Common

-- Local Modules

-- import ByteStreamParser

-- * Types
{-
newtype Control = Control [Paragraph]
newtype Paragraph = Paragraph [Field]
newtype Field = Field (C.ByteString, C.ByteString)
-}

type Control = Control' C.ByteString
type Paragraph = Paragraph' C.ByteString
type Field = Field'  C.ByteString
-- * Control Parser

type ControlParser a = Parser C.ByteString a

pKey :: ControlParser C.ByteString
pKey = notEmpty $ pTakeWhile (\c -> (c /= ':') && (c /= '\n'))

pValue :: ControlParser C.ByteString
pValue = pTakeWhile2 (\a b -> not (endOfValue a b))
    where
      endOfValue :: Char -> Maybe Char -> Bool
      endOfValue '\n' Nothing = True
      endOfValue '\n' (Just ' ') = False
      endOfValue '\n' (Just '\t') = False
      endOfValue '\n' (Just '#') = False
      endOfValue '\n' _ = True
      endOfValue _ _ = False

pField :: ControlParser Field
pField =
    do k <- pKey
       _ <- pChar ':'
       v <- pValue
--       pChar '\n'
       (pChar '\n' >> return ()) <|> pEOF
       return (Field (k,v))

pComment :: ControlParser Field
pComment =
    do c1 <- pChar '#'
       text <- pTakeWhile2 (\ a b -> not (endOfComment a b))
       return . Comment $ (B.append (B.singleton . c2w $ c1) text)
    where
      endOfComment '\n' Nothing = True
      endOfComment '\n' (Just '#') = False
      endOfComment '\n' _ = True
      endOfComment _ _ = False

pParagraph :: ControlParser Paragraph
pParagraph = 
    do f <- pMany1 (pComment <|> pField)
       pSkipMany (pChar '\n')
       return (Paragraph f)

pControl :: ControlParser Control
pControl = 
    do pSkipMany (pChar '\n')
       c <- pMany pParagraph
       return (Control c)


-- parseControlFromFile :: FilePath -> IO (Either String Control)

instance ControlFunctions C.ByteString where
    parseControlFromFile fp = 
        do c <- C.readFile fp
           case parse pControl c of
             Nothing -> return (Left (newErrorMessage (Message ("Failed to parse " ++ fp)) (newPos fp 0 0)))
             (Just (cntl,_)) -> return (Right cntl)
    parseControlFromHandle sourceName handle =
        E.try (C.hGetContents handle) >>=
        either (\ (e :: E.SomeException) -> error ("parseControlFromHandle ByteString: Failure parsing " ++ sourceName ++ ": " ++ show e)) (return . parseControl sourceName)
    parseControl sourceName c =
        do case parse pControl c of
             Nothing -> Left (newErrorMessage (Message ("Failed to parse " ++ sourceName)) (newPos sourceName 0 0))
             Just (cntl,_) -> Right cntl
    lookupP fieldName (Paragraph fields) =
        let pFieldName = C.pack (map toLower fieldName) in
        find (\ (Field (fieldName',_)) -> C.map toLower fieldName' == pFieldName) fields
    -- NOTE: probably inefficient
    stripWS = C.reverse . strip . C.reverse . strip
        where strip = C.dropWhile (flip elem " \t")
    asString = C.unpack

{-
main = 
    do [fp] <- getArgs
       C.readFile fp >>= \c -> maybe (putStrLn "failed.") (print . length . fst) (parse pControl c)
-}
-- * Helper Functions

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
_takeWhile2 :: (Word8 -> Maybe Word8 -> Bool) -> B.ByteString -> B.ByteString
_takeWhile2 f ps = BB.unsafeTake (findIndex2OrEnd (\w1 w2 -> not (f w1 w2)) ps) ps
{-# INLINE _takeWhile2 #-}

break2 :: (Word8 -> Maybe Word8 -> Bool) -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
break2 p ps = case findIndex2OrEnd p ps of n -> Just (BB.unsafeTake n ps, BB.unsafeDrop n ps)

span2 :: (Word8 -> Maybe Word8 -> Bool) -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
span2 p ps = break2 (\a b -> not (p a b)) ps


-- | 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.

findIndex2OrEnd :: (Word8 -> Maybe Word8 -> Bool) -> B.ByteString -> Int
findIndex2OrEnd k (BB.PS x s l) = unsafePerformIO $ withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    go a b | a `seq` b `seq` False = undefined
    go ptr n | n >= l    = return l
             | otherwise = do w1 <- peek ptr
                              w2 <- if (n + 1 < l) then (peek (ptr `plusPtr` 1) >>= return . Just) else return Nothing
                              if k w1 w2
                                then return n
                                else go (ptr `plusPtr` 1) (n+1)


{-
findIndex2OrEnd :: (Word8 -> Maybe Word8 -> Bool) -> B.ByteString -> Int
findIndex2OrEnd k (B.PS x s l) = unsafePerformIO $ withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    go a b | a `seq` b `seq` False = undefined
    go ptr n | n >= l    = return l
             | otherwise = do w1 <- peek ptr
                              case (w2c w1) of
                                '\n' ->
                                    if (n + 1 < l)
                                    then do w2 <- peek (ptr `plusPtr` 1)
                                            case (w2c w2) of
                                              ' ' -> go (ptr `plusPtr` 2) (n + 2)
                                              _ -> return n
                                    else return l -- go (ptr `plusPtr` 1) (n + 1)
                                _ -> go (ptr `plusPtr` 1) (n + 1)
-}
{-
                              w2 <- if (n + 1 < l) then (peek (ptr `plusPtr` 1) >>= return . Just) else return Nothing
                              if k w1 w2
                                then return n
                                else go (ptr `plusPtr` 1) (n+1)
-}
{-# INLINE findIndex2OrEnd #-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
_findIndex2 :: (Word8 -> Maybe Word8 -> Bool) -> B.ByteString -> Maybe Int
_findIndex2 k (BB.PS x s l) = unsafePerformIO $ withForeignPtr x $ \f -> go (f `plusPtr` s) 0
  where
    go a b | a `seq` b `seq` False = undefined
    go ptr n | n >= l    = return Nothing
             | otherwise = do w1 <- peek ptr
                              w2 <- if (n + 1 < l) then (peek (ptr `plusPtr` 1) >>= return . Just) else return Nothing
                              if k w1 w2
                                then return (Just n)
                                else go (ptr `plusPtr` 1) (n+1)
{-# INLINE _findIndex2 #-}

-- Copied from ByteStream because they are not exported

w2c :: Word8 -> Char
w2c = chr . fromIntegral

c2w :: Char -> Word8
c2w = fromIntegral . ord

-- * Parser

data Result a
    = Ok a
    | Fail
    | Empty
      deriving Show

m2r :: Maybe a -> Result a
m2r (Just a) = Ok a
m2r Nothing = Empty            

r2m :: Result a -> Maybe a
r2m (Ok a) = Just a
r2m _ = Nothing

newtype Parser state a = Parser { unParser :: (state -> Result (a, state)) }

instance Monad (Parser state) where
    return a = Parser (\s -> Ok (a,s))
    m >>= f =
        Parser $ \state ->
            let r = (unParser m) state in
            case r of
              Ok (a,state') -> 
                  case unParser (f a) $ state' of
                    Empty -> Fail
                    o -> o
              Empty -> Empty
              Fail -> Fail

instance MonadPlus (Parser state) where
    mzero = Parser (const Empty)
    mplus (Parser p1) (Parser p2) =
        Parser (\s -> case p1 s of
                        Empty -> p2 s
                        o -> o
               )
        
--       Parser (\s -> maybe (p2 s) (Just) (p1 s))


_pSucceed :: a -> Parser state a
_pSucceed = return

_pFail :: Parser state a
_pFail = Parser (const Empty)


(<|>) :: Parser state a -> Parser state a -> Parser state a
(<|>) = mplus


satisfy :: (Char -> Bool) -> Parser C.ByteString Char
satisfy f =
    Parser $ \bs ->
        if C.null bs
        then Empty
        else let (s,ss) = (C.head bs, C.tail bs) in
             if (f s)
                then Ok (s,ss)
                else Empty

pChar :: Char -> Parser C.ByteString Char
pChar c = satisfy ((==) c)


_try :: Parser state a -> Parser state a
_try (Parser p) =
    Parser $ \bs -> case (p bs) of
                      Fail -> Empty
                      o -> o

pEOF :: Parser C.ByteString ()
pEOF =
    Parser $ \bs -> if C.null bs then Ok ((),bs) else Empty

pTakeWhile2 :: (Char -> Maybe Char -> Bool) -> Parser C.ByteString C.ByteString
pTakeWhile2 f =
    Parser $ \bs -> m2r (span2 (\w1 w2 -> f (w2c w1) (fmap w2c w2)) bs)

pTakeWhile :: (Char -> Bool) -> Parser C.ByteString C.ByteString
pTakeWhile f =
    Parser $ \bs -> Ok (B.span (\w -> f (w2c w)) bs)

_pSkipWhile :: (Char -> Bool) -> Parser C.ByteString ()
_pSkipWhile p =
    Parser $ \bs -> Ok ((), C.dropWhile p bs)

pMany ::  Parser st a -> Parser st [a]
pMany p 
    = scan id
    where
      scan f = do x <- p
                  scan (\tail -> f (x:tail))
               <|> return (f [])

notEmpty :: Parser st C.ByteString -> Parser st C.ByteString 
notEmpty (Parser p) =
    Parser $ \s -> case p s of
                     o@(Ok (a, _s)) ->
                         if C.null a
                         then Empty
                         else o
                     x -> x

pMany1 :: Parser st a -> Parser st [a]
pMany1 p =
    do x <- p
       xs <- pMany p
       return (x:xs)

pSkipMany :: Parser st a -> Parser st ()
pSkipMany p = scan
    where
      scan = (p >> scan) <|> return ()
       
_pSkipMany1 :: Parser st a -> Parser st ()
_pSkipMany1 p = p >> pSkipMany p

parse :: Parser state a -> state -> Maybe (a, state)
parse p s = r2m ((unParser p) s)
