{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception (try)
import Control.Monad
import Data.Maybe (fromMaybe)
import Debian.Apt.Methods
import Debian.Report
import Debian.Sources
import Foreign.C.Types
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.XML.HaXml
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml.Posn
import Text.PrettyPrint.HughesPJ
import Text.Read (readMaybe)
import System.IO
import System.Posix.Env


-- * main

main :: IO ()
main =
    do (sourcesAFP, sourcesBFP) <- parseArgs
       let arch     = "i386" -- not actually used for anything right now, could be when binary package list is enabled
           cacheDir = "."    -- FIXME: replace with tempdir later
       sourcesA <- liftM parseSourcesList $ readFile sourcesAFP
       sourcesB <- liftM parseSourcesList $ readFile sourcesBFP
       trumpMap <- trumped (fetch emptyFetchCallbacks []) cacheDir arch sourcesA sourcesB
       print (showXML "trump.xsl" (trumpedXML trumpMap))
    where
      showXML :: String -> CFilter Posn -> Doc
      showXML styleSheet = document . mkDocument styleSheet . cfilterToElem
      -- cliff says this is broken with regards to cdata
      cfilterToElem :: CFilter Posn -> Element Posn
      cfilterToElem f = case f (CString False "" noPos) of
                    [CElem e _] -> xmlEscape stdXmlEscaper e
                    []        -> error "RSS produced no output"
                    _         -> error "RSS produced more than one output"
      -- <?xml-stylesheet type="text/xsl" href="cdcatalog.xsl"?>
      mkDocument :: String -> Element Posn -> Document Posn
      mkDocument styleSheet elem =
          let xmlDecl = XMLDecl "1.0" (Just (EncodingDecl "utf-8")) (Just True)
              prolog   = Prolog (Just xmlDecl)  [] Nothing [PI ("xml-stylesheet","type=\"text/xsl\" href=\""++styleSheet++"\"")]
              -- symTable = []
          in
            Document prolog [] elem []

-- * command-line helper functions

helpText :: String -> Doc
helpText progName =
    (text "Usage:" <+> text progName <+> text "<old sources.list>" <+> text "<new sources.list>"$+$
     text [] $+$
     (fsep $ map text $ words $ "Find all the packages referenced by the second sources.list which trump packages find in the first sources.list.")
    )

parseArgs :: IO (String, String)
parseArgs =
    do args <- getArgs
       case args of
         [dista, distb] -> return (dista, distb)
         _ -> exitWithHelp helpText
    where
      -- |exitFailure with nicely formatted help text on stderr
      exitWithHelp :: (String -> Doc) -- ^ generate help text, the argument is the result of getProgName
                   -> IO a -- ^ no value is returned, this function always calls exitFailure
      exitWithHelp helpText =
          do progName <- getProgName
             hPutStrLn stderr =<< renderWidth (helpText progName)
             exitFailure
      -- |render a Doc using the current terminal width
      renderWidth :: Doc -> IO String
      renderWidth doc =
          do columns <- return . fromMaybe 80 =<< getWidth
             return $ renderStyle (Style PageMode columns 1.0) doc

foreign import ccall "gwinsz.h c_get_window_size" c_get_window_size :: IO CLong

-- get the number of rows and columns using ioctl (0, TIOCGWINSZ, &w)
-- @see also: getWidth
getWinSize :: IO (Int,Int)
getWinSize = do (a,b) <- (`divMod` 65536) `fmap` c_get_window_size
                return (fromIntegral b, fromIntegral a)

-- get the number of colums.
-- First tries getWinSize, if that returns 0, then try the COLUMNS
-- shell variable.
getWidth :: IO (Maybe Int)
getWidth =
    do (cols, _) <- getWinSize
       case cols of
         -- 0 -> return . fmap read =<< getEnv "COLUMNS"
         0 -> either (\(e :: IOError) -> Nothing) (fmap read) <$> try (getEnv "COLUMNS")
         _ -> return (Just cols)


