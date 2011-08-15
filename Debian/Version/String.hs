{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Debian.Version.String
    ( ParseDebianVersion(..)
    ) where

import Text.ParserCombinators.Parsec

import Debian.Version.Common
import Debian.Version.Internal
    
instance ParseDebianVersion String where
    parseDebianVersion str =
        case parse parseDV str str of
          Left e -> error (show e)
          Right dv -> DebianVersion str dv
