-- |
-- Module      :  CabalDebian
-- Copyright   :  David Fox 2008
--
-- Maintainer  :  David Fox <dsf@seereason.com>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Main entry point for generating Debianizations from Cabal data.

-- This software may be used and distributed according to the terms of
-- the GNU General Public License, incorporated herein by reference.

module Main where

import qualified Distribution.Package.Debian.Main as Debian

main :: IO ()

main = Debian.main
