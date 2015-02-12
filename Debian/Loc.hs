{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
module Debian.Loc
    ( __LOC__
    , mapExn
    ) where

import Control.Exception (Exception, throw)
import Control.Monad.Catch (MonadCatch, catch)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax

__LOC__ :: Q Exp
__LOC__ = lift =<< location

mapExn :: forall e m a. (MonadCatch m, Exception e) => m a -> (e -> e) -> m a
mapExn task f = task `catch` (\ (e :: e) -> throw (f e))
