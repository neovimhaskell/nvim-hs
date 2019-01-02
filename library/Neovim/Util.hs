{- |
Module      :  Neovim.Util
Description :  Utility functions
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Util (
    whenM,
    unlessM,
    oneLineErrorMessage,
    ) where

import           Control.Monad       (when, unless)
import           Neovim.Context
import qualified Data.Text as T


-- | 'when' with a monadic predicate.
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mp a = mp >>= \p -> when p a


-- | 'unless' with a monadic predicate.
unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM mp a = mp >>= \p -> unless p a


oneLineErrorMessage :: Doc AnsiStyle -> T.Text
oneLineErrorMessage d = case T.lines $ docToText d of
    (x:_) -> x
    []    -> mempty
