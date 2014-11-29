{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Neovim.API.Context
Description :  The neovim context
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.Context
    where

import Control.Applicative
import Control.Monad.Reader
import Control.Concurrent.STM

data InternalEnvironment = InternalEnvironment
                         { eventQueue :: TQueue String } -- FIXME saep 2014-11-28 Actual type

newtype Neovim a = Neovim { runNeovim :: ReaderT InternalEnvironment IO a }
    deriving (Monad, Functor, Applicative, MonadReader InternalEnvironment, MonadIO)

