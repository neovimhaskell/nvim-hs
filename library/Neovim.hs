{- |
Module      :  NVim
Description :  Prelude for neovim
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

This module should export all modules and functions which are generally
useful for plugin developement for neovim.
-}
module Neovim (
    module Neovim.Classes,
    module Neovim.Context,
    module Neovim.API.String,
    module Neovim.API.TH,
    module Neovim.Config,
    module Neovim.Main,
    module Neovim.RPC.FunctionCall,
    module Neovim.Plugin,

    module Control.Monad,
    module Control.Applicative,
    module Data.Monoid,
    module Data.MessagePack,

    module System.Log,
    module Control.Concurrent,
    module Control.Concurrent.STM,
    ) where

import           Neovim.Classes
import           Neovim.Context
import           Neovim.API.String
import           Neovim.API.TH           (command, command', function,
                                          function')
import           Neovim.Config
import           Neovim.Main             (neovim)
import           Neovim.Plugin           (NeovimPlugin, Plugin (..), wrapPlugin, Synchronous(..), CommandOptions(..))
import           Neovim.RPC.FunctionCall (atomically', respond, wait, wait')

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.STM
import           Data.MessagePack
import           System.Log

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
