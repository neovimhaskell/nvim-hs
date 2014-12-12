{-# LANGUAGE RankNTypes #-}
{- |
Module      :  Neovim.API.Plugin
Description :  Plugin author documentation
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

This module describes how a Haksell plugin can be plugged into Neovim.
-}
module Neovim.API.Plugin (
    Plugin(..),

    ) where

import Neovim.API.Context
import Neovim.API.Classes
import Data.MessagePack
import Control.Concurrent.STM
import Data.Text

-- | This data type contains meta information for the plugin manager.
--
data Plugin config state =
    SimplePlugin
        { name             :: String
        -- ^ The name of the plugin.
        , functions        :: forall result. (NvimObject result) =>
                                [(Text, Function config state result)]
        -- ^ Functions provided by this plugin.
        --
        -- The type may be look suspicious, but the plan is to create this list
        -- from existing functions via template haskell.
        -- @
        -- export ['function1, 'foo, 'bar]
        -- @
        -- This is still just an idea and I don't know if this exact syntax is
        -- possible.
        }
    | Service
        { name             :: String
        -- ^ The name of the plugin.
        , initialConfig    :: config
        , initialState     :: state
        , serviceThread    :: TChan Message -> Neovim config state ()
        }

