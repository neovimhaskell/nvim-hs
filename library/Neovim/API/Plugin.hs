{-# LANGUAGE ExistentialQuantification #-}
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
    ExportedFunctionality(..),
    Plugin(..),
    SomePlugin(..),
    Request(..),
    awaitRequest,

    SomeMessage,
    fromMessage,
    ) where

import           Neovim.API.Context
import           Neovim.API.IPC         (Request (..), SomeMessage, fromMessage)

import           Control.Concurrent.STM
import           Data.MessagePack
import           Data.Text

-- | This data type is used in the plugin registration to properly register the
-- functions.
data ExportedFunctionality
    = Function Text ([Object] -> Neovim' Object)
    -- ^
    --
    -- * Name of the function (must start with an uppercase letter)
    -- * Function to call
    | Command  Text ([Object] -> Neovim' Object)
    -- ^
    --
    -- * Name of the command (must start with an uppercase letter)
    -- * Function to call
    | AutoCmd  Text Text ([Object] -> Neovim' Object)
    -- ^
    --
    -- * Type of autocmd (e.g. FileType)
    -- * Filter fo the autocmd type
    -- * Function to call

-- | This data type contains meta information for the plugin manager.
--
data Plugin r st = Plugin
    { name              :: String
    , functions         :: [ExportedFunctionality]
    , statefulFunctions :: [(Text, TQueue SomeMessage)]
    , services          :: [(r, st, Neovim r st ())]
    }

data SomePlugin = forall r st. SomePlugin (Plugin r st)

awaitRequest :: (MonadIO io) => TQueue SomeMessage -> io Request
awaitRequest q = do
    msg <- liftIO . atomically $ readTQueue q
    case fromMessage msg of
        Nothing -> awaitRequest q
        Just r@(Request{}) -> return r

