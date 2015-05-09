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
import           Control.Monad.Except
import           Data.MessagePack
import           Data.Text

-- | This data type contains meta information for the plugin manager.
--
data Plugin r st = Plugin
    { name              :: String
    , functions         :: [(Text, [Object] -> ExceptT String IO Object)]
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

