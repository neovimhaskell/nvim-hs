{- |
Module      :  Neovim.Plugin.IPC
Description :  Communication between Haskell processes/threads
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

This module reexports publicly available means to communicate between different
plugins (or more generally threads running in the same plugin provider).
-}
module Neovim.Plugin.IPC (
    SomeMessage(..),
    fromMessage,

    ) where

import           Neovim.Plugin.IPC.Classes

