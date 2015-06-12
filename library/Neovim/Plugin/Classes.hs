{-# LANGUAGE ExistentialQuantification #-}
{- |
Module      :  Neovim.Plugin.Classes
Description :  Classes and data types related to plugins
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.Classes
    where

import Data.MessagePack
import Data.Text (Text)
import Neovim.API.Context

-- | This data type is used in the plugin registration to properly register the
-- functions.
data ExportedFunctionality r st
    = Function Text ([Object] -> Neovim r st Object)
    -- ^ Exported function. Callable via @call name(arg1,arg2)@.
    --
    -- * Name of the function (must start with an uppercase letter)
    -- * Function to call
    | Command  Text ([Object] -> Neovim r st Object)
    -- ^ Exported Command. Callable via @:Name arg1 arg2@.
    --
    -- * Name of the command (must start with an uppercase letter)
    -- * Function to call
    | AutoCmd  Text Text ([Object] -> Neovim r st Object)
    -- ^ Exported autocommand. Will call the given function if the type and
    -- filter match.
    --
    -- NB: Since we are registering this on the Haskell side of things, the
    -- number of accepted arguments should be 0.
    -- TODO Should this be enforced somehow? Possibly via the TH generator.
    --
    -- * Type of autocmd (e.g. FileType)
    -- * Filter fo the autocmd type
    -- * Function to call

-- | This data type contains meta information for the plugin manager.
--
data Plugin r st = Plugin
    { exports         :: [ExportedFunctionality () ()]
    , statefulExports :: [(r, st, [ExportedFunctionality r  st])]
    }

data SomePlugin = forall r st. SomePlugin (Plugin r st)
