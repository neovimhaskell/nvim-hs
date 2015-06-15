{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
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
    | AutoCmd  Text Text Text ([Object] -> Neovim r st Object)
    -- ^ Exported autocommand. Will call the given function if the type and
    -- filter match.
    --
    -- NB: Since we are registering this on the Haskell side of things, the
    -- number of accepted arguments should be 0.
    -- TODO Should this be enforced somehow? Possibly via the TH generator.
    --
    -- * Type of autocmd (e.g. FileType)
    -- * Filter fo the autocmd type
    -- * Name for the function to call
    -- * Function to call

class FunctionName a where
    name :: a -> Text

instance FunctionName (ExportedFunctionality r st) where
    name = \case
        Function    n _ -> n
        Command     n _ -> n
        AutoCmd _ _ n _ -> n

-- | This data type contains meta information for the plugin manager.
--
data Plugin r st = Plugin
    { exports         :: [ExportedFunctionality () ()]
    , statefulExports :: [(r, st, [ExportedFunctionality r  st])]
    }

data NeovimPlugin = forall r st. NeovimPlugin (Plugin r st)

-- | Wrap a 'Plugin' in some nice blankets, so that we can put them in a simple
-- list.
wrapPlugin :: Monad m => Plugin r st -> m NeovimPlugin
wrapPlugin = return . NeovimPlugin
