{-# LANGUAGE ExistentialQuantification #-}
{- |
Module      :  Neovim.Plugin.Internal
Description :  Split module that can import Neovim.Context without creating import circles
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.Internal (
    ExportedFunctionality(..),
    StatefulFunctionality(..),
    getFunction,
    getDescription,
    NeovimPlugin(..),
    Plugin(..),
    wrapPlugin,
    ) where

import           Neovim.Context
import           Neovim.Plugin.Classes

import           Data.MessagePack


-- | This data type is used in the plugin registration to properly register the
-- functions.
newtype ExportedFunctionality r st
    = EF (FunctionalityDescription, [Object] -> Neovim r st Object)


-- | Extract the description of an 'ExportedFunctionality'.
getDescription :: ExportedFunctionality r st -> FunctionalityDescription
getDescription (EF (d,_)) = d


-- | Extract the function of an 'ExportedFunctionality'.
getFunction :: ExportedFunctionality r st -> [Object] -> Neovim r st Object
getFunction (EF (_, f)) = f


instance HasFunctionName (ExportedFunctionality r st) where
    name = name . getDescription


-- | This datatype contains the initial state (mutable and immutable) for the
-- functionalities defined here.
data StatefulFunctionality r st = StatefulFunctionality
    { readOnly        :: r
    , writable        :: st
    , functionalities :: [ExportedFunctionality r st]
    }


-- | This data type contains meta information for the plugin manager.
--
data Plugin r st = Plugin
    { exports         :: [ExportedFunctionality () ()]
    , statefulExports :: [StatefulFunctionality r st]
    }


-- | 'Plugin' values are wraped inside this data type via 'wrapPlugin' so that
-- we can put plugins in an ordinary list.
data NeovimPlugin = forall r st. NeovimPlugin (Plugin r st)


-- | Wrap a 'Plugin' in some nice blankets, so that we can put them in a simple
-- list.
wrapPlugin :: Applicative m => Plugin r st -> m NeovimPlugin
wrapPlugin = pure . NeovimPlugin
