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
newtype ExportedFunctionality env
    = EF (FunctionalityDescription, [Object] -> Neovim env Object)


-- | Extract the description of an 'ExportedFunctionality'.
getDescription :: ExportedFunctionality env -> FunctionalityDescription
getDescription (EF (d,_)) = d


-- | Extract the function of an 'ExportedFunctionality'.
getFunction :: ExportedFunctionality env -> [Object] -> Neovim env Object
getFunction (EF (_, f)) = f


instance HasFunctionName (ExportedFunctionality env) where
    name = name . getDescription


-- | This datatype contains the initial state (mutable and immutable) for the
-- functionalities defined here.
data StatefulFunctionality env = StatefulFunctionality
    { environment        :: env
    , functionalities :: [ExportedFunctionality env]
    }


-- | This data type contains meta information for the plugin manager.
--
data Plugin env = Plugin
    { exports         :: [ExportedFunctionality ()]
    , statefulExports :: [StatefulFunctionality env]
    }


-- | 'Plugin' values are wraped inside this data type via 'wrapPlugin' so that
-- we can put plugins in an ordinary list.
data NeovimPlugin = forall env. NeovimPlugin (Plugin env)


-- | Wrap a 'Plugin' in some nice blankets, so that we can put them in a simple
-- list.
wrapPlugin :: Applicative m => Plugin env -> m NeovimPlugin
wrapPlugin = pure . NeovimPlugin
