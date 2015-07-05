{-# LANGUAGE ExistentialQuantification #-}
{- |
Module      :  Neovim.Plugin.IPC.Classes
Description :  Classes used for Inter Plugin Communication
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.IPC.Classes
    where

import           Data.Data (Typeable, cast)

-- | Taken from xmonad and based on ideas in /An Extensible Dynamically-Typed
-- Hierarchy of Exceptions/, Simon Marlow, 2006.
--
-- User-extensible messages must be put into a value of this type, so that it
-- can be sent to other plugins.
data SomeMessage = forall msg. Message msg => SomeMessage msg

class Typeable message => Message message where
    -- | Try to convert a given message to a value of the message type we are
    -- interested in. Will evaluate to 'Nothing' for any other type.
    fromMessage :: SomeMessage -> Maybe message
    fromMessage (SomeMessage message) = cast message

