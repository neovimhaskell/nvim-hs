{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{- |
Module      :  Neovim.API.IPC
Description :  Communication between Haskell processes/threads
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.IPC (
    SomeMessage(..),
    RPCMessage(..),
    Request(..),
    fromMessage,

    module Data.Word,
    ) where

import           Control.Concurrent.STM
import           Data.MessagePack
import           Data.Text              as Text
import           Data.Time
import           Data.Typeable          (Typeable, cast)
import           Data.Word              (Word32)


-- | Taken from xmonad and based on ideas in /An Extensible Dynamically-Typed
-- Hierarchy of Exceptions/, Simon Marlow, 2006.
--
-- User-extensible messages must be a member of this class.
data SomeMessage = forall msg. Message msg => SomeMessage msg

class Typeable message => Message message where
    fromMessage :: (Message message) => SomeMessage -> Maybe message
    fromMessage (SomeMessage message) = cast message

data RPCMessage = FunctionCall Text [Object] (TMVar (Either Object Object)) UTCTime
                -- ^ Method name, parameters, callback, timestamp
                | Response !Word32 Object Object
                -- ^ Responese sent to indicate the result of a function call.
                --
                -- * identfier of the message as 'Word32'
                -- * Error value
                -- * Result value
    deriving (Typeable)

instance Message RPCMessage

data Request = Request
    { reqMethod :: Text
    , reqId     :: !Word32
    , reqArgs   :: [Object]
    }
    deriving (Typeable)

instance Message Request

