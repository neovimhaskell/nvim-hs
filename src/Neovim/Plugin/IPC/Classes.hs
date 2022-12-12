{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Neovim.Plugin.IPC.Classes
Description :  Classes used for Inter Plugin Communication
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC
-}
module Neovim.Plugin.IPC.Classes (
    SomeMessage (..),
    Message (..),
    FunctionCall (..),
    Request (..),
    Notification (..),
    writeMessage,
    readSomeMessage,
    UTCTime,
    getCurrentTime,
    module Data.Int,
) where

import Neovim.Classes (
    Generic,
    Int64,
    NFData (..),
    Pretty (pretty),
    deepseq,
    (<+>),
 )
import Neovim.Plugin.Classes (FunctionName, NeovimEventId)

import Data.Data (cast)
import Data.Int (Int64)
import Data.MessagePack (Object)
import Data.Time (UTCTime, formatTime, getCurrentTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Prettyprinter (hardline, nest, viaShow)
import UnliftIO (
    MonadIO (..),
    MonadUnliftIO,
    TMVar,
    TQueue,
    Typeable,
    atomically,
    evaluate,
    readTQueue,
    writeTQueue,
 )

import Prelude

{- | Taken from xmonad and based on ideas in /An Extensible Dynamically-Typed
 Hierarchy of Exceptions/, Simon Marlow, 2006.

 User-extensible messages must be put into a value of this type, so that it
 can be sent to other plugins.
-}
data SomeMessage = forall msg. Message msg => SomeMessage msg

{- | This class allows type safe casting of 'SomeMessage' to an actual message.
 The cast is successful if the type you're expecting matches the type in the
 'SomeMessage' wrapper. This way, you can subscribe to an arbitrary message
 type withouth having to pattern match on the constructors. This also allows
 plugin authors to create their own message types without having to change the
 core code of /nvim-hs/.
-}
class (NFData message, Typeable message) => Message message where
    -- | Try to convert a given message to a value of the message type we are
    -- interested in. Will evaluate to 'Nothing' for any other type.
    fromMessage :: SomeMessage -> Maybe message
    fromMessage (SomeMessage message) = cast message

writeMessage :: (MonadUnliftIO m, Message message) => TQueue SomeMessage -> message -> m ()
writeMessage q message = liftIO $ do
    evaluate (rnf message)
    atomically $ writeTQueue q (SomeMessage message)

readSomeMessage :: MonadIO m => TQueue SomeMessage -> m SomeMessage
readSomeMessage q = liftIO $ atomically (readTQueue q)

-- | Haskell representation of supported Remote Procedure Call messages.
data FunctionCall
    = -- | Method name, parameters, callback, timestamp
      FunctionCall FunctionName [Object] (TMVar (Either Object Object)) UTCTime
    deriving (Typeable, Generic)

instance NFData FunctionCall where
    rnf (FunctionCall f os v t) = f `deepseq` os `deepseq` v `seq` t `deepseq` ()

instance Message FunctionCall

instance Pretty FunctionCall where
    pretty (FunctionCall fname args _ t) =
        nest 2 $
            "Function call for:"
                <+> pretty fname
                    <> hardline
                    <> "Arguments:"
                <+> viaShow args
                    <> hardline
                    <> "Timestamp:"
                <+> (viaShow . formatTime defaultTimeLocale "%H:%M:%S (%q)") t

{- | A request is a data type containing the method to call, its arguments and
 an identifier used to map the result to the function that has been called.
-}
data Request = Request
    { -- | Name of the function to call.
      reqMethod :: FunctionName
    , -- | Identifier to map the result to a function call invocation.
      reqId :: !Int64
    , -- | Arguments for the function.
      reqArgs :: [Object]
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData Request

instance Message Request

instance Pretty Request where
    pretty Request{..} =
        nest 2 $
            "Request"
                <+> "#"
                    <> pretty reqId
                    <> hardline
                    <> "Method:"
                <+> pretty reqMethod
                    <> hardline
                    <> "Arguments:"
                <+> viaShow reqArgs

{- | A notification is similar to a 'Request'. It essentially does the same
 thing, but the function is only called for its side effects. This type of
 message is sent by neovim if the caller there does not care about the result
 of the computation.
-}
data Notification = Notification
    { -- | Event name of the notification.
      notEvent :: NeovimEventId
    , -- | Arguments for the function.
      notArgs :: [Object]
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData Notification

instance Message Notification

instance Pretty Notification where
    pretty Notification{..} =
        nest 2 $
            "Notification"
                <> hardline
                <> "Event:"
                <+> pretty notEvent
                    <> hardline
                    <> "Arguments:"
                <+> viaShow notEvent
