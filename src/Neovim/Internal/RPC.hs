{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Neovim.Internal.RPC where

import Neovim.Classes (
    NvimObject (fromObject, toObject),
 )

import Control.DeepSeq (NFData (..))
import Data.Int (Int64)
import Data.MessagePack (Object)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Pretty)
import UnliftIO (MonadUnliftIO, TBQueue, atomically, evaluate, newTBQueueIO, readTBQueue, writeTBQueue)

-- | Name of a function to call.
newtype FunctionName = F Text
    deriving (Eq, Ord, Show, Read, Generic)
    deriving (NFData, Pretty) via Text

{- | Identifier for callable things in neovim.
 -
 This is in an abstract data type because nvim-hs internally prefixes 
 functions, commands and autocmds when it registers them with neovim.
-}
newtype NvimMethod = NvimMethod {nvimMethodName :: Text}
    deriving (Eq, Ord, Show, Read, Generic)
    deriving (Pretty, NFData) via Text

-- | Conveniennce class to extract a name from some value.
class HasFunctionName a where
    name :: a -> FunctionName
    nvimMethod :: a -> NvimMethod


{- | Event that is send by neovim. This is basically a string that can be an
 event, a function name, a command or an autocmd.
-}
newtype NeovimEventId = NeovimEventId Text
    deriving (Eq, Ord, Show, Read, Generic)
    deriving (Pretty) via Text
    deriving (NFData) via Text

instance NvimObject NeovimEventId where
    toObject (NeovimEventId e) = toObject e
    fromObject o = NeovimEventId <$> fromObject o

instance HasFunctionName NeovimEventId where
  name (NeovimEventId i) = F i
  nvimMethod (NeovimEventId i) = NvimMethod i

newtype SubscriptionId = SubscriptionId Int64
    deriving (Eq, Ord, Show, Read)
    deriving (Enum) via Int64

data Subscription = Subscription
    { subscriptionId :: SubscriptionId
    , subscriptionEventId :: NeovimEventId
    , subscriptionAction :: [Object] -> IO ()
    }

data MsgpackRpcMessage
    = Request MsgpackRequest
    | Notification MsgpackNotification
    | Response MsgpackResponse
    deriving (Generic)

instance NFData MsgpackRpcMessage

data MsgpackRequest = MsgpackRequest
    { requestMethod :: NvimMethod
    , requestArguments :: [Object]
    , requestId :: RequestId
    }
    deriving (Generic)

instance NFData MsgpackRequest

newtype RequestId = RequestId Int64
    deriving (Eq, Ord, Show, Generic)
    deriving (Enum) via Int64

instance NFData RequestId

data MsgpackNotification = MsgpackNotification
    { notificationEvent :: NeovimEventId
    , notificationArguments :: [Object]
    }
    deriving (Generic)

instance NFData MsgpackNotification

data MsgpackResponse = MsgpackResponse
    { responseRequestId :: RequestId
    , responseResult :: Either Text Object
    }
    deriving (Generic)

instance NFData MsgpackResponse

newtype MsgpackRpcQueue = MsgpackRpcQueue (TBQueue MsgpackRpcMessage)

newMsgpackRcpQueue :: MonadUnliftIO io => io MsgpackRpcQueue
newMsgpackRcpQueue = MsgpackRpcQueue <$> newTBQueueIO 128

-- | A monaad that has a 'MsgpackRpcQueue'.
class HasMsgpackRpcQueue m where
    msgpackRpcQueue :: m MsgpackRpcQueue

writeMsgpackRpcQueue :: (MonadUnliftIO m, HasMsgpackRpcQueue m) => MsgpackRpcMessage -> m ()
writeMsgpackRpcQueue message = do
    evaluate (rnf message)
    MsgpackRpcQueue queue <- msgpackRpcQueue
    atomically $ writeTBQueue queue message

readBlockingFromMsgpackRpcQueue :: (MonadUnliftIO m, HasMsgpackRpcQueue m) => m MsgpackRpcMessage
readBlockingFromMsgpackRpcQueue = do
    MsgpackRpcQueue queue <- msgpackRpcQueue
    atomically $ readTBQueue queue
