{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  Neovim.RPC.EventHandler
Description :  Event handling loop
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
-}
module Neovim.RPC.EventHandler (
    runEventHandler,
) where

import Neovim.Classes
import Neovim.Context
import qualified Neovim.Context.Internal as Internal
import Neovim.Plugin.IPC.Classes
import qualified Neovim.RPC.Classes as MsgpackRPC
import Neovim.RPC.Common
import Neovim.RPC.FunctionCall

import Conduit as C
import Control.Applicative
import Control.Concurrent.STM hiding (writeTQueue)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Serialize (encode)
import System.IO (Handle)
import System.Log.Logger

import Prelude

{- | This function will establish a connection to the given socket and write
 msgpack-rpc requests to it.
-}
runEventHandler ::
    Handle ->
    Internal.Config RPCConfig ->
    IO ()
runEventHandler writeableHandle env =
    runEventHandlerContext env . runConduit $ do
        eventHandlerSource
            .| eventHandler
            .| sinkHandleFlush writeableHandle

-- | Convenient monad transformer stack for the event handler
newtype EventHandler a
    = EventHandler (ResourceT (ReaderT (Internal.Config RPCConfig) IO) a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Internal.Config RPCConfig)
        )

runEventHandlerContext ::
    Internal.Config RPCConfig -> EventHandler a -> IO a
runEventHandlerContext env (EventHandler a) =
    runReaderT (runResourceT a) env

eventHandlerSource :: ConduitT () SomeMessage EventHandler ()
eventHandlerSource =
    asks Internal.eventQueue >>= \q ->
        forever $ yield =<< readSomeMessage q

eventHandler :: ConduitM SomeMessage EncodedResponse EventHandler ()
eventHandler =
    await >>= \case
        Nothing ->
            return () -- i.e. close the conduit -- TODO signal shutdown globally
        Just message -> do
            handleMessage (fromMessage message, fromMessage message)
            eventHandler

type EncodedResponse = C.Flush ByteString

yield' :: (MonadIO io) => MsgpackRPC.Message -> ConduitM i EncodedResponse io ()
yield' o = do
    liftIO . debugM "EventHandler" $ "Sending: " ++ show o
    yield . Chunk . encode $ toObject o
    yield Flush

handleMessage ::
    (Maybe FunctionCall, Maybe MsgpackRPC.Message) ->
    ConduitM i EncodedResponse EventHandler ()
handleMessage = \case
    (Just (FunctionCall fn params reply time), _) -> do
        cfg <- asks (Internal.customConfig)
        messageId <- atomically' $ do
            i <- readTVar (nextMessageId cfg)
            modifyTVar' (nextMessageId cfg) succ
            modifyTVar' (recipients cfg) $ Map.insert i (time, reply)
            return i
        yield' $ MsgpackRPC.Request (Request fn messageId params)
    (_, Just r@MsgpackRPC.Response{}) ->
        yield' r
    (_, Just n@MsgpackRPC.Notification{}) ->
        yield' n
    _ ->
        return () -- i.e. skip to next message
