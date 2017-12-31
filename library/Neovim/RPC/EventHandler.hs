{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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

import           Neovim.Classes
import           Neovim.Context
import qualified Neovim.Context.Internal      as Internal
import           Neovim.Plugin.IPC.Classes
import qualified Neovim.RPC.Classes           as MsgpackRPC
import           Neovim.RPC.Common
import           Neovim.RPC.FunctionCall

import           Control.Applicative
import           Control.Concurrent.STM       hiding (writeTQueue)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Conduit                 as C
#if MIN_VERSION_conduit_extra(1,2,2)
import           Data.Conduit.Binary          (sinkHandleFlush)
#else
import           Data.Conduit.Binary          (sinkHandle)
#endif
import qualified Data.Map                     as Map
import           Data.Serialize               (encode)
import           System.IO                    (Handle)
import           System.Log.Logger

import           Prelude


-- | This function will establish a connection to the given socket and write
-- msgpack-rpc requests to it.
runEventHandler :: Handle
                -> Internal.Config RPCConfig Int64
                -> IO ()
runEventHandler writeableHandle env =
    runEventHandlerContext env $ do
        eventHandlerSource
            $= eventHandler
            $$ addCleanup
                (cleanUpHandle writeableHandle)
#if MIN_VERSION_conduit_extra(1,2,2)
                (sinkHandleFlush writeableHandle)
#else
                (sinkHandle writeableHandle)
#endif


-- | Convenient monad transformer stack for the event handler
newtype EventHandler a =
    EventHandler (ResourceT (ReaderT (Internal.Config RPCConfig Int64) (StateT Int64 IO)) a)
    deriving ( Functor, Applicative, Monad, MonadState Int64, MonadIO
             , MonadReader (Internal.Config RPCConfig Int64))


runEventHandlerContext
    :: Internal.Config RPCConfig Int64 -> EventHandler a -> IO a
runEventHandlerContext env (EventHandler a) =
    evalStateT (runReaderT (runResourceT a) env) 1


eventHandlerSource :: Source EventHandler SomeMessage
eventHandlerSource = asks Internal.eventQueue >>= \q ->
    forever $ yield =<< atomically' (readTQueue q)


eventHandler :: ConduitM SomeMessage EncodedResponse EventHandler ()
eventHandler = await >>= \case
    Nothing ->
        return () -- i.e. close the conduit -- TODO signal shutdown globally

    Just message -> do
        handleMessage (fromMessage message, fromMessage message)
        eventHandler


#if MIN_VERSION_conduit_extra(1,2,2)
type EncodedResponse = Flush ByteString
#else
type EncodedResponse = ByteString
#endif

yield' :: (MonadIO io) => MsgpackRPC.Message -> ConduitM i EncodedResponse io ()
yield' o = do
    liftIO . debugM "EventHandler" $ "Sending: " ++ show o
#if MIN_VERSION_conduit_extra(1,2,2)
    yield . Chunk . encode $ toObject o
    yield Flush
#else
    yield . encode $ toObject o
#endif

handleMessage :: (Maybe FunctionCall, Maybe MsgpackRPC.Message)
              -> ConduitM i EncodedResponse EventHandler ()
handleMessage = \case
    (Just (FunctionCall fn params reply time), _) -> do
        i <- get
        modify succ
        rs <- asks (recipients . Internal.customConfig)
        atomically' . modifyTVar rs $ Map.insert i (time, reply)
        yield' $ MsgpackRPC.Request (Request fn i params)

    (_, Just r@MsgpackRPC.Response{}) ->
        yield' $ r

    (_, Just n@MsgpackRPC.Notification{}) ->
        yield' $ n

    _ ->
        return () -- i.e. skip to next message

