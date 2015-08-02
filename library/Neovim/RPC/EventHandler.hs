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
import           Neovim.Plugin.IPC
import           Neovim.Plugin.IPC.Internal
import           Neovim.RPC.Common
import           Neovim.RPC.FunctionCall

import           Control.Applicative
import           Control.Concurrent.STM       hiding (writeTQueue)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Conduit                 as C
import           Data.Conduit.Binary          (sinkHandle)
import qualified Data.Map                     as Map
import           Data.MessagePack
import           Data.Serialize               (encode)
import           System.IO                    (IOMode (WriteMode))
import           System.Log.Logger

import           Prelude


-- | This function will establish a connection to the given socket and write
-- msgpack-rpc requests to it.
runEventHandler :: SocketType
                -> Internal.Config RPCConfig Int64
                -> IO ()
runEventHandler socketType env =
    runEventHandlerContext env $ do
        h <- createHandle WriteMode socketType
        eventHandlerSource
            $= eventHandler
            $$ addCleanup (cleanUpHandle h) (sinkHandle h)


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


eventHandler :: ConduitM SomeMessage ByteString EventHandler ()
eventHandler = await >>= \case
    Nothing -> return () -- i.e. close the conduit -- TODO signal shutdown globally
    Just message -> handleMessage (fromMessage message) >> eventHandler


yield' :: (MonadIO io) => Object -> ConduitM i ByteString io ()
yield' o = do
    liftIO . debugM "EventHandler" $ "Sending: " ++ show o
    yield $ encode o


handleMessage :: Maybe RPCMessage -> ConduitM i ByteString EventHandler ()
handleMessage = \case
    Just (FunctionCall fn params reply time) -> do
        i <- get
        modify succ
        rs <- asks (recipients . Internal.customConfig)
        atomically' . modifyTVar rs $ Map.insert i (time, reply)
        yield' $ ObjectArray
            [ toObject (0 :: Int64)
            , ObjectInt i
            , toObject fn
            , toObject params
            ]
    Just (Response i e res) ->
        yield' $ ObjectArray
            [ toObject (1 :: Int64)
            , ObjectInt i
            , toObject e
            , toObject res
            ]
    Just (NotificationCall fn params) ->
        yield' $ ObjectArray
            [ ObjectInt 2
            , toObject fn
            , toObject params
            ]
    Nothing -> return () -- i.e. skip to next message

