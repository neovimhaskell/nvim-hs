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

import           Neovim.API.Classes
import           Neovim.API.Context hiding (ask, asks)
import           Neovim.API.IPC
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
import           Data.Word                    (Word32)
import           System.IO                    (IOMode (WriteMode))

-- | This function will establish a connection to the given socket and write
-- msgpack-rpc requests to it.
runEventHandler :: SocketType
                -> ConfigWrapper RPCConfig
                -> IO ()
runEventHandler socketType env =
    runEventHandlerContext env $ do
        h <- createHandle WriteMode socketType
        eventHandlerSource
            $= awaitForever eventHandler
            $$ addCleanup (cleanUpHandle h) (sinkHandle h)

-- | Convenient monad transformer stack for the event handler
newtype EventHandler a =
    EventHandler (ResourceT (ReaderT (ConfigWrapper RPCConfig) (StateT Word32 IO)) a)
    deriving ( Functor, Applicative, Monad, MonadState Word32, MonadIO
             , MonadReader (ConfigWrapper RPCConfig))

runEventHandlerContext :: ConfigWrapper RPCConfig -> EventHandler a -> IO a
runEventHandlerContext env (EventHandler a) =
    evalStateT (runReaderT (runResourceT a) env) 1

eventHandlerSource :: Source EventHandler SomeMessage
eventHandlerSource = asks _eventQueue >>= \q ->
    forever $ yield =<< atomically' (readTQueue q)

eventHandler :: SomeMessage -> ConduitM SomeMessage ByteString EventHandler ()
eventHandler message = case fromMessage message of
    Just (FunctionCall fn params reply time) -> do
        i <- get
        modify succ
        rec <- asks (recipients . customConfig)
        atomically' . modifyTVar rec $ Map.insert i (time, reply)
        yield . encode $ ObjectArray
            [ toObject (0 :: Int64)
            , ObjectFixInt (PosFixInt32 i)
            , toObject fn
            , toObject params
            ]
    Just (Response i err res) ->
        yield . encode $ ObjectArray
            [ toObject (1 :: Int64)
            , ObjectFixInt (PosFixInt32 i)
            , toObject err
            , toObject res
            ]
    Nothing -> return ()

