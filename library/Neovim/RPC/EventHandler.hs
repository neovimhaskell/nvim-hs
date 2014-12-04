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
import           Neovim.API.Context
import           Neovim.RPC.Common

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Conduit                 as C
import           Data.Conduit.Binary          (sinkHandle)
import qualified Data.Map                     as Map
import           Data.MessagePack
import           Data.Serialize (encode)
import           System.IO                    (IOMode (WriteMode))

-- | This function will establish a connection to the given socket and write
-- msgpack-rpc requests to it.
runEventHandler :: SocketType
                -> InternalEnvironment ()
                -> IO ()
runEventHandler socketType env =
    runEventHandlerContext env $ do
        h <- createHandle WriteMode socketType
        eventHandlerSource
            $= awaitForever eventHandler
            $$ addCleanup (cleanUpHandle h) (sinkHandle h)

-- | Convenient monad transformer stack for the event handler
newtype EventHandler a =
    EventHandler (ResourceT (ReaderT (InternalEnvironment ()) (StateT Int64 IO)) a)
    deriving ( Functor, Applicative, Monad, MonadState Int64, MonadIO
             , MonadReader (InternalEnvironment ()))

runEventHandlerContext :: InternalEnvironment () -> EventHandler a -> IO a
runEventHandlerContext env (EventHandler a) =
    evalStateT (runReaderT (runResourceT a) env) 1

eventHandlerSource :: Source EventHandler Message
eventHandlerSource = asks eventQueue >>= \q ->
    forever $ yield =<< atomically' (readTQueue q)

eventHandler :: Message -> ConduitM Message ByteString EventHandler ()
eventHandler msg = case msg of
    FunctionCall fn params reply time -> do
        i <- get
        modify succ
        rec <- asks recipients
        atomically' . modifyTVar rec $ Map.insert i (time, reply)
        yield . encode $ ObjectArray
            [toObject (0 :: Int64), toObject i, toObject fn, toObject params]


