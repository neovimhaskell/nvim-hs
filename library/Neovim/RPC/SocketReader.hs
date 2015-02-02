{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Neovim.RPC.SocketReader
Description :  The component which reads RPC messages from the neovim instance
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.RPC.SocketReader (
    runSocketReader,
    ) where

import           Neovim.API.Classes
import           Neovim.API.Context           hiding (ask, asks)
import           Neovim.API.IPC
import           Neovim.RPC.Common
import           Neovim.RPC.FunctionCall

import           Control.Applicative
import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Conduit                 as C
import           Data.Conduit.Binary
import qualified Data.Map                     as Map
import           Data.MessagePack
import           Data.Monoid
import           Data.Serialize               (decode)
import           Data.Text                    (unpack)
import           Data.Time
import           System.IO                    (IOMode (ReadMode))
import           System.Log.Logger

logger :: String
logger = "Socket Reader"

-- | This function will establish a connection to the given socket and read
-- msgpack-rpc events from it.
runSocketReader :: SocketType -> ConfigWrapper RPCConfig -> IO ()
runSocketReader socketType env = do
    h <- createHandle ReadMode socketType
    runSocketHandler env $
        addCleanup (cleanUpHandle h) (sourceHandle h)
            $= awaitForever (yield . decode)
            $$ messageHandlerSink

-- | Convenient transformer stack for the socket reader.
newtype SocketHandler a =
    SocketHandler (ResourceT (Neovim RPCConfig ()) a)
    deriving ( Functor, Applicative, Monad , MonadIO
             , MonadReader (ConfigWrapper RPCConfig))

runSocketHandler :: ConfigWrapper RPCConfig -> SocketHandler a -> IO ()
runSocketHandler r (SocketHandler a) = void $ runNeovim r () (runResourceT a)

-- | Sink that delegates the messages depending on their type.
-- <https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md>
messageHandlerSink :: Sink (Either String Object) SocketHandler ()
messageHandlerSink = awaitForever $ \rpc -> case rpc of
    Left err -> liftIO $ errorM logger $
        "Error parsing rpc message: " <> err
    Right (ObjectArray [ObjectInt msgType, ObjectInt fi, err, result]) ->
        handleResponseOrRequest msgType fi err result
    Right (ObjectArray [ObjectInt msgType, method, params]) ->
        handleNotification msgType method params
    Right obj -> liftIO $ errorM logger $
        "Unhandled rpc message: " <> show obj

handleResponseOrRequest :: Int64 -> Int64 -> Object -> Object
                        -> Sink a SocketHandler ()
handleResponseOrRequest msgType
    | msgType == 1 = handleResponse
    | msgType == 0 = handleRequest
    | otherwise = \_ _ _ -> do
        liftIO $ errorM logger $ "Invalid message type: " <> show msgType
        return ()

handleResponse :: Int64 -> Object -> Object -> Sink a SocketHandler ()
handleResponse i err result = do
    answerMap <- asks (recipients . customConfig)
    mReply <- Map.lookup i <$> liftIO (readTVarIO answerMap)
    case mReply of
        Nothing -> liftIO $ warningM logger
            "Received response but could not find a matching recipient."
        Just (_,reply) -> do
            atomically' $ modifyTVar' answerMap $ Map.delete i
            atomically' . putTMVar reply $ case err of
                ObjectNil -> Right result
                _         -> Left err

handleRequest :: Int64 -> Object -> Object -> Sink a SocketHandler ()
handleRequest i method (ObjectArray params) = case fromObject method of
    Left err -> liftIO . errorM logger $ show err
    Right m -> ask >>= \e -> void . liftIO . forkIO $
        case Map.lookup m ((functions . customConfig) e) of
            Nothing -> debugM logger $ "No provider for: " <> unpack m
            Just (Left f) -> do
                res <- runExceptT $ f params
                atomically' . writeTQueue (_eventQueue e) . SomeMessage
                    . uncurry (Response i) $ writeErrorResponse res
            Just (Right c) -> do
                now <- liftIO getCurrentTime
                reply <- liftIO newEmptyTMVarIO
                let q = (recipients . customConfig) e
                atomically' . modifyTVar q $ Map.insert i (now, reply)
                atomically' . writeTQueue c . SomeMessage
                    $ FunctionCall m params reply now
  where
    writeErrorResponse (Left !err) = (toObject err, ObjectNil)
    writeErrorResponse (Right !res) = (ObjectNil, res)

handleRequest _ _ params = liftIO . errorM logger $
    "Parmaeters in request are not in an object array: " <> show params

handleNotification :: Int64 -> Object -> Object -> Sink a SocketHandler ()
handleNotification msgType fn params
    | msgType /= 2 = liftIO . errorM logger $
        "Message is not a noticiation. Received msgType: " <> show msgType
        <> " The expected value was 2. The following arguments were given: "
        <> show fn <> " and " <> show params

    | otherwise = error "TODO handle notification"

