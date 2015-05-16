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
import           Data.Conduit.Cereal
import qualified Data.Map                     as Map
import           Data.MessagePack
import           Data.Monoid
import qualified Data.Serialize               (get)
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
            $= conduitGet Data.Serialize.get
            $$ messageHandlerSink

-- | Convenient transformer stack for the socket reader.
newtype SocketHandler a =
    SocketHandler (ResourceT (Neovim RPCConfig ()) a)
    deriving ( Functor, Applicative, Monad , MonadIO
             , MonadReader (ConfigWrapper RPCConfig), MonadThrow)

runSocketHandler :: ConfigWrapper RPCConfig -> SocketHandler a -> IO ()
runSocketHandler r (SocketHandler a) = void $ runNeovim r () (runResourceT a)

-- | Sink that delegates the messages depending on their type.
-- <https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md>
messageHandlerSink :: Sink Object SocketHandler ()
messageHandlerSink = awaitForever $ \rpc -> case rpc of
    ObjectArray [ObjectInt msgType, ObjectInt fi, err, result] ->
        handleResponseOrRequest msgType fi err result
    ObjectArray [ObjectInt msgType, method, params] ->
        handleNotification msgType method params
    obj -> liftIO $ errorM logger $
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
    -- Fork everything so that we do not block.
    --
    -- XXX If the functions never return, we may end up with a lot of idle
    -- threads. Maybe we should gather the ThreadIds and kill them after some
    -- amount of time if they are still around. Maybe resourcet provides such a
    -- facility for threads already.
    Right m -> ask >>= \e -> void . liftIO . forkIO $
        case Map.lookup m ((functions . customConfig) e) of
            Nothing -> do
                let err = "No provider for: " <> unpack m
                debugM logger err
                atomically' . writeTQueue (_eventQueue e) . SomeMessage
                    $ Response i (toObject err) ObjectNil
            Just (Left f) -> do
                -- Stateless function: Create a boring state object for the
                -- Neovim context.
                let r = ConfigWrapper (_eventQueue e) ()
                -- drop the state of the result with (fmap fst <$>)
                res <- fmap fst <$> runNeovim r () (f params)
                -- Send the result to the event handler
                atomically' . writeTQueue (_eventQueue e) . SomeMessage
                    . uncurry (Response i) $ responseResult res
            Just (Right c) -> do
                now <- liftIO getCurrentTime
                reply <- liftIO newEmptyTMVarIO
                let q = (recipients . customConfig) e
                atomically' . modifyTVar q $ Map.insert i (now, reply)
                atomically' . writeTQueue c . SomeMessage $ Request m i params
  where
    responseResult (Left !err) = (toObject err, ObjectNil)
    responseResult (Right !res) = (ObjectNil, toObject res)

handleRequest _ _ params = liftIO . errorM logger $
    "Parmaeters in request are not in an object array: " <> show params

handleNotification :: Int64 -> Object -> Object -> Sink a SocketHandler ()
handleNotification msgType fn params
    | msgType /= 2 = liftIO . errorM logger $
        "Message is not a noticiation. Received msgType: " <> show msgType
        <> " The expected value was 2. The following arguments were given: "
        <> show fn <> " and " <> show params

    | otherwise = error "TODO handle notification"

