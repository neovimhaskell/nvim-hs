{-# LANGUAGE LambdaCase #-}
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
    parseParams,
) where

import Neovim.Classes ( Int64, NvimObject(toObject, fromObject) )
import Neovim.Context ( MonadIO(liftIO), asks, Neovim, runNeovim )
import qualified Neovim.Context.Internal as Internal
import Neovim.Plugin.Classes (
    CommandArguments (..),
    CommandOption (..),
    FunctionName (..),
    FunctionalityDescription (..),
    NeovimEventId (..),
    NvimMethod (..),
    Subscription (..),
    getCommandOptions,
 )
import Neovim.Plugin.IPC.Classes
    ( getCurrentTime,
      Notification(Notification),
      Request(Request),
      writeMessage )
import qualified Neovim.RPC.Classes as MsgpackRPC
import Neovim.RPC.Common ( RPCConfig(recipients) )
import Neovim.RPC.FunctionCall ( atomically' )

import Conduit as C
    ( Void,
      MonadTrans(lift),
      sourceHandle,
      (.|),
      awaitForever,
      runConduit,
      ConduitT )
import Control.Monad (void)
import Data.Conduit.Cereal (conduitGet2)
import Data.Default (def)
import Data.Foldable (foldl', forM_)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.MessagePack ( Object(ObjectArray) )
import qualified Data.Serialize (get)
import System.IO (Handle)
import System.Log.Logger ( debugM, errorM, warningM )
import UnliftIO (atomically, timeout, readTVarIO, modifyTVar', putTMVar, readTMVar, async, newEmptyTMVarIO, modifyTVar)

import Prelude

logger :: String
logger = "Socket Reader"

type SocketHandler = Neovim RPCConfig

{- | This function will establish a connection to the given socket and read
 msgpack-rpc events from it.
-}
runSocketReader ::
    Handle ->
    Internal.Config RPCConfig ->
    IO ()
runSocketReader readableHandle cfg =
    void . runNeovim (Internal.retypeConfig (Internal.customConfig cfg) cfg) . runConduit $ do
        sourceHandle readableHandle
            .| conduitGet2 Data.Serialize.get
            .| messageHandlerSink

{- | Sink that delegates the messages depending on their type.
 <https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md>
-}
messageHandlerSink :: ConduitT Object Void SocketHandler ()
messageHandlerSink = awaitForever $ \rpc -> do
    liftIO . debugM logger $ "Received: " <> show rpc
    case fromObject rpc of
        Right (MsgpackRPC.Request (Request fn i ps)) ->
            handleRequest i fn ps
        Right (MsgpackRPC.Response i r) ->
            handleResponse i r
        Right (MsgpackRPC.Notification (Notification eventId args)) ->
            handleNotification eventId args
        Left e ->
            liftIO . errorM logger $ "Unhandled rpc message: " <> show e

handleResponse :: Int64 -> Either Object Object -> ConduitT a Void SocketHandler ()
handleResponse i result = do
    answerMap <- asks recipients
    mReply <- Map.lookup i <$> liftIO (readTVarIO answerMap)
    case mReply of
        Nothing ->
            liftIO $ warningM logger "Received response but could not find a matching recipient."
        Just (_, reply) -> do
            atomically' . modifyTVar' answerMap $ Map.delete i
            atomically' $ putTMVar reply result

lookupFunction ::
    Internal.Config RPCConfig ->
    FunctionName ->
    IO (Maybe (FunctionalityDescription, Internal.FunctionType))
lookupFunction rpc (F functionName) = do
    functionMap <- atomically $ readTMVar (Internal.globalFunctionMap rpc)
    pure $ Map.lookup (NvimMethod functionName) functionMap

handleRequest :: Int64 -> FunctionName -> [Object] -> ConduitT a Void SocketHandler ()
handleRequest requestId functionToCall params = do
    cfg <- lift Internal.ask'
    void . liftIO . async $ timeout (10 * 1000 * 1000) (handle cfg)
    return ()
  where
    handle :: Internal.Config RPCConfig -> IO ()
    handle rpc =
        lookupFunction rpc functionToCall >>= \case
            Nothing -> do
                let errM = "No provider for: " <> show functionToCall
                debugM logger errM
                writeMessage (Internal.eventQueue rpc) $
                    MsgpackRPC.Response requestId (Left (toObject errM))
            Just (copts, Internal.Stateful c) -> do
                now <- liftIO getCurrentTime
                reply <- liftIO newEmptyTMVarIO
                let q = (recipients . Internal.customConfig) rpc
                liftIO . debugM logger $ "Executing stateful function with ID: " <> show requestId
                atomically' . modifyTVar q $ Map.insert requestId (now, reply)
                writeMessage c $ Request functionToCall requestId (parseParams copts params)

handleNotification :: NeovimEventId -> [Object] -> ConduitT a Void SocketHandler ()
handleNotification eventId@(NeovimEventId str) args = do
    cfg <- lift Internal.ask'
    liftIO (lookupFunction cfg (F str)) >>= \case
        Just (copts, Internal.Stateful c) -> liftIO $ do
            debugM logger $ "Executing function asynchronously: " <> show str
            writeMessage c $ Notification eventId (parseParams copts args)
        Nothing -> do
            liftIO $ debugM logger $ "Handling event: " <> show str
            subscriptions' <- lift $ Internal.asks' Internal.subscriptions
            subscribers <- liftIO $
                atomically $ do
                    s <- readTMVar subscriptions'
                    pure $ fromMaybe [] $ Map.lookup eventId (Internal.byEventId s)
            forM_ subscribers $ \subscription -> liftIO $ subAction subscription args

parseParams :: FunctionalityDescription -> [Object] -> [Object]
parseParams (Function _ _) args = case args of
    -- Defining a function on the remote host creates a function that, that
    -- passes all arguments in a list. At the time of this writing, no other
    -- arguments are passed for such a function.
    --
    -- The function generating the function on neovim side is called:
    -- @remote#define#FunctionOnHost@
    [ObjectArray fArgs] -> fArgs
    _ -> args
parseParams cmd@(Command _ opts) args = case args of
    (ObjectArray _ : _) ->
        let cmdArgs = filter isPassedViaRPC (getCommandOptions opts)
            (c, args') = foldl' createCommandArguments (def, []) $ zip cmdArgs args
         in toObject c : args'
    _ -> parseParams cmd [ObjectArray args]
  where
    isPassedViaRPC :: CommandOption -> Bool
    isPassedViaRPC = \case
        CmdSync{} -> False
        _ -> True

    -- Neovim passes arguments in a special form, depending on the
    -- CommandOption values used to export the (command) function (e.g. via
    -- 'command' or 'command'').
    createCommandArguments ::
        (CommandArguments, [Object]) ->
        (CommandOption, Object) ->
        (CommandArguments, [Object])
    createCommandArguments old@(c, args') = \case
        (CmdRange _, o) ->
            either (const old) (\r -> (c{range = Just r}, args')) $ fromObject o
        (CmdCount _, o) ->
            either (const old) (\n -> (c{count = Just n}, args')) $ fromObject o
        (CmdBang, o) ->
            either (const old) (\b -> (c{bang = Just b}, args')) $ fromObject o
        (CmdNargs "*", ObjectArray os) ->
            -- CommandArguments -> [String] -> Neovim r st a
            (c, os)
        (CmdNargs "+", ObjectArray (o : os)) ->
            -- CommandArguments -> String -> [String] -> Neovim r st a
            (c, o : [ObjectArray os])
        (CmdNargs "?", ObjectArray [o]) ->
            -- CommandArguments -> Maybe String -> Neovim r st a
            (c, [toObject (Just o)])
        (CmdNargs "?", ObjectArray []) ->
            -- CommandArguments -> Maybe String -> Neovim r st a
            (c, [toObject (Nothing :: Maybe Object)])
        (CmdNargs "0", ObjectArray []) ->
            -- CommandArguments -> Neovim r st a
            (c, [])
        (CmdNargs "1", ObjectArray [o]) ->
            -- CommandArguments -> String -> Neovim r st a
            (c, [o])
        (CmdRegister, o) ->
            either (const old) (\r -> (c{register = Just r}, args')) $ fromObject o
        _ -> old
parseParams Autocmd{} args = case args of
    [ObjectArray fArgs] -> fArgs
    _ -> args
