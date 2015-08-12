{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
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

import           Neovim.Classes
import           Neovim.Context
import qualified Neovim.Context.Internal    as Internal
import           Neovim.Plugin.Classes      (CommandArguments (..),
                                             CommandOption (..),
                                             FunctionName (..),
                                             FunctionalityDescription (..),
                                             getCommandOptions)
import           Neovim.Plugin.IPC.Classes
import           Neovim.Plugin              (registerInStatelessContext)
import qualified Neovim.RPC.Classes         as MsgpackRPC
import           Neovim.RPC.Common
import           Neovim.RPC.FunctionCall

import           Control.Applicative
import           Control.Concurrent         (forkIO)
import           Control.Concurrent.STM
import           Control.Monad              (void)
import           Control.Monad.Trans.Class  (lift)
import           Data.Conduit               as C
import           Data.Conduit.Binary
import           Data.Conduit.Cereal
import           Data.Default               (def)
import           Data.Foldable              (foldl', forM_)
import qualified Data.Map                   as Map
import           Data.MessagePack
import           Data.Monoid
import qualified Data.Serialize             (get)
import           System.IO                  (Handle)
import           System.Log.Logger

import           Prelude

logger :: String
logger = "Socket Reader"


type SocketHandler = Neovim RPCConfig ()


-- | This function will establish a connection to the given socket and read
-- msgpack-rpc events from it.
runSocketReader :: Handle
                -> Internal.Config RPCConfig st
                -> IO ()
runSocketReader readableHandle cfg =
    void . runNeovim (Internal.retypeConfig (Internal.customConfig cfg) () cfg) () $ do
        -- addCleanup (cleanUpHandle h) (sourceHandle h)
        -- TODO test whether/how this should be handled
        -- this has been commented out because I think that restarting the
        -- plugin provider should not cause the stdin and stdout handles to be
        -- closed since that would cause neovim to stop the plugin provider (I
        -- think).
        sourceHandle readableHandle
            $= conduitGet Data.Serialize.get
            $$ messageHandlerSink


-- | Sink that delegates the messages depending on their type.
-- <https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md>
messageHandlerSink :: Sink Object SocketHandler ()
messageHandlerSink = awaitForever $ \rpc -> do
    liftIO . debugM logger $ "Received: " <> show rpc
    case fromObject rpc of
        Right (MsgpackRPC.Request (Request fn i ps)) ->
            handleRequestOrNotification (Just i) fn ps

        Right (MsgpackRPC.Response i r) ->
            handleResponse i r

        Right (MsgpackRPC.Notification (Notification fn ps)) ->
            handleRequestOrNotification Nothing fn ps

        Left e -> liftIO . errorM logger $
            "Unhandled rpc message: " <> show e


handleResponse :: Int64 -> Either Object Object -> Sink a SocketHandler ()
handleResponse i result = do
    answerMap <- asks recipients
    mReply <- Map.lookup i <$> liftIO (readTVarIO answerMap)
    case mReply of
        Nothing -> liftIO $ warningM logger
            "Received response but could not find a matching recipient."
        Just (_,reply) -> do
            atomically' . modifyTVar' answerMap $ Map.delete i
            atomically' $ putTMVar reply result

-- | Act upon the received request or notification. The main difference between
-- the two is that a notification does not generate a reply. The distinction
-- between those two cases is done via the first paramater which is 'Maybe' the
-- function call identifier.
handleRequestOrNotification :: Maybe Int64 -> FunctionName -> [Object] -> Sink a SocketHandler ()
handleRequestOrNotification mi m params = do
    cfg <- lift Internal.ask'
    void . liftIO . forkIO $ handle cfg

  where
    lookupFunction
        :: TMVar Internal.FunctionMap
        -> STM (Maybe (FunctionalityDescription, Internal.FunctionType))
    lookupFunction funMap = Map.lookup m <$> readTMVar funMap

    handle :: Internal.Config RPCConfig () -> IO ()
    handle rpc = atomically (lookupFunction (Internal.globalFunctionMap rpc)) >>= \case

        Nothing -> do
            let errM = "No provider for: " <> show m
            debugM logger errM
            forM_ mi $ \i -> atomically' . writeTQueue (Internal.eventQueue rpc)
                . SomeMessage $ MsgpackRPC.Response i (Left (toObject errM))
        Just (copts, Internal.Stateless f) -> do
            liftIO . debugM logger $ "Executing stateless function with ID: " <> show mi
            -- Stateless function: Create a boring state object for the
            -- Neovim context.
            -- drop the state of the result with (fmap fst <$>)
            let rpc' = rpc
                    { Internal.customConfig = ()
                    , Internal.pluginSettings = Just . Internal.StatelessSettings $
                        registerInStatelessContext (\_ -> return ())
                    }
            res <- fmap fst <$> runNeovim rpc' () (f $ parseParams copts params)
            -- Send the result to the event handler
            forM_ mi $ \i -> atomically' . writeTQueue (Internal.eventQueue rpc)
                . SomeMessage . MsgpackRPC.Response i $ either (Left . toObject) Right res
        Just (copts, Internal.Stateful c) -> do
            now <- liftIO getCurrentTime
            reply <- liftIO newEmptyTMVarIO
            let q = (recipients . Internal.customConfig) rpc
            liftIO . debugM logger $ "Executing stateful function with ID: " <> show mi
            case mi of
                Just i -> do
                    atomically' . modifyTVar q $ Map.insert i (now, reply)
                    atomically' . writeTQueue c . SomeMessage $
                        Request m i (parseParams copts params)

                Nothing ->
                    atomically' . writeTQueue c . SomeMessage $
                        Notification m (parseParams copts params)


parseParams :: FunctionalityDescription -> [Object] -> [Object]
parseParams (Function _ _) args = case args of
    -- Defining a function on the remote host creates a function that, that
    -- passes all arguments in a list. At the time of this writing, no other
    -- arguments are passed for such a function.
    --
    -- The function generating the function on neovim side is called:
    -- @remote#define#FunctionOnHost@
    [ObjectArray fArgs] -> fArgs
    _                   -> args

parseParams cmd@(Command _ opts) args = case args of
    (ObjectArray _ : _) ->
        let cmdArgs = filter isPassedViaRPC (getCommandOptions opts)
            (c,args') =
                foldl' createCommandArguments (def, []) $
                    zip cmdArgs args
        in toObject c : args'

    _ -> parseParams cmd $ [ObjectArray args]
  where
    isPassedViaRPC :: CommandOption -> Bool
    isPassedViaRPC = \case
        CmdSync{}  -> False
        _          -> True

    -- Neovim passes arguments in a special form, depending on the
    -- CommandOption values used to export the (command) function (e.g. via
    -- 'command' or 'command'').
    createCommandArguments :: (CommandArguments, [Object])
                           -> (CommandOption, Object)
                           -> (CommandArguments, [Object])
    createCommandArguments old@(c, args') = \case
        (CmdRange _, o) ->
            either (const old) (\r -> (c { range = Just r }, args')) $ fromObject o

        (CmdCount _, o) ->
            either (const old) (\n -> (c { count = Just n }, args')) $ fromObject o

        (CmdBang, o) ->
            either (const old) (\b -> (c { bang = Just b }, args')) $ fromObject o

        (CmdNargs "*", ObjectArray os) ->
            -- CommandArguments -> [String] -> Neovim r st a
            (c, os)
        (CmdNargs "+", ObjectArray (o:os)) ->
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
            either (const old) (\r -> (c { register = Just r }, args')) $ fromObject o

        _ -> old

parseParams (Autocmd _ _ _) args = case args of
    [ObjectArray fArgs] -> fArgs
    _ -> args

