{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Neovim.Plugin
Description :  Plugin and functionality registration code
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC
-}
module Neovim.Plugin (
    startPluginThreads,
    wrapPlugin,
    NeovimPlugin,
    Plugin (..),
    Synchronous (..),
    CommandOption (..),
    addAutocmd,
    registerPlugin,
    registerFunctionality,
    getProviderName,
) where

import Neovim.API.Text (
    nvim_err_writeln,
    nvim_get_api_info,
    vim_call_function,
 )
import Neovim.Classes (
    AnsiStyle,
    Dictionary,
    Doc,
    NvimObject (fromObject, toObject),
    Pretty (pretty),
    (+:),
    (<+>),
 )
import Neovim.Context (
    FunctionMapEntry,
    MonadIO (liftIO),
    Neovim,
    NeovimException,
    err,
    newUniqueFunctionName,
    runNeovim,
 )
import Neovim.Context.Internal (
    runNeovimInternal,
 )
import qualified Neovim.Context.Internal as Internal
import Neovim.Internal.RPC
import Neovim.Classes (docToText)
import Neovim.Plugin.Classes (
    AutocmdOptions (AutocmdOptions),
    CommandOption (..),
    CommandOptions (getCommandOptions),
    FunctionName (..),
    FunctionalityDescription (..),
    HasFunctionName (nvimMethod),
    NeovimEventId (NeovimEventId),
    NvimMethod (..),
    Synchronous (..),
 )
import Neovim.Plugin.Internal (
    NeovimPlugin (..),
    Plugin (..),
    getDescription,
    getFunction,
    wrapPlugin,
 )
import Neovim.RPC.FunctionCall (respond)

import Control.Monad (foldM, void)
import Data.Either (rights)
import Data.Foldable (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.MessagePack (Object)
import Data.Text (Text)
import Data.Traversable (forM)
import System.Log.Logger (debugM, errorM)
import UnliftIO.Async (Async, async, race)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (SomeException, catch, try)
import UnliftIO.STM (
    TQueue,
    TVar,
    atomically,
    modifyTVar,
    newTQueueIO,
    newTVarIO,
    putTMVar,
    readTVarIO,
    takeTMVar,
    tryReadTMVar,
 )

import Neovim.Internal.RPC (MsgpackNotification (notificationEvent))
import Prettyprinter.Render.Terminal (renderStrict)
import Prelude

logger :: String
logger = "Neovim.Plugin"

startPluginThreads ::
    Internal.Config () ->
    [Neovim () NeovimPlugin] ->
    IO (Either (Doc AnsiStyle) ([FunctionMapEntry], [Async ()]))
startPluginThreads cfg = runNeovimInternal pure cfg . foldM go ([], [])
  where
    go ::
        ([FunctionMapEntry], [Async ()]) ->
        Neovim () NeovimPlugin ->
        Neovim () ([FunctionMapEntry], [Async ()])
    go (es, tids) iop = do
        NeovimPlugin p <- iop
        (es', tid) <- registerStatefulFunctionality p

        pure (es ++ es', tid : tids)

{- | Call the vimL functions to define a function, command or autocmd on the
 neovim side. Returns 'True' if registration was successful.

 Note that this does not have any effect on the side of /nvim-hs/.
-}
registerWithNeovim :: FunctionalityDescription -> Neovim anyEnv Bool
registerWithNeovim = \case
    func@(Function (F functionName) s) -> do
        pName <- getProviderName
        let (defineFunction, host) =
                either
                    (\n -> ("remote#define#FunctionOnHost", toObject n))
                    (\c -> ("remote#define#FunctionOnChannel", toObject c))
                    pName
            reportError (e :: NeovimException) = do
                liftIO . errorM logger $
                    "Failed to register function: " ++ show functionName ++ show e
                pure False
            logSuccess = do
                liftIO . debugM logger $
                    "Registered function: " ++ show functionName
                pure True

        flip catch reportError $ do
            void $
                vim_call_function defineFunction $
                    host +: nvimMethodName (nvimMethod func) +: s +: functionName +: (Map.empty :: Dictionary) +: []
            logSuccess
    cmd@(Command (F functionName) copts) -> do
        let sync = case getCommandOptions copts of
                -- This works because CommandOptions are sorted and CmdSync is
                -- the smallest element in the sorting
                (CmdSync s : _) -> s
                _ -> Sync

        pName <- getProviderName
        let (defineFunction, host) =
                either
                    (\n -> ("remote#define#CommandOnHost", toObject n))
                    (\c -> ("remote#define#CommandOnChannel", toObject c))
                    pName
            reportError (e :: NeovimException) = do
                liftIO . errorM logger $
                    "Failed to register command: " ++ show functionName ++ show e
                pure False
            logSuccess = do
                liftIO . debugM logger $
                    "Registered command: " ++ show functionName
                pure True
        flip catch reportError $ do
            void $
                vim_call_function defineFunction $
                    host +: nvimMethodName (nvimMethod cmd) +: sync +: functionName +: copts +: []
            logSuccess
    Autocmd acmdType (F functionName) sync opts -> do
        pName <- getProviderName
        let (defineFunction, host) =
                either
                    (\n -> ("remote#define#AutocmdOnHost", toObject n))
                    (\c -> ("remote#define#AutocmdOnChannel", toObject c))
                    pName
            reportError (e :: NeovimException) = do
                liftIO . errorM logger $
                    "Failed to register autocmd: " ++ show functionName ++ show e
                pure False
            logSuccess = do
                liftIO . debugM logger $
                    "Registered autocmd: " ++ show functionName
                pure True
        flip catch reportError $ do
            void $
                vim_call_function defineFunction $
                    host +: functionName +: sync +: acmdType +: opts +: []
            logSuccess

{- | Return or retrive the provider name that the current instance is associated
 with on the neovim side.
-}
getProviderName :: Neovim env (Either String Int)
getProviderName = do
    mp <- Internal.asks' Internal.providerName
    (liftIO . atomically . tryReadTMVar) mp >>= \case
        Just p ->
            pure p
        Nothing -> do
            api <- nvim_get_api_info
            case api of
                [] -> err "empty nvim_get_api_info"
                (i : _) -> do
                    case fromObject i :: Either (Doc AnsiStyle) Int of
                        Left _ ->
                            err $
                                "Expected an integral value as the first"
                                    <+> "argument of nvim_get_api_info"
                        Right channelId -> do
                            liftIO . atomically . putTMVar mp . Right $ fromIntegral channelId
                            pure . Right $ fromIntegral channelId

registerFunctionality ::
    FunctionalityDescription ->
    ([Object] -> Neovim env Object) ->
    Neovim env (Either (Doc AnsiStyle) FunctionMapEntry)
registerFunctionality d f = do
    Internal.asks' Internal.pluginSettings >>= \case
        Nothing -> do
            let msg = "Cannot register functionality in this context."
            liftIO $ errorM logger msg
            pure $ Left $ pretty msg
        Just (Internal.StatefulSettings reg q m) ->
            reg d f q m >>= \case
                Just e -> do
                    pure $ Right e
                Nothing ->
                    pure $ Left ""

registerInGlobalFunctionMap :: FunctionMapEntry -> Neovim env ()
registerInGlobalFunctionMap e = do
    liftIO . debugM logger $ "Adding function to global function map." ++ show (fst e)
    funMap <- Internal.asks' Internal.globalFunctionMap
    liftIO . atomically $ do
        m <- takeTMVar funMap
        putTMVar funMap $ Map.insert ((nvimMethod . fst) e) e m
    liftIO . debugM logger $ "Added function to global function map." ++ show (fst e)

registerPlugin ::
    (FunctionMapEntry -> Neovim env ()) ->
    FunctionalityDescription ->
    ([Object] -> Neovim env Object) ->
    MsgpackRpcQueue ->
    TVar (Map NvimMethod ([Object] -> Neovim env Object)) ->
    Neovim env (Maybe FunctionMapEntry)
registerPlugin reg d f q tm =
    registerWithNeovim d >>= \case
        True -> do
            let n = nvimMethod d
                e = (d, Stateful q)
            liftIO . atomically . modifyTVar tm $ Map.insert n f
            reg e
            pure (Just e)
        False ->
            pure Nothing

{- | Register an autocmd in the current context. This means that, if you are
 currently in a stateful plugin, the function will be called in the current
 thread and has access to the configuration and state of this thread. .

 Note that the function you pass must be fully applied.
-}
addAutocmd ::
    -- | The event to register to (e.g. BufWritePost)
    Text ->
    Synchronous ->
    AutocmdOptions ->
    -- | Fully applied function to register
    Neovim env () ->
    -- | A 'ReleaseKey' if the registration worked
    Neovim env (Either (Doc AnsiStyle) FunctionMapEntry)
addAutocmd event s opts@AutocmdOptions{} f = do
    n <- newUniqueFunctionName
    registerFunctionality (Autocmd event n s opts) (\_ -> toObject <$> f)

{- | Create a listening thread for events and add update the 'FunctionMap' with
 the corresponding 'TQueue's (i.e. communication channels).
-}
registerStatefulFunctionality ::
    Plugin env ->
    Neovim anyEnv ([FunctionMapEntry], Async ())
registerStatefulFunctionality (Plugin{environment = env, exports = fs}) = do
    messageQueue <- newMsgpackRcpQueue
    route <- liftIO $ newTVarIO Map.empty
    subscribers <- liftIO $ newTVarIO []

    cfg <- Internal.ask'

    let startupConfig =
            cfg
                { Internal.customConfig = env
                , Internal.pluginSettings =
                    Just $
                        Internal.StatefulSettings
                            (registerPlugin (\_ -> pure ()))
                            messageQueue
                            route
                }
    res <- liftIO . runNeovimInternal pure startupConfig . forM fs $ \f ->
        registerFunctionality (getDescription f) (getFunction f)
    es <- case res of
        Left e -> err e
        Right a -> pure $ rights a

    let pluginThreadConfig =
            cfg
                { Internal.customConfig = env
                , Internal.pluginSettings =
                    Just $
                        Internal.StatefulSettings
                            (registerPlugin registerInGlobalFunctionMap)
                            messageQueue
                            route
                }

    tid <- liftIO . async . void . runNeovim pluginThreadConfig $ do
        listeningThread messageQueue route subscribers

    pure (es, tid) -- NB: dropping release functions/keys here
  where
    executeFunction ::
        ([Object] -> Neovim env Object) ->
        [Object] ->
        Neovim env (Either Text Object)
    executeFunction f args =
        try (f args) >>= \case
            Left e -> pure . Left $ docToText $ pretty (e :: SomeException)
            Right res -> pure $ Right res

    killAfterSeconds :: Word -> Neovim anyEnv ()
    killAfterSeconds seconds = threadDelay (fromIntegral seconds * 1000 * 1000)

    timeoutAndLog :: Word -> FunctionName -> Neovim anyEnv Text
    timeoutAndLog seconds functionName = do
        killAfterSeconds seconds
        pure . docToText $
            pretty functionName
                <+> "has been aborted after"
                <+> pretty seconds
                <+> "seconds"

    listeningThread ::
        MsgpackRpcQueue ->
        TVar (Map NvimMethod ([Object] -> Neovim env Object)) ->
        TVar [MsgpackNotification -> Neovim env ()] ->
        Neovim env ()
    listeningThread q route subscribers = do
        readBlockingFromMsgpackRpcQueue >>= \case
            Request request@MsgpackRequest{..} -> do
                route' <- liftIO $ readTVarIO route
                forM_ (Map.lookup requestMethod route') $ \f -> do
                    respond request . either Left id
                        =<< race
                            (timeoutAndLog 10 (name requestMethod))
                            (executeFunction f requestArguments)
            Notification
                notification@MsgpackNotification
                    { notificationEvent = NeovimEventId eventId
                    , notificationArguments = notificationArguments
                    } -> do
                    let method = NvimMethod eventId
                    route' <- liftIO $ readTVarIO route
                    forM_ (Map.lookup method route') $ \f ->
                        void . async $ do
                            result <-
                                either Left id
                                    <$> race
                                        (timeoutAndLog 600 (name notificationEvent))
                                        (executeFunction f notificationArguments)
                            case result of
                                Left message ->
                                    nvim_err_writeln message
                                Right _ ->
                                    pure ()

                    subscribers' <- liftIO $ readTVarIO subscribers
                    forM_ subscribers' $ \subscriber ->
                        async $ void $ race (subscriber notification) (killAfterSeconds 10)
            Response _ -> pure ()

        listeningThread q route subscribers
