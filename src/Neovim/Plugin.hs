{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Neovim.API.String
    ( nvim_err_writeln, nvim_get_api_info, vim_call_function )
import Neovim.Classes
    ( (<+>),
      Doc,
      AnsiStyle,
      Pretty(pretty),
      NvimObject(toObject, fromObject),
      Dictionary,
      (+:) )
import Neovim.Context
    ( MonadIO(liftIO),
      NeovimException,
      newUniqueFunctionName,
      runNeovim,
      FunctionMapEntry,
      Neovim,
      err )
import Neovim.Context.Internal (
    FunctionType (..),
    runNeovimInternal,
 )
import qualified Neovim.Context.Internal as Internal
import Neovim.Plugin.Classes
    ( HasFunctionName(nvimMethod),
      FunctionName(..),
      NeovimEventId(NeovimEventId),
      Synchronous(..),
      CommandOption(..),
      CommandOptions(getCommandOptions),
      AutocmdOptions(AutocmdOptions),
      FunctionalityDescription(..),
      NvimMethod(..) )
import Neovim.Plugin.IPC.Classes
    ( Notification(Notification),
      Request(Request),
      Message(fromMessage),
      SomeMessage,
      readSomeMessage )
import Neovim.Plugin.Internal
    ( NeovimPlugin(..),
      Plugin(..),
      getDescription,
      getFunction,
      wrapPlugin )
import Neovim.RPC.FunctionCall ( respond )

import Control.Monad (foldM, void)
import Data.Foldable (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either (rights)
import Data.MessagePack ( Object )
import Data.Text (Text)
import Data.Traversable (forM)
import System.Log.Logger ( debugM, errorM )
import UnliftIO.Async (Async, async, race)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (SomeException, catch, try)
import UnliftIO.STM
    ( TVar,
      putTMVar,
      takeTMVar,
      tryReadTMVar,
      modifyTVar,
      TQueue,
      atomically,
      newTQueueIO,
      newTVarIO,
      readTVarIO )

import Prelude

logger :: String
logger = "Neovim.Plugin"

startPluginThreads ::
    Internal.Config () ->
    [Neovim () NeovimPlugin] ->
    IO (Either (Doc AnsiStyle) ([FunctionMapEntry], [Async ()]))
startPluginThreads cfg = runNeovimInternal return cfg . foldM go ([], [])
  where
    go ::
        ([FunctionMapEntry], [Async ()]) ->
        Neovim () NeovimPlugin ->
        Neovim () ([FunctionMapEntry], [Async ()])
    go (es, tids) iop = do
        NeovimPlugin p <- iop
        (es', tid) <- registerStatefulFunctionality p

        return (es ++ es', tid : tids)

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
                return False
            logSuccess = do
                liftIO . debugM logger $
                    "Registered function: " ++ show functionName
                return True

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
                return False
            logSuccess = do
                liftIO . debugM logger $
                    "Registered command: " ++ show functionName
                return True
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
                return False
            logSuccess = do
                liftIO . debugM logger $
                    "Registered autocmd: " ++ show functionName
                return True
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
            return p
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
                            return . Right $ fromIntegral channelId

registerFunctionality ::
    FunctionalityDescription ->
    ([Object] -> Neovim env Object) ->
    Neovim env (Either (Doc AnsiStyle) FunctionMapEntry)
registerFunctionality d f = do
    Internal.asks' Internal.pluginSettings >>= \case
        Nothing -> do
            let msg = "Cannot register functionality in this context."
            liftIO $ errorM logger msg
            return $ Left $ pretty msg
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
    TQueue SomeMessage ->
    TVar (Map NvimMethod ([Object] -> Neovim env Object)) ->
    Neovim env (Maybe FunctionMapEntry)
registerPlugin reg d f q tm =
    registerWithNeovim d >>= \case
        True -> do
            let n = nvimMethod d
                e = (d, Stateful q)
            liftIO . atomically . modifyTVar tm $ Map.insert n f
            reg e
            return (Just e)
        False ->
            return Nothing

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
    messageQueue <- liftIO newTQueueIO
    route <- liftIO $ newTVarIO Map.empty
    subscribers <- liftIO $ newTVarIO []

    cfg <- Internal.ask'

    let startupConfig =
            cfg
                { Internal.customConfig = env
                , Internal.pluginSettings =
                    Just $
                        Internal.StatefulSettings
                            (registerPlugin (\_ -> return ()))
                            messageQueue
                            route
                }
    res <- liftIO . runNeovimInternal return startupConfig . forM fs $ \f ->
        registerFunctionality (getDescription f) (getFunction f)
    es <- case res of
        Left e -> err e
        Right a -> return $ rights a

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

    return (es, tid) -- NB: dropping release functions/keys here
  where
    executeFunction ::
        ([Object] -> Neovim env Object) ->
        [Object] ->
        Neovim env (Either String Object)
    executeFunction f args =
        try (f args) >>= \case
            Left e -> return . Left $ show (e :: SomeException)
            Right res -> return $ Right res

    killAfterSeconds :: Word -> Neovim anyEnv ()
    killAfterSeconds seconds = threadDelay (fromIntegral seconds * 1000 * 1000)

    timeoutAndLog :: Word -> FunctionName -> Neovim anyEnv String
    timeoutAndLog seconds functionName = do
        killAfterSeconds seconds
        return . show $
            pretty functionName <+> "has been aborted after"
                <+> pretty seconds
                <+> "seconds"

    listeningThread ::
        TQueue SomeMessage ->
        TVar (Map NvimMethod ([Object] -> Neovim env Object)) ->
        TVar [Notification -> Neovim env ()] ->
        Neovim env ()
    listeningThread q route subscribers = do
        msg <- readSomeMessage q

        forM_ (fromMessage msg) $ \req@(Request fun@(F methodName) _ args) -> do
            let method = NvimMethod methodName
            route' <- liftIO $ readTVarIO route
            forM_ (Map.lookup method route') $ \f -> do
                respond req . either Left id
                    =<< race
                        (timeoutAndLog 10 fun)
                        (executeFunction f args)

        forM_ (fromMessage msg) $ \notification@(Notification (NeovimEventId methodName) args) -> do
            let method = NvimMethod methodName
            route' <- liftIO $ readTVarIO route
            forM_ (Map.lookup method route') $ \f ->
                void . async $ do
                    result <- either Left id <$> race
                        (timeoutAndLog 600 (F methodName))
                        (executeFunction f args)
                    case result of
                      Left message ->
                          nvim_err_writeln message
                      Right _ ->
                          return ()

            subscribers' <- liftIO $ readTVarIO subscribers
            forM_ subscribers' $ \subscriber ->
                async $ void $ race (subscriber notification) (killAfterSeconds 10)

        listeningThread q route subscribers
