{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    StartupConfig,
    wrapPlugin,
    NeovimPlugin,
    Plugin(..),
    Synchronous(..),
    CommandOption(..),

    addAutocmd,

    registerPlugin,
    ) where

import           Neovim.API.String
import           Neovim.Classes
import           Neovim.Config
import           Neovim.Context
import           Neovim.Context.Internal      (FunctionType (..), runNeovimInternal)
import qualified Neovim.Context.Internal      as Internal
import           Neovim.Plugin.Classes        hiding (register)
import           Neovim.Plugin.Internal
import           Neovim.Plugin.IPC.Classes
import qualified Neovim.Plugin.Startup        as Plugin
import           Neovim.RPC.FunctionCall

import           Control.Applicative
import           Control.Monad                (foldM, void)
import           Control.Monad.Trans.Resource hiding (register)
import           Data.ByteString              (ByteString)
import           Data.ByteString.UTF8         (toString)
import           Data.Foldable                (forM_)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes)
import           Data.MessagePack
import           Data.Traversable             (forM)
import           System.Log.Logger
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>), Pretty(..), text)
import           UnliftIO.Exception           (SomeException, try)
import           UnliftIO.Async               (Async, async, race)
import           UnliftIO.STM
import           UnliftIO.Concurrent          (threadDelay)

import           Prelude


logger :: String
logger = "Neovim.Plugin"


type StartupConfig = Plugin.StartupConfig NeovimConfig


startPluginThreads :: Internal.Config StartupConfig
                   -> [Neovim StartupConfig NeovimPlugin]
                   -> IO (Either Doc ([FunctionMapEntry],[Async ()]))
startPluginThreads cfg = runNeovimInternal return cfg . foldM go ([], [])
  where
    go :: ([FunctionMapEntry], [Async ()])
       -> Neovim StartupConfig NeovimPlugin
       -> Neovim StartupConfig ([FunctionMapEntry], [Async ()])
    go (es, tids) iop = do
        NeovimPlugin p <- iop
        (es', tid) <- registerStatefulFunctionality p

        return $ (es ++ es', tid:tids)


-- | Callthe vimL functions to define a function, command or autocmd on the
-- neovim side. Returns 'True' if registration was successful.
--
-- Note that this does not have any effect on the side of /nvim-hs/.
registerWithNeovim :: FunctionalityDescription -> Neovim anyEnv Bool
registerWithNeovim = \case
    Function (F functionName) s -> do
        pName <- getProviderName
        let (defineFunction, host) = either
                (\n -> ("remote#define#FunctionOnHost", toObject n))
                (\c -> ("remote#define#FunctionOnChannel", toObject c))
                pName
        ret <- vim_call_function defineFunction $
            host +: functionName +: s +: functionName +: (Map.empty :: Dictionary) +: []

        case ret of
            Left e -> do
                liftIO . errorM logger $
                    "Failed to register function: " ++ show functionName ++ show e
                return False
            Right _ -> do
                liftIO . debugM logger $
                    "Registered function: " ++ show functionName
                return True

    Command (F functionName) copts -> do
        let sync = case getCommandOptions copts of
                    -- This works because CommandOptions are sorted and CmdSync is
                    -- the smallest element in the sorting
                    (CmdSync s:_) -> s
                    _             -> Sync

        pName <- getProviderName
        let (defineFunction, host) = either
                (\n -> ("remote#define#CommandOnHost", toObject n))
                (\c -> ("remote#define#CommandOnChannel", toObject c))
                pName
        ret <- vim_call_function defineFunction $
                    host +: functionName +: sync +: functionName +: copts +: []

        case ret of
            Left e -> do
                liftIO . errorM logger $
                    "Failed to register command: " ++ show functionName ++ show e
                return False
            Right _ -> do
                liftIO . debugM logger $
                    "Registered command: " ++ show functionName
                return True

    Autocmd acmdType (F functionName) opts -> do
        pName <- getProviderName
        let (defineFunction, host) = either
                (\n -> ("remote#define#AutocmdOnHost", toObject n))
                (\c -> ("remote#define#AutocmdOnChannel", toObject c))
                pName
        ret <- vim_call_function defineFunction $
                    host +:  functionName +:  Async  +:  acmdType  +:  opts +: []
        case ret of
            Left e -> do
                liftIO . errorM logger $
                    "Failed to register autocmd: " ++ show functionName ++ show e
                return False
            Right _ -> do
                liftIO . debugM logger $
                    "Registered autocmd: " ++ show functionName
                return True


-- | Return or retrive the provider name that the current instance is associated
-- with on the neovim side.
getProviderName :: Neovim env (Either String Int)
getProviderName = do
    mp <- Internal.asks' Internal.providerName
    (liftIO . atomically . tryReadTMVar) mp >>= \case
        Just p ->
            return p

        Nothing -> do
            api <- nvim_get_api_info
            case api of
                Right (i:_) -> do
                    case fromObject i :: Either Doc Int of
                      Left _ ->
                          err "Expected an integral value as the first argument of nvim_get_api_info"
                      Right channelId -> do
                          liftIO . atomically . putTMVar mp . Right $ fromIntegral channelId
                          return . Right $ fromIntegral channelId

                _ ->
                    err "Could not determine provider name."


registerFunctionality :: FunctionalityDescription
                      -> ([Object] -> Neovim env Object)
                      -> Neovim env (Maybe (FunctionMapEntry, Either (Neovim anyEnv ()) ReleaseKey))
registerFunctionality d f = Internal.asks' Internal.pluginSettings >>= \case
    Nothing -> do
        liftIO $ errorM logger "Cannot register functionality in this context."
        return Nothing

    Just (Internal.StatefulSettings reg q m) ->
        reg d f q m >>= \case
            Just e -> do
                -- Redefine fields so that it gains a new type
                cfg <- Internal.retypeConfig () <$> Internal.ask'
                rk <- fst <$> allocate (return ()) (free cfg (fst e))
                return $ Just (e, Right rk)

            Nothing ->
                return Nothing

  where
    freeFun = \case
        Autocmd event _ AutocmdOptions{..} -> do
            void . vim_command . unwords $ catMaybes
                    [ Just "autocmd!", acmdGroup
                    , Just (toString event) , Just acmdPattern
                    ]

        Command{} ->
            liftIO $ warningM logger "Free not implemented for commands."

        Function{} ->
            liftIO $ warningM logger "Free not implemented for functions."


    free cfg = const . void . liftIO . runNeovimInternal return cfg . freeFun


registerInGlobalFunctionMap :: FunctionMapEntry -> Neovim env ()
registerInGlobalFunctionMap e = do
    liftIO . debugM logger $ "Adding function to global function map." ++ show (fst e)
    funMap <- Internal.asks' Internal.globalFunctionMap
    liftIO . atomically $ do
        m <- takeTMVar funMap
        putTMVar funMap $ Map.insert ((name . fst) e) e m
    liftIO . debugM logger $ "Added function to global function map." ++ show (fst e)

registerPlugin
    :: (FunctionMapEntry -> Neovim env ())
    -> FunctionalityDescription
    -> ([Object] -> Neovim env Object)
    -> TQueue SomeMessage
    -> TVar (Map FunctionName ([Object] -> Neovim env Object))
    -> Neovim env (Maybe FunctionMapEntry)
registerPlugin reg d f q tm = registerWithNeovim d >>= \case
    True -> do
        let n = name d
            e = (d, Stateful q)
        liftIO . atomically . modifyTVar tm $ Map.insert n f
        reg e
        return (Just e)

    False ->
        return Nothing


-- | Register an autocmd in the current context. This means that, if you are
-- currently in a stateful plugin, the function will be called in the current
-- thread and has access to the configuration and state of this thread. .
--
-- Note that the function you pass must be fully applied.
--
addAutocmd :: ByteString
           -- ^ The event to register to (e.g. BufWritePost)
           -> AutocmdOptions
           -> (Neovim env ())
           -- ^ Fully applied function to register
           -> Neovim env (Maybe (Either (Neovim anyEnv ()) ReleaseKey))
           -- ^ A 'ReleaseKey' if the registration worked
addAutocmd event (opts@AutocmdOptions{..}) f = do
    n <- newUniqueFunctionName
    fmap snd <$> registerFunctionality (Autocmd event n opts) (\_ -> toObject <$> f)


-- | Create a listening thread for events and add update the 'FunctionMap' with
-- the corresponding 'TQueue's (i.e. communication channels).
registerStatefulFunctionality
    :: Plugin env
    -> Neovim anyEnv ([FunctionMapEntry], Async ())
registerStatefulFunctionality (Plugin { environment = env, exports = fs }) = do
    messageQueue <- liftIO newTQueueIO
    route <- liftIO $ newTVarIO Map.empty

    cfg <- Internal.ask'

    let startupConfig = cfg
            { Internal.customConfig = env
            , Internal.pluginSettings = Just $ Internal.StatefulSettings
                (registerPlugin (\_ -> return ())) messageQueue route
            }
    res <- liftIO . runNeovimInternal return startupConfig . forM fs $ \f ->
            registerFunctionality (getDescription f) (getFunction f)
    es <- case res of
        Left e -> err e
        Right a -> return $ catMaybes a

    let pluginThreadConfig = cfg
            { Internal.customConfig = env
            , Internal.pluginSettings = Just $ Internal.StatefulSettings
                (registerPlugin registerInGlobalFunctionMap) messageQueue route
            }

    tid <- liftIO . async . void . runNeovim pluginThreadConfig $ do
                listeningThread messageQueue route

    return (map fst es, tid) -- NB: dropping release functions/keys here


  where
    executeFunction
        :: ([Object] -> Neovim env Object)
        -> [Object]
        -> Neovim env (Either String Object)
    executeFunction f args = try (f args) >>= \case
            Left e -> return . Left $ show (e :: SomeException)
            Right res -> return $ Right res

    timeoutAndLog :: Word ->  FunctionName -> Neovim anyEnv String
    timeoutAndLog seconds functionName = do
        threadDelay (fromIntegral seconds * 1000 * 1000)
        return . show $
            pretty functionName <+> text "has been aborted after"
            <+> text (show seconds) <+> text "seconds"


    listeningThread :: TQueue SomeMessage
                    -> TVar (Map FunctionName ([Object] -> Neovim env Object))
                    -> Neovim env ()
    listeningThread q route = do
        msg <- liftIO . atomically $ readTQueue q

        forM_ (fromMessage msg) $ \req@Request{..} -> do
            route' <- liftIO $ readTVarIO route
            forM_ (Map.lookup reqMethod route') $ \f -> do
                respond req . either Left id =<< race
                    (timeoutAndLog 10 reqMethod)
                    (executeFunction f reqArgs)

        forM_ (fromMessage msg) $ \Notification{..} -> do
            route' <- liftIO $ readTVarIO route
            forM_ (Map.lookup notMethod route') $ \f ->
                void . async $ do
                    result <- either Left id <$> race
                        (timeoutAndLog 600 notMethod)
                        (executeFunction f notArgs)
                    case result of
                      Left message ->
                          nvim_err_writeln' message
                      Right _ ->
                          return ()


        listeningThread q route

