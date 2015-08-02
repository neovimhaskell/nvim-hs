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
    register,
    wrapPlugin,
    NeovimPlugin,
    Plugin(..),
    Synchronous(..),
    CommandOption(..),

    addAutocmd,
    addAutocmd',
    ) where

import           Neovim.API.String
import           Neovim.Classes
import           Neovim.Context
import           Neovim.Context.Internal      (FunctionType (..))
import qualified Neovim.Context.Internal      as Internal
import           Neovim.Plugin.Classes        hiding (register)
import           Neovim.Plugin.Internal
import           Neovim.Plugin.IPC
import           Neovim.Plugin.IPC.Internal
import           Neovim.RPC.FunctionCall

import           Control.Applicative          ((<$>))
import           Control.Concurrent           (ThreadId, forkIO)
import           Control.Concurrent.STM
import           Control.Monad                (foldM, void)
import           Control.Monad.Catch          (SomeException, try)
import           Control.Monad.Trans.Resource hiding (register)
import           Data.Foldable                (forM_)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes)
import           Data.MessagePack
import           Data.Text                    (Text, unpack)
import           Data.Traversable             (forM)
import           System.Log.Logger


logger :: String
logger = "Neovim.Plugin"


-- TODO Note that there may be high contention on the global function map if a
--      lot of plugins are registered. The earlier version of this module did
--      not have this problem, but it made the types and function way more
--      complicated.  The change maed for autocmd registration allowd sharing of
--      the function details and simplified the code. If you think this should
--      be made more efficient again, feel free to contribute a pull request.


startPluginThreads :: Internal.Config () ()
                   -> [IO NeovimPlugin]
                   -> IO (Either String [(NeovimPlugin, [ThreadId])])
startPluginThreads cfg = fmap (fmap fst) . runNeovim cfg () . foldM go []
  where
    go :: [(NeovimPlugin, [ThreadId])]
       -> IO NeovimPlugin
       -> Neovim r () [(NeovimPlugin, [ThreadId])]
    go pluginThreads iop = do
        NeovimPlugin p <- liftIO iop
        tids <- mapM registerStatefulFunctionality (statefulExports p)
        return $ (NeovimPlugin p, tids) : pluginThreads

register :: Internal.Config () ()
         -> [NeovimPlugin]
         -> IO ()
register cfg ps =
    let cfg' = cfg
            { Internal.pluginSettings = Just (Internal.StatelessSettings registerInStatelessContext) }
    in void . runNeovim cfg' () $ do
        forM_ ps $ \(NeovimPlugin p) -> do
            forM_ (exports p) $ \e ->
                registerFunctionality (getDescription e) (getFunction e)


-- | Callthe vimL functions to define a function, command or autocmd on the
-- neovim side. Returns 'True' if registration was successful.
--
-- Note that this does not have any effect on the side of /nvim-hs/.
registerWithNeovim :: FunctionalityDescription -> Neovim anyConfig anyState Bool
registerWithNeovim = \case
    Function functionName s -> do
        pName <- Internal.asks' Internal.providerName
        ret <- wait $ vim_call_function "remote#define#FunctionOnHost"
            [ toObject pName, toObject functionName, toObject s
            , toObject functionName, toObject (Map.empty :: Dictionary)
            ]
        case ret of
            Left e -> do
                liftIO . errorM logger $
                    "Failed to register function: " ++ unpack functionName ++ show e
                return False
            Right _ -> do
                liftIO . debugM logger $
                    "Registered function: " ++ unpack functionName
                return True

    Command functionName copts -> do
        let sync = case getCommandOptions copts of
                    -- This works because CommandOptions are sorted and CmdSync is
                    -- the smallest element in the sorting
                    (CmdSync s:_) -> s
                    _             -> Sync

        pName <- Internal.asks' Internal.providerName
        ret <- wait $ vim_call_function "remote#define#CommandOnHost"
            [ toObject pName, toObject functionName, toObject sync
            , toObject functionName, toObject copts
            ]
        case ret of
            Left e -> do
                liftIO . errorM logger $
                    "Failed to register command: " ++ unpack functionName ++ show e
                return False
            Right _ -> do
                liftIO . debugM logger $
                    "Registered command: " ++ unpack functionName
                return True

    Autocmd acmdType functionName opts -> do
        pName <- Internal.asks' Internal.providerName
        ret <- wait $ vim_call_function "remote#define#AutocmdOnHost"
            [ toObject pName, toObject functionName, toObject Async
            , toObject acmdType , toObject opts
            ]
        case ret of
            Left e -> do
                liftIO . errorM logger $
                    "Failed to register autocmd: " ++ unpack functionName ++ show e
                return False
            Right _ -> do
                liftIO . debugM logger $
                    "Registered autocmd: " ++ unpack functionName
                return True


registerFunctionality :: FunctionalityDescription
                      -> ([Object] -> Neovim r st Object)
                      -> Neovim r st (Maybe Text)
registerFunctionality d f = Internal.asks' Internal.pluginSettings >>= \case
    Nothing -> do
        liftIO $ errorM logger "Cannot register functionality in this context."
        return Nothing

    Just (Internal.StatelessSettings reg) ->
        reg d f

    Just (Internal.StatefulSettings reg q m) ->
        reg d f q m


registerInStatelessContext
    :: FunctionalityDescription
    -> ([Object] -> Neovim' Object)
    -> Neovim r st (Maybe Text)
registerInStatelessContext d f = registerWithNeovim d >>= \case
    False ->
        return Nothing

    True -> do
        let n = name d
        funMap <- Internal.asks' Internal.globalFunctionMap
        liftIO . atomically . modifyTVar funMap $
            Map.insert n (d, Stateless f)
        return (Just n)

registerInStatefulContext
    :: FunctionalityDescription
    -> ([Object] -> Neovim r st Object)
    -> TQueue SomeMessage
    -> TVar (Map Text ([Object] -> Neovim r st Object))
    -> Neovim r st (Maybe Text)
registerInStatefulContext d f q tm = registerWithNeovim d >>= \case
    True -> do
        let n = name d
        liftIO . atomically . modifyTVar tm $ Map.insert n f
        funMap <- Internal.asks' Internal.globalFunctionMap
        liftIO . atomically . modifyTVar funMap $ Map.insert n (d, Stateful q)
        return (Just n)
    False ->
        return Nothing


-- | Register an autocmd in the current context. This means that, if you are
-- currently in a stateful plugin, the function will be called in the current
-- thread and has access to the configuration and state of this thread. If you
-- need that information, but do not want to block the other functoins in this
-- thread, you have to maunally fork a thread and make the state you need
-- available there. If you don't care abou the state (or your function has been
-- appield to all the necessary state (e.g. a 'TVar' to share the rusult), then
-- you can also call 'addAutocmd'' which will register a stateless function that
-- only interacts with other threads by means of concurrency abstractions.
--
-- Note that the function you pass must be fully applied.
--
-- Note that this function is equivalent to 'addAutocmd'' if called from a
-- stateless plugin thread.
addAutocmd :: Text
           -- ^ The event to register to (e.g. BufWritePost)
           -> AutocmdOptions
           -> (Neovim r st ())
           -- ^ Fully applied function to register
           -> Neovim r st (Maybe ReleaseKey)
           -- ^ A 'ReleaseKey' if the registration worked
addAutocmd event (opts@AutocmdOptions{..}) f = do
    n <- newUniqueFunctionName
    mn <- registerFunctionality (Autocmd event n opts) (\_ -> toObject <$> f)
    forM mn $ \registeredName -> do
        -- Redefine fields so that it gains a new type
        cfg' <- Internal.ask'
        let cfg = cfg'
                { Internal.customConfig = ()
                , Internal.pluginSettings = Nothing
                }
        fst <$> allocate (return ()) (free cfg)

  where
    free cfg = const . void . liftIO . runNeovim cfg () $ do
        vim_call_function "autocmd!" $ catMaybes
            [ toObject <$> acmdGroup, Just (toObject event)
            , Just (toObject acmdPattern)
            ]
        return ()


-- | Add a stateless autocmd.
--
-- See 'addAutocmd' for more details.
addAutocmd' :: Text -> AutocmdOptions -> Neovim' () -> Neovim r st (Maybe Text)
addAutocmd' event opts f = do
    n <- newUniqueFunctionName
    registerInStatelessContext (Autocmd event n opts) (\_ -> toObject <$> f)


-- | Create a listening thread for events and add update the 'FunctionMap' with
-- the corresponding 'TQueue's (i.e. communication channels).
registerStatefulFunctionality
    :: (r, st, [ExportedFunctionality r st])
    -> Neovim anyconfig anyState ThreadId
registerStatefulFunctionality (r, st, fs) = do
    q <- liftIO newTQueueIO
    route <- liftIO $ newTVarIO Map.empty
    let internalPluginCfg = Internal.StatefulSettings registerInStatefulContext q route

    cfg <- Internal.ask'
    let pluginThreadConfig = cfg
            { Internal.customConfig = r
            , Internal.pluginSettings = Just internalPluginCfg
            }

    tid <- liftIO . forkIO . void . runNeovim pluginThreadConfig st $ do
                forM_ fs $ \f ->
                    registerFunctionality (getDescription f) (getFunction f)

                listeningThread q route

    return tid

  where
    executeFunction
        :: ([Object] -> Neovim r st Object)
        -> [Object]
        -> Neovim r st (Either String Object)
    executeFunction f args = try (f args) >>= \case
            Left e -> return . Left $ show (e :: SomeException)
            Right res -> return $ Right res

    listeningThread :: TQueue SomeMessage
                    -> TVar (Map Text ([Object] -> Neovim r st Object))
                    -> Neovim r st loop
    listeningThread q route = do
        msg <- liftIO . atomically $ readTQueue q

        forM_ (fromMessage msg) $ \req@Request{..} -> do
            route' <- liftIO $ readTVarIO route
            forM_ (Map.lookup reqMethod route') $ \f ->
                respond req =<< executeFunction f reqArgs

        forM_ (fromMessage msg) $ \Notification{..} -> do
            route' <- liftIO $ readTVarIO route
            forM_ (Map.lookup notMethod route') $ \f ->
                void $ executeFunction f notArgs

        listeningThread q route

