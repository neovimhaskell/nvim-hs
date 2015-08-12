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
import           Neovim.Plugin.IPC.Classes
import           Neovim.RPC.FunctionCall

import           Control.Applicative
import           Control.Concurrent           (ThreadId, forkIO)
import           Control.Concurrent.STM
import           Control.Monad                (foldM, void)
import           Control.Monad.Catch          (SomeException, try)
import           Control.Monad.Trans.Resource hiding (register)
import           Data.ByteString              (ByteString)
import           Data.Foldable                (forM_)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes)
import           Data.MessagePack
import           Data.Traversable             (forM)
import           System.Log.Logger

import           Prelude


logger :: String
logger = "Neovim.Plugin"


-- TODO Note that there may be high contention on the global function map if a
--      lot of plugins are registered. The earlier version of this module did
--      not have this problem, but it made the types and function way more
--      complicated.  The change maed for autocmd registration allowd sharing of
--      the function details and simplified the code. If you think this should
--      be made more efficient again, feel free to contribute a pull request.


startPluginThreads :: Internal.Config () ()
                   -> [Neovim' NeovimPlugin]
                   -> IO (Either String ([FunctionMapEntry],[ThreadId]))
startPluginThreads cfg = fmap (fmap fst) . runNeovim cfg () . foldM go ([], [])
  where
    go :: ([FunctionMapEntry], [ThreadId])
       -> Neovim' NeovimPlugin
       -> Neovim' ([FunctionMapEntry], [ThreadId])
    go acc iop = do
        NeovimPlugin p <- iop

        (es, tids) <- foldl (\(es, tids) (es', tid) -> (es'++es, tid:tids)) acc
            <$> mapM registerStatefulFunctionality (statefulExports p)

        es' <- forM (exports p) $ \e -> do
            registerInStatelessContext
                (\_ -> return ())
                (getDescription e)
                (getFunction e)

        return $ (catMaybes es' ++ es, tids)


-- | Callthe vimL functions to define a function, command or autocmd on the
-- neovim side. Returns 'True' if registration was successful.
--
-- Note that this does not have any effect on the side of /nvim-hs/.
registerWithNeovim :: FunctionalityDescription -> Neovim anyConfig anyState Bool
registerWithNeovim = \case
    Function (F functionName) s -> do
        pName <- Internal.asks' Internal.providerName
        ret <- wait $ vim_call_function "remote#define#FunctionOnHost"
            [ toObject pName, toObject functionName, toObject s
            , toObject functionName, toObject (Map.empty :: Dictionary)
            ]
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

        pName <- Internal.asks' Internal.providerName
        ret <- wait $ vim_call_function "remote#define#CommandOnHost"
            [ toObject pName, toObject functionName, toObject sync
            , toObject functionName, toObject copts
            ]
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
        pName <- Internal.asks' Internal.providerName
        ret <- wait $ vim_call_function "remote#define#AutocmdOnHost"
            [ toObject pName, toObject functionName, toObject Async
            , toObject acmdType , toObject opts
            ]
        case ret of
            Left e -> do
                liftIO . errorM logger $
                    "Failed to register autocmd: " ++ show functionName ++ show e
                return False
            Right _ -> do
                liftIO . debugM logger $
                    "Registered autocmd: " ++ show functionName
                return True


registerFunctionality :: FunctionalityDescription
                      -> ([Object] -> Neovim r st Object)
                      -> Neovim r st (Maybe (FunctionMapEntry, Either (Neovim anyR anySt ()) ReleaseKey))
registerFunctionality d f = Internal.asks' Internal.pluginSettings >>= \case
    Nothing -> do
        liftIO $ errorM logger "Cannot register functionality in this context."
        return Nothing

    Just (Internal.StatelessSettings reg) ->
        reg d f >>= \case
            Just e -> do
                return $ Just (e, Left (freeFun (fst e)))
            _ ->
                return Nothing

    Just (Internal.StatefulSettings reg q m) ->
        reg d f q m >>= \case
            Just e -> do
                -- Redefine fields so that it gains a new type
                cfg' <- Internal.ask'
                let cfg = cfg'
                        { Internal.customConfig = ()
                        , Internal.pluginSettings = Nothing
                        }
                rk <- fst <$> allocate (return ()) (free cfg (fst e))
                return $ Just (e, Right rk)

            Nothing ->
                return Nothing

  where
    freeFun = \case
        Autocmd event _ AutocmdOptions{..} -> do
            waitErr' "Unregister autocmd" . vim_call_function "autocmd!" $ catMaybes
                    [ toObject <$> acmdGroup, Just (toObject event)
                    , Just (toObject acmdPattern)
                    ]

        Command{} ->
            liftIO $ warningM logger "Free not implemented for commands."

        Function{} ->
            liftIO $ warningM logger "Free not implemented for functions."


    free cfg = const . void . liftIO . runNeovim cfg () . freeFun


registerInStatelessContext
    :: (FunctionMapEntry -> Neovim r st ())
    -> FunctionalityDescription
    -> ([Object] -> Neovim' Object)
    -> Neovim r st (Maybe FunctionMapEntry)
registerInStatelessContext reg d f = registerWithNeovim d >>= \case
    False ->
        return Nothing

    True -> do
        let e = (d, Stateless f)
        reg e
        return $ Just e

registerInGlobalFunctionMap :: FunctionMapEntry -> Neovim r st ()
registerInGlobalFunctionMap e = do
    liftIO . debugM logger $ "Adding function to global function map." ++ show (fst e)
    funMap <- Internal.asks' Internal.globalFunctionMap
    liftIO . atomically $ do
        m <- takeTMVar funMap
        putTMVar funMap $ Map.insert ((name . fst) e) e m
    liftIO . debugM logger $ "Added function to global function map." ++ show (fst e)

registerInStatefulContext
    :: (FunctionMapEntry -> Neovim r st ())
    -> FunctionalityDescription
    -> ([Object] -> Neovim r st Object)
    -> TQueue SomeMessage
    -> TVar (Map FunctionName ([Object] -> Neovim r st Object))
    -> Neovim r st (Maybe FunctionMapEntry)
registerInStatefulContext reg d f q tm = registerWithNeovim d >>= \case
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
-- thread and has access to the configuration and state of this thread. If you
-- need that information, but do not want to block the other functions in this
-- thread, you have to manually fork a thread and make the state you need
-- available there. If you don't care abou the state (or your function has been
-- appield to all the necessary state (e.g. a 'TVar' to share the rusult), then
-- you can also call 'addAutocmd'' which will register a stateless function that
-- only interacts with other threads by means of concurrency abstractions.
--
-- Note that the function you pass must be fully applied.
--
-- Note beside: This function is equivalent to 'addAutocmd'' if called from a
-- stateless plugin thread.
addAutocmd :: ByteString
           -- ^ The event to register to (e.g. BufWritePost)
           -> AutocmdOptions
           -> (Neovim r st ())
           -- ^ Fully applied function to register
           -> Neovim r st (Maybe (Either (Neovim anyR anySt ()) ReleaseKey))
           -- ^ A 'ReleaseKey' if the registration worked
addAutocmd event (opts@AutocmdOptions{..}) f = do
    n <- newUniqueFunctionName
    fmap snd <$> registerFunctionality (Autocmd event n opts) (\_ -> toObject <$> f)


-- | Add a stateless autocmd.
--
-- See 'addAutocmd' for more details.
addAutocmd' :: ByteString
            -> AutocmdOptions
            -> Neovim' ()
            -> Neovim r st (Maybe ReleaseKey)
addAutocmd' event opts f = do
    n <- newUniqueFunctionName
    void $ registerInStatelessContext
                registerInGlobalFunctionMap
                (Autocmd event n opts)
                (\_ -> toObject <$> f)
    return Nothing


-- | Create a listening thread for events and add update the 'FunctionMap' with
-- the corresponding 'TQueue's (i.e. communication channels).
registerStatefulFunctionality
    :: (r, st, [ExportedFunctionality r st])
    -> Neovim anyconfig anyState ([FunctionMapEntry], ThreadId)
registerStatefulFunctionality (r, st, fs) = do
    q <- liftIO newTQueueIO
    route <- liftIO $ newTVarIO Map.empty

    cfg <- Internal.ask'

    let startupConfig = cfg
            { Internal.customConfig = r
            , Internal.pluginSettings = Just $ Internal.StatefulSettings
                (registerInStatefulContext (\_ -> return ())) q route
            }
    res <- liftIO . runNeovim startupConfig st $ forM fs $ \f ->
            registerFunctionality (getDescription f) (getFunction f)
    es <- case res of
        Left e -> err e
        Right (a,_) -> return $ catMaybes a

    let pluginThreadConfig = cfg
            { Internal.customConfig = r
            , Internal.pluginSettings = Just $ Internal.StatefulSettings
                (registerInStatefulContext registerInGlobalFunctionMap) q route
            }

    tid <- liftIO . forkIO . void . runNeovim pluginThreadConfig st $ do
                listeningThread q route

    return (map fst es, tid) -- NB: dropping release functions/keys here

  where
    executeFunction
        :: ([Object] -> Neovim r st Object)
        -> [Object]
        -> Neovim r st (Either String Object)
    executeFunction f args = try (f args) >>= \case
            Left e -> return . Left $ show (e :: SomeException)
            Right res -> return $ Right res

    listeningThread :: TQueue SomeMessage
                    -> TVar (Map FunctionName ([Object] -> Neovim r st Object))
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

