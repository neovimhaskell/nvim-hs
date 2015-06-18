{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{- |
Module      :  Neovim.Plugin
Description :
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
    ) where

import           Neovim.API.Context
import           Neovim.API.IPC
import           Neovim.API.String
import           Neovim.Plugin.Classes
import           Neovim.RPC.Common
import           Neovim.RPC.FunctionCall

import           Control.Concurrent       (ThreadId)
import           Control.Concurrent.STM
import           Control.Exception.Lifted (SomeException, try)
import           Control.Monad            (foldM, void, forM)
import qualified Control.Monad.Reader     as R
import           Data.Foldable            (forM_)
import qualified Data.Map                 as Map
import           Data.MessagePack
import           Data.Text                (unpack)
import           System.Log.Logger

logger :: String
logger = "Neovim.Plugin"

startPluginThreads :: ConfigWrapper r
                   -> [IO NeovimPlugin]
                   -> IO (Either String [(NeovimPlugin, [(ThreadId, [(FunctionalityDescription, FunctionType)])])])
startPluginThreads cfg = fmap (fmap fst) . runNeovim cfg () . foldM go []
  where
    go :: [(NeovimPlugin, [(ThreadId, [(FunctionalityDescription, FunctionType)])])]
       -> IO NeovimPlugin
       -> Neovim r () [(NeovimPlugin, [(ThreadId, [(FunctionalityDescription, FunctionType)])])]
    go pluginThreads iop = do
        NeovimPlugin p <- liftIO iop
        tids <- mapM registerStatefulFunctionality (statefulExports p)
        return $ (NeovimPlugin p, tids) : pluginThreads

register :: ConfigWrapper (TMVar FunctionMap)
         -> [(NeovimPlugin, [(ThreadId, [(FunctionalityDescription, FunctionType)])])]
         -> IO ()
register (cfg@ConfigWrapper{ customConfig = sem }) ps = void . runNeovim cfg () $ do
    registeredFunctions <- forM ps $ \(NeovimPlugin p, tffs) -> do
        let statefulFunctionsToRegister = concatMap snd tffs
            statelessFunctionsToRegister = map (\ef -> (functionalityDescription ef, (Stateless . functionalityFunction) ef)) $ exports p
            functionsToRegister = statefulFunctionsToRegister ++ statelessFunctionsToRegister
        mapM_ (registerWithNeovim . fst) $ functionsToRegister
        return $ map (\(d, f) -> (name d, f)) functionsToRegister
    liftIO . atomically . putTMVar sem . Map.fromList $ concat registeredFunctions

registerWithNeovim :: FunctionalityDescription -> Neovim customConfig () ()
registerWithNeovim description = case description of
    Function functionName s -> do
        pName <- R.asks _providerName
        let sync = case s of
                Sync -> "1"
                Async -> "0"
        ret <- wait . vim_command $ concat
            [ "call remote#define#FunctionOnHost('" , pName ,"', '"
            , unpack functionName, "', ", sync, ",'", unpack functionName, "',{})"
            ]
        case ret of
            Left e -> do
                liftIO . errorM logger $
                    "Failed to register function: " ++ unpack functionName ++ show e
            Right _ -> do
                liftIO $ debugM logger $ "Registered function: " ++ unpack functionName
    Command{} -> error "TODO register commands"
    AutoCmd{} -> error "TODO register autocmds"

-- | Create a listening thread for events and add update the 'FunctionMap' with
-- the corresponding 'TQueue's (i.e. communication channels).
registerStatefulFunctionality
    :: (r, st, [ExportedFunctionality r st])
    -> Neovim customcConfig () (ThreadId, [(FunctionalityDescription, FunctionType)])
registerStatefulFunctionality (r, st, fs) = do
    q <- liftIO newTQueueIO
    tid <- forkNeovim r st (listeningThread q)
    return (tid, map (\n -> (functionalityDescription n, Stateful q)) fs)
  where
    functionRoutes = foldr updateRoute Map.empty fs
    updateRoute f = case functionalityDescription f of
        Function  n _ -> Map.insert n (functionalityFunction f)
        Command   n _ -> Map.insert n (functionalityFunction f)
        AutoCmd _ _ _ -> error "Not implemented." -- FIXME

    executeFunction
        :: ([Object] -> Neovim r st Object)
        -> [Object]
        -> Neovim r st (Either String Object)
    executeFunction f args = do
        try (f args) >>= \case
            Left e -> let e' = e :: SomeException
                      in return . Left $ show e'
            Right res -> return $ Right res
    listeningThread q = do
        msg <- liftIO . atomically $ readTQueue q
        forM_ (fromMessage msg) $ \req@Request{..} ->
            forM_ (Map.lookup reqMethod functionRoutes) $ \f ->
                respond req =<< executeFunction f reqArgs
        forM_ (fromMessage msg) $ \Notification{..} ->
            forM_ (Map.lookup notMethod functionRoutes) $ \f ->
                void $ executeFunction f notArgs
        listeningThread q

