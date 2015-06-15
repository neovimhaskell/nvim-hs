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
import           Control.Monad            (foldM, void)
import qualified Control.Monad.Reader     as R
import           Data.Foldable            (forM_)
import qualified Data.Map                 as Map
import           Data.MessagePack
import           Data.Text                (unpack)
import           System.Log.Logger

logger :: String
logger = "Neovim.Plugin"

-- | Register the given list of plugins. The first argument is the
-- communication channel to the internal mediators of remote procedure calls.
-- The result is a list of thread identifiers started for plugins that carry
-- state around and a 'FunctionMap'.
register :: ConfigWrapper (TVar FunctionMap)
         -> [IO NeovimPlugin]
         -> IO (Either String [ThreadId])
register cfg = fmap (fmap fst) . runNeovim cfg () . foldM go []
  where
    go :: [ThreadId]
       -> IO NeovimPlugin
       -> Neovim (TVar FunctionMap) () [ThreadId]
    go pluginThreads iop = do
        NeovimPlugin p <- liftIO iop
        mapM_ (\e -> updateMap e (Map.insert (name e) (Stateless e))) (exports p)
        tids <- mapM registerStatefulFunctionality (statefulExports p)
        return $ tids ++ pluginThreads


updateMap :: ExportedFunctionality r st
          -> (FunctionMap -> FunctionMap)
          -> Neovim (TVar FunctionMap) () ThreadId
updateMap ef upd = do
    m <- ask
    waiting <- liftIO newEmptyTMVarIO
    modifyMap $ Map.insert (name ef) (Loading waiting)
    forkNeovim m () $ do
        case ef of
            Function{} -> do
                pName <- R.asks _providerName
                ret <- wait . vim_command $ concat
                    [ "call remote#define#FunctionOnHost('" , pName ,"', '"
                    , unpack (name ef), "', 1,'", unpack (name ef), "',{})"
                    ]
                case ret of
                    Left e -> do
                        liftIO . errorM logger $
                            "Failed to register function: " ++ unpack (name ef) ++ show e
                    Right _ -> do
                        liftIO $ debugM logger $ "Registerd function: " ++ unpack (name ef)
                        modifyMap $ upd
            Command{} -> error "TODO register commands"
            AutoCmd{} -> error "TODO register autocmds"
        atomically' $ putTMVar waiting ()

  where
    modifyMap f = ask >>= \m -> atomically' $ modifyTVar m f

-- | Create a listening thread for events and add update the 'FunctionMap' with
-- the corresponding 'TQueue's (i.e. communication channels).
registerStatefulFunctionality
    :: (r, st, [ExportedFunctionality r st])
    -> Neovim (TVar FunctionMap) () ThreadId
registerStatefulFunctionality (r, st, fs) = do
    q <- liftIO newTQueueIO
    tid <- forkNeovim r st (listeningThread q)
    forM_ fs $ \f -> updateMap f (Map.insert (name f) (Stateful q))
    return tid
  where
    functionRoutes = foldr updateRoute Map.empty fs
    updateRoute = \case
        Function    n f -> Map.insert n f
        Command     n f -> Map.insert n f
        AutoCmd _ _ _ _ -> error "Not implemented." -- FIXME

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

