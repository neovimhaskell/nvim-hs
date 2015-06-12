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
module Neovim.Plugin
    where

import           Neovim.API.Context
import           Neovim.API.IPC
import           Neovim.Plugin.Classes
import           Neovim.RPC.Common
import           Neovim.RPC.FunctionCall

import           Control.Concurrent       (ThreadId, forkIO)
import           Control.Concurrent.STM
import           Control.Exception.Lifted (SomeException, try)
import           Control.Monad
import qualified Control.Monad.Reader     as R
import qualified Data.Map                 as Map
import           Data.MessagePack

-- | Register the given list of plugins. The first argument is the
-- communication channel to the internal mediators of remote procedure calls.
-- The result is a list of thread identifiers started for plugins that carry
-- state around and a 'FunctionMap'.
register :: ConfigWrapper ()
         -> [IO SomePlugin]
         -> IO (Either String ([ThreadId], FunctionMap))
register cfg = fmap (fmap fst) . runNeovim cfg () . foldM go ([], mempty)
  where
    go :: ([ThreadId], FunctionMap)
       -> IO SomePlugin
       -> Neovim' ([ThreadId], FunctionMap)
    go (pluginThreads, m) iop = do
        SomePlugin p <- liftIO iop
        (tids, m') <- registerStatefulFunctionalities (statefulExports p)
            =<< foldM updateMap m (exports p)
        return (tids ++ pluginThreads, m')

    updateMap m = \case
        Function n f  -> do
            return $ Map.insert n (Left f) m
        Command  n f  -> return $ Map.insert n (Left f) m
        AutoCmd _ _ _ -> error "TODO register autocmds"

registerStatefulFunctionalities
    :: [(r,st, [ExportedFunctionality r st])]
    -> FunctionMap
    -> Neovim' ([ThreadId], FunctionMap)
registerStatefulFunctionalities es funMap = foldM f ([], funMap) es
  where
    f (tids,m) e = (\(tid,m') -> (tid:tids,m'))
        <$> registerStatefulFunctionality m e

-- | Create a listening thread for events and add update the 'FunctionMap' with
-- the corresponding 'TQueue's (i.e. communication channels).
registerStatefulFunctionality
    :: FunctionMap
    -> (r, st, [ExportedFunctionality r st])
    -> Neovim' (ThreadId, FunctionMap)
registerStatefulFunctionality m (r, st, fs) = do
    q <- liftIO newTQueueIO
    cfg <- R.ask
    tid <- liftIO . forkIO . void
        $ runNeovim (cfg { customConfig = r }) st (listeningThread q)
    return (tid, foldr (updateMap q) m fs)
  where
    updateMap q = \case
        Function fn _ -> Map.insert fn (Right q)
        Command  fn _ -> Map.insert fn (Right q)
        AutoCmd _ _ _ -> error "Not implemented." -- FIXME

    functionRoutes = foldr updateRoute Map.empty fs
    updateRoute = \case
        Function fn f -> Map.insert fn f
        Command  fn f -> Map.insert fn f
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

