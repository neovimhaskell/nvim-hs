{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{- |
Module      :  Neovim.API.Plugin
Description :  Plugin author documentation
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

This module describes how a Haksell plugin can be plugged into Neovim.
-}
module Neovim.API.Plugin (
    ExportedFunctionality(..),
    register,
    Plugin(..),
    SomePlugin(..),
    Request(..),
    Notification(..),

    SomeMessage,
    fromMessage,
    ) where

import           Neovim.API.Context
import           Neovim.API.IPC           (Notification (..), Request (..),
                                           SomeMessage, fromMessage)
import           Neovim.RPC.Common
import           Neovim.RPC.FunctionCall

import           Control.Concurrent       (ThreadId, forkIO)
import           Control.Concurrent.STM
import           Control.Exception.Lifted (SomeException (..), try)
import           Control.Monad            (foldM, void)
import qualified Control.Monad.Reader     as R
import           Data.Foldable            (forM_)
import qualified Data.Map                 as Map
import           Data.MessagePack
import           Data.Monoid
import           Data.Text                (Text)

import           Neovim.Debug

-- | This data type is used in the plugin registration to properly register the
-- functions.
data ExportedFunctionality r st
    = Function Text ([Object] -> Neovim r st Object)
    -- ^ Exported function. Callable via @call name(arg1,arg2)@.
    --
    -- * Name of the function (must start with an uppercase letter)
    -- * Function to call
    | Command  Text ([Object] -> Neovim r st Object)
    -- ^ Exported Command. Callable via @:Name arg1 arg2@.
    --
    -- * Name of the command (must start with an uppercase letter)
    -- * Function to call
    | AutoCmd  Text Text ([Object] -> Neovim r st Object)
    -- ^ Exported autocommand. Will call the given function if the type and
    -- filter match.
    --
    -- NB: Since we are registering this on the Haskell side of things, the
    -- number of accepted arguments should be 0.
    -- TODO Should this be enforced somehow? Possibly via the TH generator.
    --
    -- * Type of autocmd (e.g. FileType)
    -- * Filter fo the autocmd type
    -- * Function to call

-- | This data type contains meta information for the plugin manager.
--
data Plugin r st = Plugin
    { exports         :: [ExportedFunctionality () ()]
    , statefulExports :: [(r, st, [ExportedFunctionality r  st])]
    }

data SomePlugin = forall r st. SomePlugin (Plugin r st)

updateFunctionMap
    :: FunctionMap
    -> [ExportedFunctionality () ()]
    -> Neovim' FunctionMap
updateFunctionMap = foldM updateMap
  where
    updateMap m = \case
        Function n f  -> return $ Map.insert n (Left f) m
        Command  n f  -> return $ Map.insert n (Left f) m
        AutoCmd _ _ _ -> error "TODO register autocmds"

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
            =<< updateFunctionMap m (exports p)
        return (tids ++ pluginThreads, m')

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
    return $ (tid, foldr (updateMap q) m fs)
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
        liftIO . debugM "Plugin.hs" $
            "Executing function with arguments: " <> show args
        try (f args) >>= \case
            Left e -> let e' = e :: SomeException
                      in return . Left $ show e'
            Right res -> return $ Right res
    listeningThread q = do
        msg <- liftIO . atomically $ readTQueue q
        liftIO . debugM "Plugin.hs" $ "Running listeningThread"
        forM_ (fromMessage msg) $ \req@Request{..} ->
            forM_ (Map.lookup reqMethod functionRoutes) $ \f ->
                respond req =<< executeFunction f reqArgs
        forM_ (fromMessage msg) $ \Notification{..} ->
            forM_ (Map.lookup notMethod functionRoutes) $ \f ->
                void $ executeFunction f notArgs
        listeningThread q

