{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
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

    SomeMessage,
    fromMessage,
    ) where

import           Neovim.API.Context
import           Neovim.API.IPC           (Request (..), SomeMessage,
                                           fromMessage)
import           Neovim.RPC.Common
import           Neovim.RPC.FunctionCall

import           Control.Concurrent       (ThreadId, forkIO)
import           Control.Concurrent.STM
import           Control.Exception.Lifted (SomeException (..), try)
import           Control.Monad            (foldM, void)
import           Data.Foldable            (forM_)
import qualified Data.Map                 as Map
import           Data.MessagePack
import           Data.Text                (Text)

-- | This data type is used in the plugin registration to properly register the
-- functions.
data ExportedFunctionality r st
    = Function Text ([Object] -> Neovim r st Object)
    -- ^
    --
    -- * Name of the function (must start with an uppercase letter)
    -- * Function to call
    | Command  Text ([Object] -> Neovim r st Object)
    -- ^
    --
    -- * Name of the command (must start with an uppercase letter)
    -- * Function to call
    | AutoCmd  Text Text ([Object] -> Neovim r st Object)
    -- ^
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
    :: TQueue SomeMessage
    -> FunctionMap
    -> [ExportedFunctionality () ()]
    -> IO FunctionMap
updateFunctionMap evq = foldM updateMap
  where
    updateMap m = \case
        Function n f  -> return $ Map.insert n (Left f) m
        Command  n f  -> return $ Map.insert n (Left f) m
        AutoCmd _ _ _ -> error "TODO register autocmds"

-- | Register the given list of plugins. The first argument is the
-- communication channel to the internal mediators of remote procedure calls.
-- The result is a list of thread identifiers started for plugins that carry
-- state around and a 'FunctionMap'.
register :: TQueue SomeMessage -> [IO SomePlugin] -> IO ([ThreadId], FunctionMap)
register evq = foldM go ([], mempty)
  where
    go (pluginThreads, m) iop = do
        SomePlugin p <- iop
        (tids, m') <- registerStatefulFunctionalities evq (statefulExports p)
            =<< updateFunctionMap evq m (exports p)
        return (tids ++ pluginThreads, m')

registerStatefulFunctionalities
    :: TQueue SomeMessage
    -> [(r,st, [ExportedFunctionality r st])]
    -> FunctionMap
    -> IO ([ThreadId], FunctionMap)
registerStatefulFunctionalities evq es funMap = foldM f ([], funMap) es
  where
    f (tids,m) e = (\(tid,m') -> (tid:tids,m'))
        <$> registerStatefulFunctionality evq m e

-- | Create a listening thread for events and add update the 'FunctionMap' with
-- the corresponding 'TQueue's (i.e. communication channels).
registerStatefulFunctionality
    :: TQueue SomeMessage -- ^ Global event queue
    -> FunctionMap        -- ^ Function map to update
    -> (r, st, [ExportedFunctionality r st])
    -> IO (ThreadId, FunctionMap)
registerStatefulFunctionality evq m (r, st, fs) = do
    q <- newTQueueIO
    tid <- forkIO . void $ runNeovim (ConfigWrapper evq r) st (listeningThread q)
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

    listeningThread q = do
        msg <- liftIO . atomically $ readTQueue q
        let funAndRequest = do req <- fromMessage msg
                               f <- Map.lookup (reqMethod req) functionRoutes
                               Just (f,req)
        forM_ funAndRequest $ \(f,req) -> do
            retVal <- (try . f . reqArgs) req >>= \case
                Left e -> let e' = e :: SomeException
                          in return . Left $ show e'
                Right res -> return $ Right res
            respond req retVal
        listeningThread q

