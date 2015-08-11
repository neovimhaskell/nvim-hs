{-# LANGUAGE LambdaCase #-}
{- |
Module      :  Neovim.Debug
Description :  Utilities to debug Neovim and nvim-hs functionality
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Debug
    where

import qualified Neovim.Context.Internal as Internal
import           Neovim.Log              (disableLogger)
import           Neovim.Plugin           (startPluginThreads)
import           Neovim.RPC.Common       (SocketType (Environment),
                                          createHandle, newRPCConfig)
import           Neovim.RPC.EventHandler
import           Neovim.RPC.SocketReader

import           Control.Concurrent
import           Control.Concurrent.STM  (atomically, putTMVar)
import           Control.Monad
import           Options.Applicative
import           System.IO               (IOMode (ReadWriteMode))

import           Prelude

debug :: r -> st -> Internal.Neovim r st a -> IO (Either String (a, st))
debug r st a = disableLogger $ do
    h <- createHandle ReadWriteMode Environment
    rpcConfig <- newRPCConfig
    conf <- Internal.newConfig (pure "debug-nvim-hs") (pure ())

    let startupConf = conf { Internal.customConfig = ()
                           , Internal.pluginSettings = Nothing
                           }
        rpcEnv = conf { Internal.customConfig = rpcConfig
                      , Internal.pluginSettings = Nothing
                      }
    ehTid <- forkIO $ runEventHandler h rpcEnv
    srTid <- forkIO $ runSocketReader h rpcEnv
    startPluginThreads startupConf [] >>= \case
        Left e ->
            return (Left e)

        Right (funMapEntries, pluginTids) -> do
            atomically $ putTMVar
                            (Internal.globalFunctionMap conf)
                            (Internal.mkFunctionMap funMapEntries)
            res <- Internal.runNeovim
                (conf { Internal.customConfig = r, Internal.pluginSettings = Nothing })
                st
                a

            mapM_ killThread (srTid:ehTid:pluginTids)
            return res


debug' :: Internal.Neovim' a -> IO (Either String a)
debug' a = fmap fst <$> debug () () a
