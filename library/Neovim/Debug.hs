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
module Neovim.Debug (
    debug,
    debug',
    develMain,
    quitDevelMain,
    restartDevelMain,

    printGlobalFunctionMap,

    runNeovim,
    runNeovim',
    module Neovim,
    ) where

import           Neovim
import           Neovim.Context               (runNeovim)
import qualified Neovim.Context.Internal      as Internal
import           Neovim.Log                   (disableLogger)
import           Neovim.Main                  (CommandLineOptions (..),
                                               runPluginProvider)
import           Neovim.RPC.Common            (RPCConfig)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.Map                     as Map
import           Foreign.Store
import           System.IO                    (stdout)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

import           Prelude


-- | Run a 'Neovim' function.
--
-- This function connects to the socket pointed to by the environment variable
-- @$NVIM_LISTEN_ADDRESS@ and executes the command. It does not register itself
-- as a real plugin provider, you can simply call neovim-functions from the
-- module "Neovim.API.String" this way.
--
-- Tip: If you run a terminal inside a neovim instance, then this variable is
-- automatically set.
debug :: r -> st -> Internal.Neovim r st a -> IO (Either Doc (a, st))
debug r st a = disableLogger $ do
    runPluginProvider def { env = True } Nothing transitionHandler Nothing
  where
    transitionHandler tids cfg = takeMVar (Internal.transitionTo cfg) >>= \case
        Internal.Failure e ->
            return $ Left e

        Internal.InitSuccess -> do
            res <- Internal.runNeovim
                (cfg { Internal.customConfig = r, Internal.pluginSettings = Nothing })
                st
                a

            mapM_ killThread tids
            return res

        _ ->
            return . Left $ text "Unexpected transition state."


-- | Run a 'Neovim'' function.
--
-- @
-- debug' a = fmap fst <$> debug () () a
-- @
--
-- See documentation for 'debug'.
debug' :: Internal.Neovim' a -> IO (Either Doc a)
debug' a = fmap fst <$> debug () () a


-- | This function is intended to be run _once_ in a ghci session that to
-- give a REPL based workflow when developing a plugin.
--
-- Note that the dyre-based reload mechanisms, i.e. the
-- "Neovim.Plugin.ConfigHelper" plugin, is not started this way.
--
-- To use this in ghci, you simply bind the results to some variables. After
-- each reload of ghci, you have to rebind those variables.
--
-- Example:
--
-- @
-- λ 'Right' (tids, cfg) <- 'develMain' 'Nothing'
--
-- λ 'runNeovim'' cfg \$ vim_call_function \"getqflist\" []
-- 'Right' ('Right' ('ObjectArray' []))
--
-- λ :r
--
-- λ 'Right' (tids, cfg) <- 'develMain' 'Nothing'
-- @
--
develMain
    :: Maybe NeovimConfig
    -> IO (Either Doc ([ThreadId], Internal.Config RPCConfig ()))
develMain mcfg = lookupStore 0 >>= \case
    Nothing -> do
        x <- disableLogger $
                runPluginProvider def { env = True } mcfg transitionHandler Nothing
        void $ newStore x
        return x

    Just x ->
        readStore x
  where
    transitionHandler tids cfg = takeMVar (Internal.transitionTo cfg) >>= \case
        Internal.Failure e ->
            return $ Left e

        Internal.InitSuccess -> do
            transitionHandlerThread <- forkIO $ do
                myTid <- myThreadId
                void $ transitionHandler (myTid:tids) cfg
            return $ Right (transitionHandlerThread:tids, cfg)

        Internal.Quit -> do
            lookupStore 0 >>= \case
                Nothing ->
                    return ()

                Just x ->
                    deleteStore x

            mapM_ killThread tids
            return . Left $ text "Quit develMain"

        _ ->
            return . Left $ text "Unexpected transition state for develMain."


-- | Quit a previously started plugin provider.
quitDevelMain :: Internal.Config r st -> IO ()
quitDevelMain cfg = putMVar (Internal.transitionTo cfg) Internal.Quit


-- | Restart the development plugin provider.
restartDevelMain
    :: Internal.Config RPCConfig ()
    -> Maybe NeovimConfig
    -> IO (Either Doc ([ThreadId], Internal.Config RPCConfig ()))
restartDevelMain cfg mcfg = do
    quitDevelMain cfg
    develMain mcfg


-- | Convenience function to run a stateless 'Neovim' function.
runNeovim' :: Internal.Config r st -> Neovim' a -> IO (Either Doc a)
runNeovim' cfg =
    fmap (fmap fst) . runNeovim (Internal.retypeConfig () () cfg) ()


-- | Print the global function map to the console.
printGlobalFunctionMap :: Internal.Config r st -> IO ()
printGlobalFunctionMap cfg = do
    es <- fmap Map.toList . atomically $ readTMVar (Internal.globalFunctionMap cfg)
    let header = text "Printing global function map:"
        funs   = map (\(fname, (d, f)) ->
                    nest 3 (pretty fname
                    </> text "->"
                    </> pretty d <+> text ":"
                    <+> pretty f)) es
    displayIO stdout . renderPretty 0.4 80 $
        nest 2 (header <$$> vcat funs)
            <$$> Pretty.empty


