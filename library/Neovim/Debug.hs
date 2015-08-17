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

    runNeovim,
    runNeovim',
    module Neovim,
    ) where

import           Neovim
import           Neovim.Context          (runNeovim)
import qualified Neovim.Context.Internal as Internal
import           Neovim.Log              (disableLogger)
import           Neovim.Main             (CommandLineOptions (..),
                                          runPluginProvider)
import           Neovim.RPC.Common       (RPCConfig)

import           Control.Concurrent
import           Control.Monad
import           Foreign.Store

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
debug :: r -> st -> Internal.Neovim r st a -> IO (Either String (a, st))
debug r st a = disableLogger $ do
    runPluginProvider def { env = True } Nothing finalizer Nothing
  where
    finalizer tids cfg = takeMVar (Internal.quit cfg) >>= \case
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
            return $ Left "Unexpected finalizer state."


-- | Run a 'Neovim'' function.
--
-- @
-- debug' a = fmap fst <$> debug () () a
-- @
--
-- See documentation for 'debug'.
debug' :: Internal.Neovim' a -> IO (Either String a)
debug' a = fmap fst <$> debug () () a


-- | This function is intended to be run _once_ in a ghci session that to
-- give a REPL based workflow when developing a plugin.
--
-- To use this in ghci, you simply bind the results to some variables. After
-- each reload of ghci, you have to rebind those variables.
--
-- Example:
--
-- @
-- λ Right (tids, cfg) <- develMain 'Nothing'
--
-- λ runNeovim' cfg \$ vim_call_function "getqflist" []
-- Right (Right (ObjectArray []))
--
-- λ :r
--
-- λ Right (tids, cfg) <- develMain
-- @
--
develMain :: Maybe NeovimConfig -> IO (Either String ([ThreadId], Internal.Config RPCConfig ()))
develMain mcfg = lookupStore 0 >>= \case
    Nothing -> do
        x <- disableLogger $
                runPluginProvider def { env = True } mcfg finalizer Nothing
        void $ newStore x
        return x

    Just x ->
        readStore x
  where
    finalizer tids cfg = takeMVar (Internal.quit cfg) >>= \case
        Internal.Failure e ->
            return $ Left e

        Internal.InitSuccess ->
            return $ Right (tids, cfg)

        _ ->
            return $ Left "Unexpected finalizer state for develMain."


-- | Convenience function to run a stateless 'Neovim' function.
runNeovim' :: Internal.Config r st -> Neovim' a -> IO (Either String a)
runNeovim' cfg =
    fmap (fmap fst) . runNeovim (Internal.retypeConfig () () cfg) ()

