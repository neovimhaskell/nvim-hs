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
    ) where

import qualified Neovim.Context.Internal as Internal
import           Neovim.Log              (disableLogger)
import           Neovim.Main             (CommandLineOptions (..),
                                          runPluginProvider)

import           Control.Concurrent
import           Control.Monad
import           Data.Default
import           Options.Applicative

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
    runPluginProvider def { env = True } Nothing finalizer
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

