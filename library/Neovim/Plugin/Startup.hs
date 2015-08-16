{- |
Module      :  Neovim.Plugin.Startup
Description :  Startup utilities for plugins
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

This plugin only exists due to cyclic dependencies.
-}
module Neovim.Plugin.Startup
    where

import qualified Config.Dyre                  as Dyre

-- | This data type contains internal fields of /nvim-hs/ that may
-- be useful for plugin authors. It is available via 'ask' inside
-- the plugin startup code.
data StartupConfig cfg = StartupConfig
    { dyreParams :: Maybe (Dyre.Params cfg)
    -- ^ The configuration options for "Config.Dyre". This is always set if
    -- /nvim-hs/ has been started via "Config.Dyre". Be sure to set up the
    -- 'ghcEnvironmentVariables' correctly if you issue a recompilation via
    -- the "Config.Dyre" API.

    , ghcEnvironmentVariables :: [(String, Maybe String)]
    -- ^ The GHC environment variables with which /nvim-hs/ has been started.
    -- This are mainly of significance if you want to use the same environment
    -- for compilation or a REPL that /nvim-hs/ runs on.
    --
    -- These variables have to be used if you want to invoke functionality of
    -- "Config.Dyre" targeting /nvim-hs/.
    }

