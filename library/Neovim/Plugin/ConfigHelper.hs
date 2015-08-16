{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{- |
Module      :  Neovim.Plugin.ConfigHelper
Description :  Helper plugin to ease recompiling the nvim-hs config
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.ConfigHelper
    where

import           Neovim.API.TH
import           Neovim.Config
import           Neovim.Context
import           Neovim.Plugin.Classes
import           Neovim.Plugin.ConfigHelper.Internal
import           Neovim.Plugin.Internal
import           Neovim.Plugin.Startup

import           Config.Dyre.Paths                   (getPaths)
import           Data.Default


plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = asks dyreParams >>= \case
    Nothing ->
        wrapPlugin Plugin { exports = [], statefulExports = [] }

    Just params -> do
        ghcEnv <- asks ghcEnvironmentVariables
        (_, _, cfgFile, _, libsDir) <- liftIO $ getPaths params
        wrapPlugin Plugin
            { exports =
                [ $(function' 'pingNvimhs) Sync
                ]
            , statefulExports =
                [ ((params, ghcEnv), [],
                    [ $(autocmd 'recompileNvimhs) "BufWritePost" def
                            { acmdPattern = cfgFile
                            }
                    , $(autocmd 'recompileNvimhs) "BufWritePost" def
                            { acmdPattern = libsDir++"/*"
                            }
                    , $(command' 'restartNvimhs) [CmdSync Async, CmdBang, CmdRegister]
                    ])
                ]
            }
