{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

import           Config.Dyre                         (Params)
import           Config.Dyre.Paths                   (getPaths)
import           Neovim.API.TH
import           Neovim.Config
import           Neovim.Plugin.Classes
import           Neovim.Plugin.ConfigHelper.Internal

-- | Note that you cannot really use this plugin by hand. It is automatically
-- loaded for all Neovim instances.
--
-- The first argument is a set of environment variables that is needed to
-- recompile /nvim-hs/. They are temporarily set during the recompilation
-- function call.
plugin :: [(String, Maybe String)] -> Params NeovimConfig -> IO NeovimPlugin
plugin ghcEnv params = do
    (_, _, cfgFile, _, libsDir) <- getPaths params
    wrapPlugin Plugin
        { exports =
            [ $(function' 'pingNvimhs) Sync
            ]
        , statefulExports =
            [ ((params, ghcEnv), [],
                [ $(autocmd 'recompileNvimhs) "BufWritePost" def
                        { acmdSync    = Async
                        , acmdPattern = cfgFile
                        }
                , $(autocmd 'recompileNvimhs) "BufWritePost" def
                        { acmdSync    = Async
                        , acmdPattern = libsDir++"/*"
                        }
                , $(command' 'restartNvimhs) [CmdSync Async, CmdBang, CmdRegister]
                ])
            ]
        }
