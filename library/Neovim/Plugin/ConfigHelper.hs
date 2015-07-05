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

import Neovim.Plugin.Classes
import Config.Dyre (Params)
import Neovim.Config
import Neovim.API.TH
import Neovim.Plugin.ConfigHelper.Internal

-- | Note that you cannot really use this plugin by hand. It is automatically
-- loaded for all Neovim instances.
plugin :: Params NeovimConfig -> IO NeovimPlugin
plugin params = wrapPlugin Plugin
    { exports =
        [ $(function' 'pingNvimhs) Sync
        , $(command' 'restartNvimhs) def { sync = Async }
        ]
    , statefulExports =
        [ (params, Nothing, [$(command' 'recompileNvimhs) def { sync = Async }])
        ]
    }
