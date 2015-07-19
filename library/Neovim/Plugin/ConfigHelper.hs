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
import Data.Text (pack)

-- | Note that you cannot really use this plugin by hand. It is automatically
-- loaded for all Neovim instances.
plugin :: Params NeovimConfig -> IO NeovimPlugin
plugin params = do
    (_, _, cfgFile, _, libsDir) <- getPaths params
    wrapPlugin Plugin
        { exports =
            [ $(function' 'pingNvimhs) Sync
            , $(command' 'restartNvimhs) def { cmdSync = Async }
            ]
        , statefulExports =
            [ (params, [],
                [ $(command' 'recompileNvimhs) def { cmdSync = Async }
                , $(autocmd 'recompileNvimhs) "BufWritePost" def
                        { acmdSync    = Async
                        , acmdPattern = pack cfgFile
                        }
                , $(autocmd 'recompileNvimhs) "BufWritePost" def
                        { acmdSync    = Async
                        , acmdPattern = pack (libsDir++"/.*")
                        }
                ])
            ]
        }
