{-# LANGUAGE TemplateHaskell #-}
module Fibonacci (plugin) where

import Neovim
import Fibonacci.Plugin (fibonacci)

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin = wrapPlugin Plugin
    { exports         = [ $(function' 'fibonacci) Sync ]
    , statefulExports = []
    }
