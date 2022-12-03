{-# LANGUAGE TemplateHaskell #-}

module Fibonacci (plugin) where

import Fibonacci.Plugin (fibonacci)
import Neovim

plugin :: Neovim (StartupConfig NeovimConfig) () NeovimPlugin
plugin =
    wrapPlugin
        Plugin
            { exports = [$(function' 'fibonacci) Sync]
            , statefulExports = []
            }
