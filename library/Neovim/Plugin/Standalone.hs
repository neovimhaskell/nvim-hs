{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Neovim.Plugin.Standalone
Description :  Plugin to manage standalone plugins which can be managed as
               ordinary neovim plugins.
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.Standalone
  ( plugin
  , StandaloneImplementation(..)
  , defaultImplementations
  ) where


import           Neovim.API.TH
import           Neovim.Context
import           Neovim.Plugin
import qualified Neovim.Plugin.Standalone.Cabal    as Cabal
import           Neovim.Plugin.Standalone.Internal
import qualified Neovim.Plugin.Standalone.Stack    as Stack
import           UnliftIO.STM                      (newTVarIO)


-- | Uses stack if stack.yaml is present and falls back to cabal with the
-- v2-family of commands.
defaultImplementations :: [StandaloneImplementation]
defaultImplementations =
  [ Stack.implementation
  , Cabal.v2implementation
  ]


-- | You can configure this plugin by specifying the list of
-- 'StandaloneImplementation's which are tried in order. If you specify
-- 'Nothing' the list of 'defaultImplementations' is used.
plugin :: Maybe [StandaloneImplementation] -> Neovim StartupConfig NeovimPlugin
plugin userConfiguredImplementations = do
  startedPlugins <- newTVarIO mempty
  pathLookupCache <- newTVarIO mempty
  let preferredImplementations =
          maybe defaultImplementations id userConfiguredImplementations
  wrapPlugin Plugin
    { environment = StandaloneEnv{..}
    , exports =
      [ $(function' 'nvimhsStartMeIfNecessary) Async
      ]
    }
