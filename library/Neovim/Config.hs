{- |
Module      :  Neovim.Config
Description :  The user editable and compilable configuration
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.Config (
    NeovimConfig(..),
    module Data.Default,
    module System.Log,
    ) where

import           Neovim.Plugin.Internal (NeovimPlugin)

import qualified Config.Dyre            as Dyre
import           Data.Default           (Default (def))
import           System.Log             (Priority (..))

data NeovimConfig = Config
    { plugins      :: [IO NeovimPlugin]
    -- ^ The list of plugins. The IO type inside the list allows the plugin
    -- author to run some arbitrary startup code before creating a value of
    -- type 'NeovimPlugin'.
    , errorMessage :: Maybe String
    -- ^ Used by 'Dyre' for storing compilation errors.
    , logOptions   :: Maybe (FilePath, Priority)
    -- ^ Set the general logging options.
    , dyreParams   :: Maybe (Dyre.Params NeovimConfig)
    -- ^ Parmaeters used by 'Dyre'. This is only used for the
    -- "Neovim.Plugin.ConfigHelper" plugin.
    }

instance Default NeovimConfig where
    def = Config
            { plugins      = []
            , errorMessage = Nothing
            , logOptions   = Nothing
            , dyreParams   = Nothing
            }

