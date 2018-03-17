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
    module System.Log,
    ) where

import           Neovim.Context         (Neovim)
import           Neovim.Plugin.Internal (NeovimPlugin)
import           Neovim.Plugin.Startup  (StartupConfig)

import           System.Log             (Priority (..))

-- | This data type contains information about the configuration of neovim. See
-- the fields' documentation for what you possibly want to change. Also, the
-- tutorial in the "Neovim" module should get you started.
data NeovimConfig = Config
    { plugins      :: [Neovim (StartupConfig NeovimConfig) NeovimPlugin]
    -- ^ The list of plugins. The IO type inside the list allows the plugin
    -- author to run some arbitrary startup code before creating a value of
    -- type 'NeovimPlugin'.

    , logOptions   :: Maybe (FilePath, Priority)
    -- ^ Set the general logging options.

    , errorMessage :: Maybe String
    -- ^ Internally used field. Changing this has no effect.
    --
    -- Used by 'Dyre' for storing compilation errors.

    }

