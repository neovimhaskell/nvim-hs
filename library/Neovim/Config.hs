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
    ) where

import Neovim.API.Context (Neovim)

import Data.Default

data NeovimConfig = Config
    { plugins :: [Neovim ()]
    , errorMessage :: Maybe String
    -- ^ Used by "Dyre" for storing compilation errors.
    }

instance Default NeovimConfig where
    def = Config
            { plugins = []
            , errorMessage = Nothing
            }

