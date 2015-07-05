{- |
Module      :  Main
Description :  Main module for the haskell plugin provider
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Main where

import           Data.Default
import           Neovim       (neovim)

main :: IO ()
main = neovim def
