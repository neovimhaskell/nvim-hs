{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
Module      :  Neovim.API
Description :  Template Haskell generated API reexport module
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API
    where

import Neovim.API.TH

$(generateAPITypes)
