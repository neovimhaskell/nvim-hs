{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{- |
Module      :  Neovim.API.String
Description :  String based API
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.String
    where

import           Neovim.API.TH

$(generateAPI defaultAPITypeToHaskellTypeMap)

