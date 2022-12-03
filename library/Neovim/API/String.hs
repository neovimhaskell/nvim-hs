{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoOverloadedStrings #-}

{- |
Module      :  Neovim.API.String
Description :  String based API
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

Note that this module is completely generated. If you're reading this on
hackage, the actual functions of this module may be different from what is
available to you. All the functions in this module depend on the neovim version
that was used when this package was compiled.
-}
module Neovim.API.String where

import Neovim.API.TH

$(generateAPI stringListTypeMap)
