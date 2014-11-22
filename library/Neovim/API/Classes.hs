{- |
Module      :  Neovim.API.Classes
Description :  Type classes used for conversion of msgpack and Haskell types
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.Classes
    ( ID(..)
    , module Data.Int
    ) where

import Data.Int (Int64)

-- | This is pretty much just 'Enum'. This class exists because  'Enum'
-- does not guarantee 64 bit compatibility.
class ID a where
    toID :: a -> Int64
    fromID :: Int64 -> a

