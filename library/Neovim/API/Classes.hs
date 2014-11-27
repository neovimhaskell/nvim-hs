{- |
Module      :  Neovim.API.Classes
Description :  Type classes used for conversion of msgpack and Haskell types
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.Classes
    ( CustomType(..)
    , Callable(..)
    , module Data.Int
    ) where

import Data.Int (Int64)
import Data.ByteString
import Data.MessagePack

class CustomType a where
    -- | Custom types (and error types) contain a unique identifier that is
    -- used for serialization. This function extracts this idenfifier.
    getID :: a -> Int64
    -- | Custom types can contain any type of payload.
    getConstructor :: Int64 -> Object -> a
    -- | Return the custom payload of the given value.
    payload :: a -> Object

-- | All generated functions are instance of this class.
class Callable a where
    -- | Extract the name and arguments from the data type.
    args :: a -> (ByteString, [Object])


