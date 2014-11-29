{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{- |
Module      :  Neovim.API.Classes
Description :  Type classes used for conversion of msgpack and Haskell types
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.Classes
    ( NvimInstance(..)
    , Callable(..)
    , module Data.Int
    ) where

import           Control.Arrow
import           Data.ByteString      (ByteString)
import           Data.Int             (Int64,Int8)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.MessagePack
import           Data.Serialize
import           Data.Text            (Text)

-- FIXME saep 2014-11-28 Is assuming UTF-8 reasonable?
import qualified Data.ByteString.UTF8 as U (fromString, toString)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)


-- | All generated functions are instance of this class.
class Callable a where
    -- | Extract the name and arguments from the data type.
    args :: a -> (ByteString, [Object])


-- | Conversion from 'Object' files to Haskell types and back with respect
-- to neovim's interpretation.
class NvimInstance o where
    toObject :: o -> Object
    fromObject :: Object -> o

{-instance NvimInstance o => Serialize o where-}
    {-put = put . toObject-}
    {-get = fmap toObject get-}

instance NvimInstance () where
    toObject _ = ObjectNil
    fromObject ~ObjectNil = ()

instance NvimInstance Bool where
    toObject = ObjectBool
    fromObject ~(ObjectBool o) = o

instance NvimInstance Double where
    toObject = ObjectDouble
    fromObject ~(ObjectDouble o) = o

instance NvimInstance Int64 where
    toObject = ObjectInt
    fromObject ~(ObjectInt o) = o

instance NvimInstance [Char] where
    toObject = ObjectBinary . U.fromString
    fromObject ~(ObjectBinary o) = U.toString o

instance NvimInstance o => NvimInstance [o] where
    toObject = ObjectArray . map toObject
    fromObject ~(ObjectArray os) = map fromObject os

instance (Ord key, NvimInstance key, NvimInstance val) => NvimInstance (Map key val) where
    toObject = ObjectMap
        . Map.fromList . map (toObject *** toObject) . Map.toList
    fromObject ~(ObjectMap om) = Map.fromList
        . map (fromObject *** fromObject) $ Map.toList om

instance NvimInstance Text where
    toObject = ObjectBinary . encodeUtf8
    fromObject ~(ObjectBinary o) = decodeUtf8 o

instance NvimInstance ByteString where
    toObject = ObjectBinary
    fromObject ~(ObjectBinary o) = o

