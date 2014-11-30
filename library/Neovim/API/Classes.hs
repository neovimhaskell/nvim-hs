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
    , module Data.Int
    ) where

import           Control.Arrow
import           Data.ByteString      (ByteString)
import           Data.Int             (Int64,Int8)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.MessagePack
import           Data.Text            (Text)

-- FIXME saep 2014-11-28 Is assuming UTF-8 reasonable?
import qualified Data.ByteString.UTF8 as U (fromString, toString)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)


-- | Conversion from 'Object' files to Haskell types and back with respect
-- to neovim's interpretation.
class NvimInstance o where
    toObject :: o -> Object
    fromObject :: Object -> o

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

instance (Ord key, NvimInstance key, NvimInstance val)
        => NvimInstance (Map key val) where
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

instance NvimInstance Object where
    toObject = id
    fromObject = id

-- By the magic of vim, i will create these.
instance NvimInstance o => NvimInstance (o, o) where
    toObject ~(o1, o2) = ObjectArray $ map toObject [o1, o2]
    fromObject ~(ObjectArray [o1, o2]) = (fromObject o1, fromObject o2)

instance NvimInstance o => NvimInstance (o, o, o) where
    toObject ~(o1, o2, o3) = ObjectArray $ map toObject [o1, o2, o3]
    fromObject ~(ObjectArray [o1, o2, o3]) = (fromObject o1, fromObject o2, fromObject o3)

instance NvimInstance o => NvimInstance (o, o, o, o) where
    toObject ~(o1, o2, o3, o4) = ObjectArray $ map toObject [o1, o2, o3, o4]
    fromObject ~(ObjectArray [o1, o2, o3, o4]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4)

instance NvimInstance o => NvimInstance (o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5) = ObjectArray $ map toObject [o1, o2, o3, o4, o5]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7, o8) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7, fromObject o8)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7, o8, o9) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8, o9]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7, fromObject o8, fromObject o9)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7, fromObject o8, fromObject o9, fromObject o10)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7, fromObject o8, fromObject o9, fromObject o10, fromObject o11)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7, fromObject o8, fromObject o9, fromObject o10, fromObject o11, fromObject o12)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7, fromObject o8, fromObject o9, fromObject o10, fromObject o11, fromObject o12, fromObject o13)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o, o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7, fromObject o8, fromObject o9, fromObject o10, fromObject o11, fromObject o12, fromObject o13, fromObject o14)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o, o, o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7, fromObject o8, fromObject o9, fromObject o10, fromObject o11, fromObject o12, fromObject o13, fromObject o14, fromObject o15)

instance NvimInstance o => NvimInstance (o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o) where
    toObject ~(o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16]
    fromObject ~(ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15, o16]) = (fromObject o1, fromObject o2, fromObject o3, fromObject o4, fromObject o5, fromObject o6, fromObject o7, fromObject o8, fromObject o9, fromObject o10, fromObject o11, fromObject o12, fromObject o13, fromObject o14, fromObject o15, fromObject o16)

