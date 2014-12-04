{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Module      :  Neovim.API.Classes
Description :  Type classes used for conversion of msgpack and Haskell types
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.Classes
    ( NvimObject(..)
    , module Data.Int
    ) where

import           Control.Applicative
import           Control.Arrow
import           Data.ByteString      (ByteString)
import           Data.Int             (Int64, Int8)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.MessagePack
import           Data.Monoid
import           Data.Text            as Text (Text, unpack)
import           Data.Traversable

-- FIXME saep 2014-11-28 Is assuming UTF-8 reasonable?
import qualified Data.ByteString.UTF8 as U (fromString, toString)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)


-- | Conversion from 'Object' files to Haskell types and back with respect
-- to neovim's interpretation.
class NvimObject o where
    toObject :: o -> Object
    fromObjectUnsafe :: Object -> o
    fromObjectUnsafe o = either
        (error ("Not the expected object" <> show o))
        id
        (fromObject o)
    fromObject :: Object -> Either String o
    fromObject = return . fromObjectUnsafe

instance NvimObject () where
    toObject _           = ObjectNil
    fromObject ObjectNil = return ()
    fromObject o         = Left $ "Expected ObjectNil, but got " <> show o

instance NvimObject Bool where
    toObject                  = ObjectBool
    fromObject (ObjectBool o) = return o
    fromObject o              = Left $ "Expected ObjectBool, but got " <> show o

instance NvimObject Double where
    toObject                    = ObjectDouble
    fromObject (ObjectDouble o) = return o
    fromObject o                = Left $ "Expected ObjectDouble, but got " <> show o

instance NvimObject Int64 where
    toObject                 = ObjectInt
    fromObject (ObjectInt o) = return o
    fromObject o             = Left $ "Expected ObjectInt, but got " <> show o

instance NvimObject [Char] where
    toObject                    = ObjectBinary . U.fromString
    fromObject (ObjectBinary o) = return $ U.toString o
    fromObject (ObjectString o) = return $ Text.unpack o
    fromObject o                = Left $ "Expected ObjectBinary, but got " <> show o

instance NvimObject o => NvimObject [o] where
    toObject                    = ObjectArray . map toObject
    fromObject (ObjectArray os) = return $ map fromObjectUnsafe os
    fromObject o                = Left $ "Expected ObjectArray, but got " <> show o

instance (Ord key, NvimObject key, NvimObject val)
        => NvimObject (Map key val) where
    toObject = ObjectMap
        . Map.fromList . map (toObject *** toObject) . Map.toList
    fromObject (ObjectMap om) = Map.fromList <$>
        (sequenceA
            . map (uncurry (liftA2 (,))
                    . (fromObject *** fromObject))
                . Map.toList) om

    fromObject o = Left $ "Expected ObjectMap, but got " <> show o

instance NvimObject Text where
    toObject                    = ObjectBinary . encodeUtf8
    fromObject (ObjectBinary o) = return $ decodeUtf8 o
    fromObject (ObjectString o) = return o
    fromObject o                = Left $ "Expected ObjectBinary, but got " <> show o

instance NvimObject ByteString where
    toObject                    = ObjectBinary
    fromObject (ObjectBinary o) = return o
    fromObject o                = Left $ "Expected ObjectBinary, but got " <> show o

instance NvimObject Object where
    toObject = id
    fromObject = return
    fromObjectUnsafe = id

-- By the magic of vim, i will create these.
instance NvimObject o => NvimObject (o, o) where
    toObject (o1, o2) = ObjectArray $ map toObject [o1, o2]
    fromObject (ObjectArray [o1, o2]) = (,)
        <$> fromObject o1
        <*> fromObject o2
    fromObject o = Left $ "Expected ObjectArray, but got " <> show o

instance NvimObject o => NvimObject (o, o, o) where
    toObject (o1, o2, o3) = ObjectArray $ map toObject [o1, o2, o3]
    fromObject (ObjectArray [o1, o2, o3]) = (,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
    fromObject o = Left $ "Expected ObjectArray, but got " <> show o

instance NvimObject o => NvimObject (o, o, o, o) where
    toObject (o1, o2, o3, o4) = ObjectArray $ map toObject [o1, o2, o3, o4]
    fromObject (ObjectArray [o1, o2, o3, o4]) = (,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
    fromObject o = Left $ "Expected ObjectArray, but got " <> show o

instance NvimObject o => NvimObject (o, o, o, o, o) where
    toObject (o1, o2, o3, o4, o5) = ObjectArray $ map toObject [o1, o2, o3, o4, o5]
    fromObject (ObjectArray [o1, o2, o3, o4, o5]) = (,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
    fromObject o = Left $ "Expected ObjectArray, but got " <> show o

instance NvimObject o => NvimObject (o, o, o, o, o, o) where
    toObject (o1, o2, o3, o4, o5, o6) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6]
    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6]) = (,,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
        <*> fromObject o6
    fromObject o = Left $ "Expected ObjectArray, but got " <> show o

instance NvimObject o => NvimObject (o, o, o, o, o, o, o) where
    toObject (o1, o2, o3, o4, o5, o6, o7) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7]
    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6, o7]) = (,,,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
        <*> fromObject o6
        <*> fromObject o7
    fromObject o = Left $ "Expected ObjectArray, but got " <> show o

instance NvimObject o => NvimObject (o, o, o, o, o, o, o, o) where
    toObject (o1, o2, o3, o4, o5, o6, o7, o8) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8]
    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8]) = (,,,,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
        <*> fromObject o6
        <*> fromObject o7
        <*> fromObject o8
    fromObject o = Left $ "Expected ObjectArray, but got " <> show o

instance NvimObject o => NvimObject (o, o, o, o, o, o, o, o, o) where
    toObject (o1, o2, o3, o4, o5, o6, o7, o8, o9) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6, o7, o8, o9]
    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9]) = (,,,,,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
        <*> fromObject o6
        <*> fromObject o7
        <*> fromObject o8
        <*> fromObject o9
    fromObject o = Left $ "Expected ObjectArray, but got " <> show o

