{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{- |
Module      :  Neovim.Classes
Description :  Type classes used for conversion of msgpack and Haskell types
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.Classes
    ( NvimObject(..)
    , Dictionary

    , module Data.Int
    , module Data.Word
    ) where


import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Except
import           Data.ByteString      (ByteString)
import           Data.Int             (Int16, Int32, Int64, Int8)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.MessagePack
import           Data.Monoid
import           Data.Text            as Text (Text)
import           Data.Traversable     hiding (forM, mapM)
import           Data.Word            (Word, Word16, Word32, Word64, Word8)

import           Prelude

-- FIXME saep 2014-11-28 Is assuming UTF-8 reasonable?
import qualified Data.ByteString.UTF8 as U (fromString, toString)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)


-- | A generic vim dictionary is a simply a map from strings to objects.  This
-- type alias is sometimes useful as a type annotation especially if the
-- OverloadedStrings extension is enabled.
type Dictionary = Map ByteString Object


-- | Conversion from 'Object' files to Haskell types and back with respect
-- to neovim's interpretation.
class NvimObject o where
    toObject :: o -> Object

    fromObjectUnsafe :: Object -> o
    fromObjectUnsafe o = case fromObject o of
        Left e -> error $ unwords
            [ "Not the expected object:"
            , show o , "(", e, ")"
            ]
        Right obj -> obj

    fromObject :: (NvimObject o) => Object -> Either String o
    fromObject = return . fromObjectUnsafe


-- Instances for NvimObject {{{1
instance NvimObject () where
    toObject _           = ObjectNil

    fromObject ObjectNil = return ()
    fromObject o         = throwError $ "Expected ObjectNil, but got " <> show o


-- We may receive truthy values from neovim, so we should be more forgiving
-- here.
instance NvimObject Bool where
    toObject                      = ObjectBool

    fromObject (ObjectBool o)     = return o
    fromObject (ObjectInt  0)     = return False
    fromObject ObjectNil          = return False
    fromObject (ObjectBinary "0") = return False
    fromObject (ObjectBinary "")  = return False
    fromObject (ObjectString "0") = return False
    fromObject (ObjectString "")  = return False
    fromObject _                  = return True


instance NvimObject Double where
    toObject                    = ObjectDouble

    fromObject (ObjectDouble o) = return o
    fromObject (ObjectFloat o)  = return $ realToFrac o
    fromObject (ObjectInt o)    = return $ fromIntegral o
    fromObject o                = throwError $ "Expected ObjectDouble, but got " <> show o


instance NvimObject Integer where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt o)    = return $ toInteger o
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected ObjectInt, but got " <> show o


instance NvimObject Int64 where
    toObject                    = ObjectInt

    fromObject (ObjectInt i)    = return i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Int32 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt i)    = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Int16 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt i)    = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Int8 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt i)    = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Word where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt i)    = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Word64 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt i)    = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Word32 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt i)    = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Word16 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt i)    = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Word8 where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt i)    = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Int where
    toObject                    = ObjectInt . fromIntegral

    fromObject (ObjectInt i)    = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o)  = return $ round o
    fromObject o                = throwError $ "Expected any Integer value, but got " <> show o


instance NvimObject Char where
    toObject c = ObjectBinary . U.fromString $ [c]

    fromObject str = case fromObject str of
        Right [c] -> return c
        _   -> throwError $ "Expected one element string but got: " <> show str


instance NvimObject [Char] where
    toObject                    = ObjectBinary . U.fromString

    fromObject (ObjectBinary o) = return $ U.toString o
    fromObject (ObjectString o) = return $ U.toString o
    fromObject o                = throwError $ "Expected ObjectBinary, but got " <> show o


instance NvimObject o => NvimObject [o] where
    toObject                    = ObjectArray . map toObject

    fromObject (ObjectArray os) = mapM fromObject os
    fromObject o                = throwError $ "Expected ObjectArray, but got " <> show o


instance NvimObject o => NvimObject (Maybe o) where
    toObject = maybe ObjectNil toObject

    fromObject ObjectNil = return Nothing
    fromObject o = either throwError (return . Just) $ fromObject o


instance (Ord key, NvimObject key, NvimObject val)
        => NvimObject (Map key val) where
    toObject = ObjectMap
        . Map.fromList . map (toObject *** toObject) . Map.toList

    fromObject (ObjectMap om) = Map.fromList <$>
        (sequenceA
            . map (uncurry (liftA2 (,))
                    . (fromObject *** fromObject))
                . Map.toList) om

    fromObject o = throwError $ "Expected ObjectMap, but got " <> show o


instance NvimObject Text where
    toObject                    = ObjectBinary . encodeUtf8

    fromObject (ObjectBinary o) = return $ decodeUtf8 o
    fromObject (ObjectString o) = return $ decodeUtf8 o
    fromObject o                = throwError $ "Expected ObjectBinary, but got " <> show o


instance NvimObject ByteString where
    toObject                    = ObjectBinary

    fromObject (ObjectBinary o) = return o
    fromObject o                = throwError $ "Expected ObjectBinary, but got " <> show o


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
    fromObject o = throwError $ "Expected ObjectArray, but got " <> show o

instance NvimObject o => NvimObject (o, o, o) where
    toObject (o1, o2, o3) = ObjectArray $ map toObject [o1, o2, o3]

    fromObject (ObjectArray [o1, o2, o3]) = (,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
    fromObject o = throwError $ "Expected ObjectArray, but got " <> show o


instance NvimObject o => NvimObject (o, o, o, o) where
    toObject (o1, o2, o3, o4) = ObjectArray $ map toObject [o1, o2, o3, o4]

    fromObject (ObjectArray [o1, o2, o3, o4]) = (,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
    fromObject o = throwError $ "Expected ObjectArray, but got " <> show o


instance NvimObject o => NvimObject (o, o, o, o, o) where
    toObject (o1, o2, o3, o4, o5) = ObjectArray $ map toObject [o1, o2, o3, o4, o5]

    fromObject (ObjectArray [o1, o2, o3, o4, o5]) = (,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
    fromObject o = throwError $ "Expected ObjectArray, but got " <> show o


instance NvimObject o => NvimObject (o, o, o, o, o, o) where
    toObject (o1, o2, o3, o4, o5, o6) = ObjectArray $ map toObject [o1, o2, o3, o4, o5, o6]

    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6]) = (,,,,,)
        <$> fromObject o1
        <*> fromObject o2
        <*> fromObject o3
        <*> fromObject o4
        <*> fromObject o5
        <*> fromObject o6
    fromObject o = throwError $ "Expected ObjectArray, but got " <> show o


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
    fromObject o = throwError $ "Expected ObjectArray, but got " <> show o


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
    fromObject o = throwError $ "Expected ObjectArray, but got " <> show o


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
    fromObject o = throwError $ "Expected ObjectArray, but got " <> show o


-- 1}}}
