{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      :  Neovim.Classes
Description :  Type classes used for conversion of msgpack and Haskell types
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
-}
module Neovim.Classes (
    NvimObject (..),
    Dictionary,
    (+:),
    Generic,
    docToObject,
    docFromObject,
    docToText,
    Doc,
    AnsiStyle,
    Pretty (..),
    (<+>),
    module Data.Int,
    module Data.Word,
    module Control.DeepSeq,
) where

import Neovim.Exceptions (NeovimException (..))

import Control.Applicative (Applicative (liftA2))
import Control.Arrow ((***))
import Control.DeepSeq (NFData)
import Control.Monad ()
import Control.Monad.Except (
    MonadError (throwError),
 )
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Int (
    Int16,
    Int32,
    Int64,
    Int8,
 )
import qualified Data.Map.Strict as SMap
import Data.MessagePack
import Data.Monoid
import Data.Text as Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (
    Word,
    Word16,
    Word32,
    Word64,
    Word8,
 )
import GHC.Generics (Generic)
import Prettyprinter (
    Doc,
    Pretty (..),
    defaultLayoutOptions,
    layoutPretty,
    viaShow,
    (<+>),
 )
import qualified Prettyprinter as P
import Prettyprinter.Render.Terminal (
    AnsiStyle,
    renderStrict,
 )

import qualified Data.ByteString.UTF8 as UTF8 (fromString, toString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import UnliftIO.Exception (throwIO)

import Prelude

infixr 5 +:

{- | Convenient operator to create a list of 'Object' from normal values.
 @
 values +: of :+ different :+ types :+ can +: be +: combined +: this +: way +: []
 @
-}
(+:) :: (NvimObject o) => o -> [Object] -> [Object]
o +: os = toObject o : os

{- | Convert a 'Doc'-ument to a messagepack 'Object'. This is more a convenience
 method to transport error message from and to neovim. It generally does not
 hold that 'docToObject . docFromObject' = 'id'.
-}
docToObject :: Doc AnsiStyle -> Object
docToObject = ObjectString . encodeUtf8 . docToText

-- | See 'docToObject'.
docFromObject :: Object -> Either (Doc AnsiStyle) (Doc AnsiStyle)
docFromObject o = (P.viaShow :: Text -> Doc AnsiStyle) <$> fromObject o

docToText :: Doc AnsiStyle -> Text
docToText = renderStrict . layoutPretty defaultLayoutOptions

{- | A generic vim dictionary is a simply a map from strings to objects.  This
 type alias is sometimes useful as a type annotation especially if the
 OverloadedStrings extension is enabled.
-}
type Dictionary = SMap.Map ByteString Object

{- | Conversion from 'Object' files to Haskell types and back with respect
 to neovim's interpretation.

 The 'NFData' constraint has been added to allow forcing results of function
 evaluations in order to catch exceptions from pure code. This adds more
 stability to the plugin provider and seems to be a cleaner approach.
-}
class NFData o => NvimObject o where
    toObject :: o -> Object

    fromObjectUnsafe :: Object -> o
    fromObjectUnsafe o = case fromObject o of
        Left e ->
            error . show $
                "Not the expected object:"
                    <+> P.viaShow o
                    <+> P.lparen <> e <> P.rparen
        Right obj -> obj

    fromObject :: Object -> Either (Doc AnsiStyle) o
    fromObject = return . fromObjectUnsafe

    fromObject' :: (MonadIO io) => Object -> io o
    fromObject' = either (throwIO . ErrorMessage) return . fromObject

    {-# MINIMAL toObject, (fromObject | fromObjectUnsafe) #-}

-- Instances for NvimObject {{{1
instance NvimObject () where
    toObject _ = ObjectNil

    fromObject ObjectNil = return ()
    fromObject o = throwError $ "Expected ObjectNil, but got" <+> P.viaShow o

-- We may receive truthy values from neovim, so we should be more forgiving
-- here.
instance NvimObject Bool where
    toObject = ObjectBool

    fromObject (ObjectBool o) = return o
    fromObject (ObjectInt 0) = return False
    fromObject (ObjectUInt 0) = return False
    fromObject ObjectNil = return False
    fromObject (ObjectBinary "0") = return False
    fromObject (ObjectBinary "") = return False
    fromObject (ObjectString "0") = return False
    fromObject (ObjectString "") = return False
    fromObject _ = return True

instance NvimObject Double where
    toObject = ObjectDouble

    fromObject (ObjectDouble o) = return o
    fromObject (ObjectFloat o) = return $ realToFrac o
    fromObject (ObjectInt o) = return $ fromIntegral o
    fromObject (ObjectUInt o) = return $ fromIntegral o
    fromObject o =
        throwError $
            "Expected ObjectDouble, but got"
                <+> viaShow o

instance NvimObject Integer where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt o) = return $ toInteger o
    fromObject (ObjectUInt o) = return $ toInteger o
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected ObjectInt, but got" <+> viaShow o

instance NvimObject Int64 where
    toObject = ObjectInt

    fromObject (ObjectInt i) = return i
    fromObject (ObjectUInt o) = return $ fromIntegral o
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance NvimObject Int32 where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt i) = return $ fromIntegral i
    fromObject (ObjectUInt i) = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance NvimObject Int16 where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt i) = return $ fromIntegral i
    fromObject (ObjectUInt i) = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance NvimObject Int8 where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt i) = return $ fromIntegral i
    fromObject (ObjectUInt i) = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance NvimObject Word where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt i) = return $ fromIntegral i
    fromObject (ObjectUInt i) = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance NvimObject Word64 where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt i) = return $ fromIntegral i
    fromObject (ObjectUInt i) = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance NvimObject Word32 where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt i) = return $ fromIntegral i
    fromObject (ObjectUInt i) = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance NvimObject Word16 where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt i) = return $ fromIntegral i
    fromObject (ObjectUInt i) = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance NvimObject Word8 where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt i) = return $ fromIntegral i
    fromObject (ObjectUInt i) = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance NvimObject Int where
    toObject = ObjectInt . fromIntegral

    fromObject (ObjectInt i) = return $ fromIntegral i
    fromObject (ObjectUInt i) = return $ fromIntegral i
    fromObject (ObjectDouble o) = return $ round o
    fromObject (ObjectFloat o) = return $ round o
    fromObject o = throwError $ "Expected any Integer value, but got" <+> viaShow o

instance {-# OVERLAPPING #-} NvimObject [Char] where
    toObject = ObjectBinary . UTF8.fromString

    fromObject (ObjectBinary o) = return $ UTF8.toString o
    fromObject (ObjectString o) = return $ UTF8.toString o
    fromObject o = throwError $ "Expected ObjectString, but got" <+> viaShow o

instance {-# OVERLAPPABLE #-} NvimObject o => NvimObject [o] where
    toObject = ObjectArray . map toObject

    fromObject (ObjectArray os) = mapM fromObject os
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

instance NvimObject o => NvimObject (Maybe o) where
    toObject = maybe ObjectNil toObject

    fromObject ObjectNil = return Nothing
    fromObject o = either throwError (return . Just) $ fromObject o

instance NvimObject o => NvimObject (Vector o) where
    toObject = ObjectArray . V.toList . V.map toObject

    fromObject (ObjectArray os) = V.fromList <$> mapM fromObject os
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

-- | Right-biased instance for toObject.
instance (NvimObject l, NvimObject r) => NvimObject (Either l r) where
    toObject = either toObject toObject

    fromObject o = case fromObject o of
        Right r ->
            return $ Right r
        Left e1 -> case fromObject o of
            Right l ->
                return $ Left l
            Left e2 ->
                throwError $ e1 <+> "--" <+> e2

instance
    (Ord key, NvimObject key, NvimObject val) =>
    NvimObject (SMap.Map key val)
    where
    toObject =
        ObjectMap
            . SMap.fromList
            . map (toObject *** toObject)
            . SMap.toList

    fromObject (ObjectMap om) =
        SMap.fromList
            <$> ( traverse
                    ( uncurry (liftA2 (,))
                        . (fromObject *** fromObject)
                    )
                    . SMap.toList
                )
                om
    fromObject o = throwError $ "Expected ObjectMap, but got" <+> viaShow o

instance NvimObject Text where
    toObject = ObjectBinary . encodeUtf8

    fromObject (ObjectBinary o) = return $ decodeUtf8 o
    fromObject (ObjectString o) = return $ decodeUtf8 o
    fromObject o = throwError $ "Expected ObjectBinary, but got" <+> viaShow o

instance NvimObject ByteString where
    toObject = ObjectBinary

    fromObject (ObjectBinary o) = return o
    fromObject (ObjectString o) = return o
    fromObject o = throwError $ "Expected ObjectBinary, but got" <+> viaShow o

instance NvimObject Object where
    toObject = id

    fromObject = return
    fromObjectUnsafe = id

-- By the magic of vim, i will create these.
instance (NvimObject o1, NvimObject o2) => NvimObject (o1, o2) where
    toObject (o1, o2) = ObjectArray [toObject o1, toObject o2]

    fromObject (ObjectArray [o1, o2]) =
        (,)
            <$> fromObject o1
            <*> fromObject o2
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

instance (NvimObject o1, NvimObject o2, NvimObject o3) => NvimObject (o1, o2, o3) where
    toObject (o1, o2, o3) = ObjectArray [toObject o1, toObject o2, toObject o3]

    fromObject (ObjectArray [o1, o2, o3]) =
        (,,)
            <$> fromObject o1
            <*> fromObject o2
            <*> fromObject o3
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

instance (NvimObject o1, NvimObject o2, NvimObject o3, NvimObject o4) => NvimObject (o1, o2, o3, o4) where
    toObject (o1, o2, o3, o4) = ObjectArray [toObject o1, toObject o2, toObject o3, toObject o4]

    fromObject (ObjectArray [o1, o2, o3, o4]) =
        (,,,)
            <$> fromObject o1
            <*> fromObject o2
            <*> fromObject o3
            <*> fromObject o4
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

instance (NvimObject o1, NvimObject o2, NvimObject o3, NvimObject o4, NvimObject o5) => NvimObject (o1, o2, o3, o4, o5) where
    toObject (o1, o2, o3, o4, o5) = ObjectArray [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5]

    fromObject (ObjectArray [o1, o2, o3, o4, o5]) =
        (,,,,)
            <$> fromObject o1
            <*> fromObject o2
            <*> fromObject o3
            <*> fromObject o4
            <*> fromObject o5
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

instance (NvimObject o1, NvimObject o2, NvimObject o3, NvimObject o4, NvimObject o5, NvimObject o6) => NvimObject (o1, o2, o3, o4, o5, o6) where
    toObject (o1, o2, o3, o4, o5, o6) = ObjectArray [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5, toObject o6]

    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6]) =
        (,,,,,)
            <$> fromObject o1
            <*> fromObject o2
            <*> fromObject o3
            <*> fromObject o4
            <*> fromObject o5
            <*> fromObject o6
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

instance (NvimObject o1, NvimObject o2, NvimObject o3, NvimObject o4, NvimObject o5, NvimObject o6, NvimObject o7) => NvimObject (o1, o2, o3, o4, o5, o6, o7) where
    toObject (o1, o2, o3, o4, o5, o6, o7) = ObjectArray [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5, toObject o6, toObject o7]

    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6, o7]) =
        (,,,,,,)
            <$> fromObject o1
            <*> fromObject o2
            <*> fromObject o3
            <*> fromObject o4
            <*> fromObject o5
            <*> fromObject o6
            <*> fromObject o7
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

instance (NvimObject o1, NvimObject o2, NvimObject o3, NvimObject o4, NvimObject o5, NvimObject o6, NvimObject o7, NvimObject o8) => NvimObject (o1, o2, o3, o4, o5, o6, o7, o8) where
    toObject (o1, o2, o3, o4, o5, o6, o7, o8) = ObjectArray [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5, toObject o6, toObject o7, toObject o8]

    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8]) =
        (,,,,,,,)
            <$> fromObject o1
            <*> fromObject o2
            <*> fromObject o3
            <*> fromObject o4
            <*> fromObject o5
            <*> fromObject o6
            <*> fromObject o7
            <*> fromObject o8
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

instance (NvimObject o1, NvimObject o2, NvimObject o3, NvimObject o4, NvimObject o5, NvimObject o6, NvimObject o7, NvimObject o8, NvimObject o9) => NvimObject (o1, o2, o3, o4, o5, o6, o7, o8, o9) where
    toObject (o1, o2, o3, o4, o5, o6, o7, o8, o9) = ObjectArray [toObject o1, toObject o2, toObject o3, toObject o4, toObject o5, toObject o6, toObject o7, toObject o8, toObject o9]

    fromObject (ObjectArray [o1, o2, o3, o4, o5, o6, o7, o8, o9]) =
        (,,,,,,,,)
            <$> fromObject o1
            <*> fromObject o2
            <*> fromObject o3
            <*> fromObject o4
            <*> fromObject o5
            <*> fromObject o6
            <*> fromObject o7
            <*> fromObject o8
            <*> fromObject o9
    fromObject o = throwError $ "Expected ObjectArray, but got" <+> viaShow o

-- 1}}}
