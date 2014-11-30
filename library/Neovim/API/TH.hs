{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{- |
Module      :  Neovim.API.TH
Description :  Template Haskell API generation module
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.TH
    ( generateAPITypes
    , module Control.Exception.Lifted
    , module Neovim.API.Classes
    , module Data.Data
    , module Data.MessagePack
    ) where

import           Neovim.API.Classes
import           Neovim.API.Context
import           Neovim.API.Parser

import           Language.Haskell.TH

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent.STM   (STM)
import           Control.Exception
import           Control.Exception.Lifted
import           Control.Monad
import           Data.ByteString          (ByteString)
import           Data.Data                (Data, Typeable)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.MessagePack
import           Data.Monoid

generateAPITypes :: Q [Dec]
generateAPITypes = do
    api <- either fail return =<< runIO parseAPI
    let exceptionName = mkName "NeovimException"
        exceptions = (\(n,i) -> (mkName ("Neovim" <> n), i)) <$> errorTypes api
        customTypesN = first mkName <$> customTypes api
    join <$> sequence
        [ fmap return . createDataTypeWithByteStringComponent exceptionName $ fst <$> exceptions
        , exceptionInstance exceptionName
        , customTypeInstance exceptionName exceptions
        , mapM (\n -> createDataTypeWithByteStringComponent n [n]) $ fst <$> customTypesN
        , join <$> mapM (\(n,i) -> customTypeInstance n [(n,i)]) customTypesN
        , fmap join . mapM createFunction $ functions api
        ]

apiTypeToHaskellTypeMap :: Map String (Q Type)
apiTypeToHaskellTypeMap = Map.fromList
    [ ("Boolean", [t|Bool|])
    , ("Integer", [t|Int64|])
    , ("Float"  , [t|Double|])
    , ("Array"  , [t|Object|])
    ]

apiTypeToHaskellType :: NeovimType -> Maybe (Q Type)
apiTypeToHaskellType at = case at of
    Void -> Nothing
    NestedType t Nothing ->
        appT listT <$> apiTypeToHaskellType t
    NestedType t (Just n) ->
        foldl appT (tupleT n) . replicate n <$> apiTypeToHaskellType t
    SimpleType t ->
        return . fromMaybe ((conT . mkName) t) $ Map.lookup t apiTypeToHaskellTypeMap

-- | This function will create a wrapper function with neovim's function name
-- as its name.
--
-- Synchronous function:
-- @
-- buffer_get_number :: Buffer -> Neovim Int64
-- buffer_get_number buffer = scall "buffer_get_number" [toObject buffer]
-- @
--
-- Asynchronous function:
-- @
-- vim_eval :: String -> Neovim (TMVar Object)
-- vim_eval str = acall "vim_eval" [toObject str]
-- @
--
-- Asynchronous function without a return value:
-- @
-- vim_feed_keys :: String -> String -> Bool -> Neovim ()
-- vim_feed_keys keys mode escape_csi =
--     acallVoid "vim_feed_keys" [ toObject keys
--                               , toObject mode
--                               , toObject escape_csi
--                               ]
-- @
--
createFunction :: NeovimFunction -> Q [Dec]
createFunction nf = do
    let resultMod | deferred nf = appT [t|STM|]
                  | otherwise   = id

        callFn | returnType nf == Void = [|acallVoid|]
               | deferred nf           = [|acall|]
               | otherwise             = [|scall|]

        functionName = (mkName . name) nf
        toObjVar v = [|toObject $(varE v)|]

    ret <- [t|Neovim|] `appT` case (apiTypeToHaskellType . returnType) nf of
        Nothing -> [t|()|]
        Just t -> resultMod t

    -- fromJust should be safe here as void parameters do not make a lot of
    -- sense and probably are an error in the API
    vars <- mapM (\(t,n) -> (,) <$> (fromJust . apiTypeToHaskellType) t
                                <*> newName n)
            $ parameters nf
    sequence
        [ sigD functionName . return
            . foldr (AppT . AppT ArrowT) ret $ map fst vars
        , funD functionName
            [ clause
                (map (varP . snd) vars)
                (normalB (callFn
                    `appE` (litE . stringL . name) nf
                    `appE` listE (map (toObjVar . snd) vars)))
                []
            ]
        ]

-- | @ createDataTypeWithObjectComponent SomeName [Foo,Bar]@
-- will create this:
-- @
-- data SomeName = Foo !Object
--               | Bar !Object
--               deriving (Typeable, Eq, Show)
-- @
--
createDataTypeWithByteStringComponent :: Name -> [Name] -> Q Dec
createDataTypeWithByteStringComponent nme cs = do
        tObject <- [t|ByteString|]
        dataD
            (return [])
            nme
            []
            (map (\n-> normalC n [return (IsStrict, tObject)]) cs)
            (mkName <$> ["Typeable", "Eq", "Show"])

-- | If the first parameter is @mkName NeovimException@, this function will
-- generate  @instance Exception NeovimException"@.
exceptionInstance :: Name -> Q [Dec]
exceptionInstance exceptionName = return <$>
    instanceD
        (return [])
        ([t|Exception|] `appT` conT exceptionName)
        []

-- | @customTypeInstance Foo [(Bar, 1), (Quz, 2)]@
-- will create this:
-- @
-- instance Serializable Foo where
--     toObject (Bar bs) = ObjectExt 1 bs
--     toObject (Quz bs) = ObjectExt 2 bs
--     fromObject (ObjectExt 1 bs) = Bar bs
--     fromObject (ObjectExt 2 bs) = Quz bs
-- @
customTypeInstance :: Name -> [(Name, Int64)] -> Q [Dec]
customTypeInstance typeName nis =
    let patTilde = case nis of
                       [_] -> tildeP
                       _   -> id

        fromObjectClause :: Name -> Int64 -> Q Clause
        fromObjectClause n i = newName "bs" >>= \bs -> clause
            [ patTilde
                (conP (mkName "ObjectExt")
                    [(litP . integerL . fromIntegral) i,varP bs])
            ]
            (normalB [|$(conE n) $(varE bs)|])
            []

        toObjectClause n i = newName "bs" >>= \bs -> clause
            [conP n [varP bs]]
            (normalB [|ObjectExt $((litE. integerL . fromIntegral) i) $(varE bs)|])
            []

    in return <$> instanceD
        (return [])
        ([t|NvimInstance|] `appT` conT typeName)
        [ funD (mkName "toObject") $ map (uncurry toObjectClause) nis
        , funD (mkName "fromObject") $ map (uncurry fromObjectClause) nis
        ]


