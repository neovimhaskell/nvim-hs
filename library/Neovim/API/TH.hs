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
import           Neovim.API.Parser

import           Language.Haskell.TH

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent.STM   (TVar)
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
        customTypesN = (\(n,i) -> (mkName n, i)) <$> customTypes api
    join <$> sequence
        [ fmap return . createDataTypeWithByteStringComponent exceptionName $ fst <$> exceptions
        , exceptionInstance exceptionName
        , customTypeInstance exceptionName exceptions
        , mapM (\n -> createDataTypeWithByteStringComponent n [n]) $ fst <$> customTypesN
        , fmap join $ mapM (\(n,i) -> customTypeInstance n [(n,i)]) customTypesN
        , fmap join . mapM createFunction $ functions api
        ]

apiTypeToHaskellTypeMap :: Map String String
apiTypeToHaskellTypeMap = Map.fromList
    [ ("Boolean", "Bool")
    , ("Integer", "Int64")
    ]

apiTypeToHaskellType :: String -> String
apiTypeToHaskellType at = fromMaybe at $ Map.lookup at apiTypeToHaskellTypeMap

createFunction :: NeovimFunction -> Q [Dec]
createFunction nf = do
    let rt = mkName $ apiTypeToHaskellType (returnType nf)
    vars <- mapM (\(t,n) -> (,) <$> newName t <*> newName n) $ parameters nf
    return [] -- TODO saep 2014-11-28

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


