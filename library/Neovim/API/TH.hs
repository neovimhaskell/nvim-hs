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
    ) where

import           Neovim.API.Classes
import           Neovim.API.Parser

import           Language.Haskell.TH

import           Control.Applicative
import           Control.Exception
import           Control.Exception.Lifted
import           Control.Monad
import           Data.Data                (Data, Typeable)
import           Data.MessagePack
import           Data.Monoid

generateAPITypes :: Q [Dec]
generateAPITypes = do
    api <- either fail return =<< runIO parseAPI
    let exceptionName = mkName "NeovimException"
        exceptions = (\(n,i) -> (mkName ("Neovim" <> n), i)) <$> errorTypes api
        customTypesN = (\(n,i) -> (mkName n, i)) <$> customTypes api
    join <$> sequence
        [ fmap return . createDataTypeWithObjectComponent exceptionName $ fst <$> exceptions
        , exceptionInstance exceptionName
        , customTypeInstance exceptionName exceptions
        , mapM (\n -> createDataTypeWithObjectComponent n [n]) $ fst <$> customTypesN
        , fmap join $ mapM (\(n,i) -> customTypeInstance n [(n,i)]) customTypesN
        ]

-- | @ createDataTypeWithObjectComponent SomeName [Foo,Bar]@
-- will create this:
-- @
-- data SomeName = Foo !Object
--               | Bar !Object
--               deriving (Typeable, Eq, Show)
-- @
--
createDataTypeWithObjectComponent :: Name -> [Name] -> Q Dec
createDataTypeWithObjectComponent n cs = do
        tObject <- [t|Object|]
        dataD
            (return [])
            n
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
-- instance CustomType Foo where
--     getID (Bar _) = 1
--     getID (Quz _) = 2
--
--     getConstructor 1 = Bar
--     getConstructor 2 = Quz
-- @
customTypeInstance :: Name -> [(Name, Int64)] -> Q [Dec]
customTypeInstance typeName nis =
    let getConstructorClause n i = clause
            [(litP . integerL . toInteger) i]
            (normalB (conE n))
            []

        getIDClause n i = clause
            [conP n [ [p|_|] ] ]
            (normalB ((litE . IntegerL . toInteger) i))
            []

        payloadClause n = [p|p|] >>= \(VarP p) -> clause
            [conP n [return (VarP p)] ]
            (normalB (varE p))
            []

    in sequence
            [ instanceD
                (return [])
                ([t|CustomType|] `appT` conT typeName)
                [ funD (mkName "getConstructor") $ map (uncurry getConstructorClause) nis
                , funD (mkName "getID") $ map (uncurry getIDClause) nis
                , funD (mkName "payload") $ map (payloadClause . fst) nis
                ]
            ]

