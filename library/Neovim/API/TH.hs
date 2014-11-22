{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Neovim.API.TH
Description :  Template Haskell API generation module
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.TH
    ( generateAPI
    , module Control.Exception.Lifted
    , module Neovim.API.Classes
    , module Data.Data
    ) where

import Neovim.API.Parser
import Neovim.API.Classes

import Language.Haskell.TH

import Data.Data (Data, Typeable)
import Control.Applicative
import Data.Monoid
import Control.Exception.Lifted

generateAPI :: Q [Dec]
generateAPI = do
    api <- either fail return =<< runIO parseAPI
    let exceptionName = mkName "NeovimException"
        exceptions = (\(n,i) -> (mkName ("Neovim" <> n), i)) <$> errorTypes api
    concat <$> sequence
        [ exceptionDataType exceptionName $ fst <$> exceptions
        , exceptionInstance exceptionName
        , idInstance exceptionName exceptions
        ]
-- | For every error type in the deserialized 'Object', create
-- a constructor in the NeovimException data type.
exceptionDataType :: Name -> [Name] -> Q [Dec]
exceptionDataType exceptionName names = return <$>
    dataD
        (return [])
        exceptionName
        []
        (map (\name -> normalC name []) names)
        (mkName <$> ["Data", "Typeable", "Eq", "Show"])

-- | If the first parameter is @mkName NeovimException@, this function will
-- generate  @instance Exception NeovimException"@.
exceptionInstance :: Name -> Q [Dec]
exceptionInstance exceptionName = return <$>
    instanceD
        (return [])
        ((conT . mkName) "Exception" `appT` conT exceptionName)
        []

-- | Create an 'ID' instance for the given type name and the list of
-- Constructor names paired with the 'Int64' value representing their
-- identifier.
idInstance :: Name -> [(Name, Int64)] -> Q [Dec]
idInstance typeName nis =
    let fromIDClause n i = clause
            [(litP . integerL . toInteger) i]
            (normalB (conE n))
            []

        toIDClause n i = clause
            [conP n []]
            (normalB ((litE . IntegerL . toInteger) i))
            []

    in sequence
            [ instanceD
                (return [])
                ((conT . mkName) "ID" `appT` conT typeName)
                [ funD (mkName "fromID") $ map (uncurry fromIDClause) nis
                , funD (mkName "toID") $ map (uncurry toIDClause) nis
                ]
            ]


