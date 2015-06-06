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
    ( generateAPI
    , function
    , function'
    , command
    , command'
    , autocmd
    , defaultAPITypeToHaskellTypeMap
    , module Control.Exception.Lifted
    , module Neovim.API.Classes
    , module Data.Data
    , module Data.MessagePack
    ) where

import           Neovim.API.Classes
import           Neovim.API.Context
import           Neovim.API.Parser
import           Neovim.API.Plugin        (ExportedFunctionality (..))
import           Neovim.RPC.FunctionCall

import           Language.Haskell.TH

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent.STM   (STM)
import           Control.Exception
import           Control.Exception.Lifted
import           Control.Monad
import           Data.ByteString          (ByteString)
import           Data.Char                (toUpper, isUpper)
import           Data.Data                (Data, Typeable)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.MessagePack
import           Data.Monoid
import           Data.Text                (pack)

import           Prelude

-- | Generate the API types and functions provided by @nvim --api-info@.
--
-- The provided map allows the use of different Haskell types for the types
-- defined in the API. The types must be an instance of 'NvimObject' and they
-- must form an isomorphism with the sent messages types. Currently, it
-- provides a Convenient way to replace the String@ type with 'Text',
-- 'ByteString' or 'String'.
generateAPI :: Map String (Q Type) -> Q [Dec]
generateAPI typeMap = do
    api <- either fail return =<< runIO parseAPI
    let exceptionName = mkName "NeovimExceptionGen"
        exceptions = (\(n,i) -> (mkName ("Neovim" <> n), i)) <$> errorTypes api
        customTypesN = first mkName <$> customTypes api
    join <$> sequence
        [ fmap return . createDataTypeWithByteStringComponent exceptionName $ fst <$> exceptions
        , exceptionInstance exceptionName
        , customTypeInstance exceptionName exceptions
        , mapM (\n -> createDataTypeWithByteStringComponent n [n]) $ fst <$> customTypesN
        , join <$> mapM (\(n,i) -> customTypeInstance n [(n,i)]) customTypesN
        , fmap join . mapM (createFunction typeMap) $ functions api
        ]

-- | Default type mappings for the requested API.
defaultAPITypeToHaskellTypeMap :: Map String (Q Type)
defaultAPITypeToHaskellTypeMap = Map.fromList
    [ ("Boolean"   , [t|Bool|])
    , ("Integer"   , [t|Int64|])
    , ("Float"     , [t|Double|])
    , ("Array"     , [t|Object|])
    , ("Dictionary", [t|Map Object Object|])
    , ("void"      , [t|()|])
    ]

apiTypeToHaskellType :: Map String (Q Type) -> NeovimType -> Q Type
apiTypeToHaskellType typeMap at = case at of
    Void -> [t|()|]
    NestedType t Nothing ->
        appT listT $ apiTypeToHaskellType typeMap t
    NestedType t (Just n) ->
        foldl appT (tupleT n) . replicate n $ apiTypeToHaskellType typeMap t
    SimpleType t ->
        fromMaybe ((conT . mkName) t) $ Map.lookup t typeMap

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
createFunction :: Map String (Q Type) -> NeovimFunction -> Q [Dec]
createFunction typeMap nf = do
    let withDeferred | deferred nf = appT [t|STM|]
                     | otherwise   = id
        withException | canFail nf = appT [t|Either Object|]
                      | otherwise  = id

        callFn | deferred nf && canFail nf = [|acall|]
               | deferred nf               = [|acall'|]
               | canFail nf                = [|scall|]
               | otherwise                 = [|scall'|]

        functionName = (mkName . name) nf
        toObjVar v = [|toObject $(varE v)|]


    ret <- let (r,st) = ((mkName "r"), (mkName "st"))
           in forallT [PlainTV r, PlainTV st] (return []) $ appT ([t|Neovim $(varT r) $(varT st) |])
            . withDeferred . withException
            . apiTypeToHaskellType typeMap $ returnType nf

    vars <- mapM (\(t,n) -> (,) <$> apiTypeToHaskellType typeMap t
                                <*> newName n)
            $ parameters nf
    sequence
        [ sigD functionName . return
            . foldr (AppT . AppT ArrowT) ret $ map fst vars
        , funD functionName
            [ clause
                (map (varP . snd) vars)
                (normalB (callFn
                    `appE` ([| pack |] `appE` (litE . stringL . name) nf)
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
--     fromObject (ObjectExt 1 bs) = return $ Bar bs
--     fromObject (ObjectExt 2 bs) = return $ Quz bs
--     fromObject o = Left $ "Object is not convertible to: Foo Received: " <> show o
-- @
customTypeInstance :: Name -> [(Name, Int64)] -> Q [Dec]
customTypeInstance typeName nis =
    let fromObjectClause :: Name -> Int64 -> Q Clause
        fromObjectClause n i = newName "bs" >>= \bs ->
            clause
                [ conP (mkName "ObjectExt")
                    [(litP . integerL . fromIntegral) i,varP bs]
                ]
                (normalB [|return $ $(conE n) $(varE bs)|])
                []
        fromObjectErrorClause :: Q Clause
        fromObjectErrorClause = do
            o <- newName "o"
            let n = nameBase typeName
            clause
                [ varP o ]
                (normalB [|Left $ "Object is not convertible to: " <> n <> " Received: " <> show $(varE o)|])
                []

        toObjectClause :: Name -> Int64 -> Q Clause
        toObjectClause n i = newName "bs" >>= \bs ->
            clause
                [conP n [varP bs]]
                (normalB [|ObjectExt $((litE . integerL . fromIntegral) i) $(varE bs)|])
                []

    in return <$> instanceD
        (return [])
        ([t|NvimObject|] `appT` conT typeName)
        [ funD (mkName "toObject") $ map (uncurry toObjectClause) nis
        , funD (mkName "fromObject")
            $ map (uncurry fromObjectClause) nis
            <> [fromObjectErrorClause]
        ]

function :: String -> Name -> Q Exp
function customName functionName
    | null customName = error "Empty names are not allowed for exported functions."
    | otherwise = [|Function (pack $(litE (StringL customName))) $(functionImplementation functionName) |]

function' :: Name -> Q Exp
function' functionName = function (nameBase functionName) functionName

command :: String -> Name -> Q Exp
command [] _ = error "Empty names are not allowed for exported commands."
command customFunctionName@(c:_) functionName
    | (not . isUpper) c = error $ "Custom command name must start with a capiatl letter: " <> show customFunctionName
    | otherwise = [|Command (pack $(litE (StringL customFunctionName))) $(functionImplementation functionName)|]

command' :: Name -> Q Exp
command' functionName =
    let (c:cs) = nameBase functionName
    in command (toUpper c:cs) functionName

autocmd :: Name -> Q Exp
autocmd functionName =
    [|\t f -> AutoCmd t f $(functionImplementation functionName)|]

-- | Generate a function of type @[Object] -> Neovim' Object@ from the argument
-- function.
--
-- The function
-- @
-- add :: Int -> Int -> Int
-- add = (+)
-- @
-- will be converted to
-- @
-- \args -> case args of
--     [x,y] -> case pure add <*> fromObject x <*> fromObject y of
--         Left e -> err $ "Wrong type of arguments for add: " ++ e
--         Right action -> toObject <$> action
--     _ -> err $ "Wrong number of arguments for add: " ++ show xs
-- @
--
functionImplementation :: Name -> Q Exp
functionImplementation functionName = do
    fInfo <- reify functionName
    -- We only need the number of arguments to generate the appropriate function
    let nargs = case fInfo of
            VarI _ functionType _ _ -> determineNumberOfArguments functionType
            x -> error $ "Value given to function is (likely) not the name of a function.\n"
                            <> show x
    topLevelCase nargs

  where

    determineNumberOfArguments :: Type -> Int
    determineNumberOfArguments ft = case ft of
        AppT (AppT ArrowT _) r -> 1 + determineNumberOfArguments r
        _ -> 0
    -- \args -> case args of ...
    topLevelCase :: Int -> Q Exp
    topLevelCase n = newName "args" >>= \args ->
        lamE [varP args] (caseE (varE args) [matchingCase n, errorCase])

    -- _ -> err "Wrong number of arguments"
    errorCase :: Q Match
    errorCase = match wildP (normalB [|err "Wrong number of arguments."|]) []

    -- [x,y] -> case pure add <*> fromObject x <*> fromObject y of ...
    matchingCase :: Int -> Q Match
    matchingCase n = mapM (\_ -> newName "x") [1..n] >>= \vars ->
        match (listP (map varP vars))
              (normalB
                (caseE
                    (foldl genArgumentCast [|pure $(varE functionName)|]
                        (zip vars (repeat [|(<*>)|])))
                  [successfulEvaluation, failedEvaluation]))
              []

    genArgumentCast :: Q Exp -> (Name, Q Exp) -> Q Exp
    genArgumentCast e (v,op) = infixE (Just e) op (Just [|fromObject $(varE v)|])

    successfulEvaluation :: Q Match
    successfulEvaluation = newName "action" >>= \action ->
        match (conP (mkName "Right") [varP action])
              (normalB [|toObject <$> $(varE action)|])
              []
    failedEvaluation :: Q Match
    failedEvaluation = newName "e" >>= \e ->
        match (conP (mkName "Left") [varP e])
              (normalB [|err $(varE e)|])
              []

