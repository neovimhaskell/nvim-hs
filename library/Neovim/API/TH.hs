{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE CPP #-}
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
    , module Neovim.Classes
    , module Data.Data
    , module Data.MessagePack
    ) where

import           Neovim.API.Parser
import           Neovim.Classes
import           Neovim.Context
import           Neovim.Plugin.Classes    (CommandArguments (..),
                                           CommandOption (..),
                                           FunctionalityDescription (..),
                                           FunctionName(..),
                                           mkCommandOptions)
import           Neovim.Plugin.Internal   (ExportedFunctionality (..))
import           Neovim.RPC.FunctionCall

import           Language.Haskell.TH

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Concurrent.STM   (STM)
import           Control.Exception
import           Control.Exception.Lifted
import           Control.Monad
import           Data.ByteString          (ByteString)
import           Data.ByteString.UTF8     (fromString)
import           Data.Char                (isUpper, toUpper)
import           Data.Data                (Data, Typeable)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe
import           Data.MessagePack
import           Data.Monoid
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import           Text.PrettyPrint.ANSI.Leijen (text, (<+>), Doc)

import           Prelude

dataD' :: CxtQ -> Name -> [TyVarBndr] -> [ConQ] -> [Name] -> DecQ
#if __GLASGOW_HASKELL__ < 800
dataD' = dataD
#else
dataD' cxtQ n tyvarbndrs conq ns =
    dataD cxtQ n tyvarbndrs Nothing conq (mapM conT ns)
#endif

-- | Generate the API types and functions provided by @nvim --api-info@.
--
-- The provided map allows the use of different Haskell types for the types
-- defined in the API. The types must be an instance of 'NvimObject' and they
-- must form an isomorphism with the sent messages types. Currently, it
-- provides a Convenient way to replace the /String/ type with 'Text',
-- 'ByteString' or 'String'.
generateAPI :: Map String (Q Type) -> Q [Dec]
generateAPI typeMap = do
    api <- either (fail . show) return =<< runIO parseAPI
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
    , ("Array"     , [t|[Object]|])
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
    let withDeferred | async nf    = appT [t|STM|]
                     | otherwise   = id

        withException | canFail nf = appT [t|Either NeovimException|]
                      | otherwise  = id

        callFn | async nf && canFail nf = [|acall|]
               | async nf               = [|acall'|]
               | canFail nf             = [|scall|]
               | otherwise              = [|scall'|]

        functionName = (mkName . name) nf
        toObjVar v = [|toObject $(varE v)|]


    ret <- let (r,st) = (mkName "r", mkName "st")
           in forallT [PlainTV r, PlainTV st] (return [])
            . appT ([t|Neovim $(varT r) $(varT st) |])
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
                    `appE` ([| (F . fromString) |] `appE` (litE . stringL . name) nf)
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
#if __GLASGOW_HASKELL__ < 800
        let strictNess = (IsStrict, tObject)
#else
        let strictNess = (Bang NoSourceUnpackedness SourceStrict, tObject)
#endif
        dataD'
            (return [])
            nme
            []
            (map (\n-> normalC n [return strictNess]) cs)
            (mkName <$> ["Typeable", "Eq", "Show"])


-- | If the first parameter is @mkName NeovimException@, this function will
-- generate  @instance Exception NeovimException@.
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
                (normalB [|throwError $
                            text "Object is not convertible to:"
                            <+> text n
                            <+> text "Received:" <+> (text . show) $(varE o)|])
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


-- | Define an exported function by providing a custom name and referencing the
-- function you want to export.
--
-- Note that the name must start with an upper case letter.
--
-- Example: @ $(function \"MyExportedFunction\" 'myDefinedFunction) 'Sync' @
function :: String -> Name -> Q Exp
function [] _ = error "Empty names are not allowed for exported functions."
function customName@(c:_) functionName
    | (not . isUpper) c = error $ "Custom function name must start with a capiatl letter: " <> show customName
    | otherwise = do
        (_, fun) <- functionImplementation functionName
        [|\funOpts -> EF (Function (F (fromString $(litE (StringL customName)))) funOpts, $(return fun)) |]


-- | Define an exported function. This function works exactly like 'function',
-- but it generates the exported name automatically by converting the first
-- letter to upper case.
function' :: Name -> Q Exp
function' functionName =
    let (c:cs) = nameBase functionName
    in function (toUpper c:cs) functionName


-- | Simply data type used to identify a string-ish type (e.g. 'String', 'Text',
-- 'ByteString' for a value of type.
data ArgType = StringyType
             | ListOfStringyTypes
             | Optional ArgType
             | CommandArgumentsType
             | OtherType
             deriving (Eq, Ord, Show, Read)


-- | Given a value of type 'Type', test whether it can be classified according
-- to the constructors of 'ArgType'.
classifyArgType :: Type -> Q ArgType
classifyArgType t = do
    set <- genStringTypesSet
    maybeType <- [t|Maybe|]
    cmdArgsType <- [t|CommandArguments|]
    case t of
        AppT ListT (ConT str) | str `Set.member` set
            -> return ListOfStringyTypes

        AppT m mt@(ConT _) | m == maybeType
            -> Optional <$> classifyArgType mt

        ConT str | str `Set.member` set
            -> return StringyType
        cmd | cmd == cmdArgsType
            -> return CommandArgumentsType

        _ -> return OtherType

  where
    genStringTypesSet = do
        types <- sequence [[t|String|],[t|ByteString|],[t|Text|]]
        return $ Set.fromList [ n | ConT n <- types ]


-- | Similarly to 'function', this function is used to export a command with a
-- custom name.
--
-- Note that commands must start with an upper case letter.
--
-- Due to limitations on the side of (neo)vim, commands can only have one of the
-- following five signatures, where you can replace 'String' with 'ByteString'
-- or 'Text' if you wish:
--
-- * 'CommandArguments' -> 'Neovim' r st ()
--
-- * 'CommandArguments' -> 'Maybe' 'String' -> 'Neovim' r st ()
--
-- * 'CommandArguments' -> 'String' -> 'Neovim' r st ()
--
-- * 'CommandArguments' -> ['String'] -> 'Neovim' r st ()
--
-- * 'CommandArguments' -> 'String' -> ['String'] -> 'Neovim' r st ()
--
-- Example: @ $(command \"RememberThePrime\" 'someFunction) ['CmdBang'] @
--
-- Note that the list of command options (i.e. the last argument) removes
-- duplicate options by means of some internally convenient sorting. You should
-- simply not define the same option twice.
command :: String -> Name -> Q Exp
command [] _ = error "Empty names are not allowed for exported commands."
command customFunctionName@(c:_) functionName
    | (not . isUpper) c = error $ "Custom command name must start with a capiatl letter: " <> show customFunctionName
    | otherwise = do
        (argTypes, fun) <- functionImplementation functionName
        -- See :help :command-nargs for what the result strings mean
        case argTypes of
            (CommandArgumentsType:_) -> return ()
            _ -> error "First argument for a function exported as a command must be CommandArguments!"
        let nargs = case tail argTypes of
                []                                -> [|CmdNargs "0"|]
                [StringyType]                     -> [|CmdNargs "1"|]
                [Optional StringyType]            -> [|CmdNargs "?"|]
                [ListOfStringyTypes]              -> [|CmdNargs "*"|]
                [StringyType, ListOfStringyTypes] -> [|CmdNargs "+"|]
                _                                 -> error $ unlines
                    [ "Trying to generate a command without compatible types."
                    , "Due to a limitation burdened on us by vimL, we can only"
                    , "use a limited amount type signatures for commands. See"
                    , "the documentation for 'command' for a more thorough"
                    , "explanation."
                    ]
        [|\copts -> EF (Command
                            (F (fromString $(litE (StringL customFunctionName))))
                            (mkCommandOptions ($(nargs) : copts))
                       , $(return fun))|]

-- | Define an exported command. This function works exactly like 'command', but
-- it generates the command name by converting the first letter to upper case.
command' :: Name -> Q Exp
command' functionName =
    let (c:cs) = nameBase functionName
    in command (toUpper c:cs) functionName


-- | This function generates an export for autocmd. Since this is a static
-- registration, arguments are not allowed here. You can, of course, define a
-- fully applied function and pass it as an argument. If you have to add
-- autocmds dynamically, it can be done with 'addAutocmd'.
--
-- Example:
--
-- @
-- someFunction :: a -> b -> c -> d -> Neovim r st res
-- someFunction = ...
--
-- theFunction :: Neovim r st res
-- theFunction = someFunction 1 2 3 4
--
-- $(autocmd 'theFunction) def
-- @
--
-- @def@ is of type 'AutocmdOptions'.
--
-- Note that you have to define @theFunction@ in a different module due to
-- the use of Template Haskell.
autocmd :: Name -> Q Exp
autocmd functionName =
    let (c:cs) = nameBase functionName
    in do
        (as, fun) <- functionImplementation functionName
        case as of
            [] ->
                [|\t acmdOpts -> EF (Autocmd t (F (fromString $(litE (StringL (toUpper c : cs))))) acmdOpts, $(return fun))|]

            _ ->
                error "Autocmd functions have to be fully applied (i.e. they should not take any arguments)."


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
functionImplementation :: Name -> Q ([ArgType], Exp)
functionImplementation functionName = do
    fInfo <- reify functionName
    nargs <- mapM classifyArgType $ case fInfo of
#if __GLASGOW_HASKELL__ < 800
            VarI _ functionType _ _ ->
#else
            VarI _ functionType _ ->
#endif
                determineNumberOfArguments functionType

            x ->
                error $ "Value given to function is (likely) not the name of a function.\n" <> show x

    e <- topLevelCase nargs
    return (nargs, e)

  where
    determineNumberOfArguments :: Type -> [Type]
    determineNumberOfArguments ft = case ft of
        ForallT _ _ t -> determineNumberOfArguments t
        AppT (AppT ArrowT t) r -> t : determineNumberOfArguments r
        _ -> []
    -- \args -> case args of ...
    topLevelCase :: [ArgType] -> Q Exp
    topLevelCase ts = do
        let n = length ts
            minLength = length [ () | Optional _ <- reverse ts ]
        args <- newName "args"
        lamE [varP args] (caseE (varE args)
            (zipWith matchingCase [n,n-1..] [0..minLength] ++ [errorCase]))

    -- _ -> err "Wrong number of arguments"
    errorCase :: Q Match
    errorCase = match wildP
        (normalB [|throw . ErrorMessage . text $ "Wrong number of arguments for function: "
                        ++ $(litE (StringL (nameBase functionName))) |]) []

    -- [x,y] -> case pure add <*> fromObject x <*> fromObject y of ...
    matchingCase :: Int -> Int -> Q Match
    matchingCase n x = do
        vars <- mapM (\_ -> Just <$> newName "x") [1..n]
        let optVars = replicate x (Nothing :: Maybe Name)
        match ((listP . map varP . catMaybes) vars)
              (normalB
                (caseE
                    (foldl genArgumentCast [|pure $(varE functionName)|]
                        (zip (vars ++ optVars) (repeat [|(<*>)|])))
                  [successfulEvaluation, failedEvaluation]))
              []

    genArgumentCast :: Q Exp -> (Maybe Name, Q Exp) -> Q Exp
    genArgumentCast e = \case
        (Just v,op) ->
            infixE (Just e) op (Just [|fromObject $(varE v)|])
        (Nothing, op) ->
            infixE (Just e) op (Just [|pure Nothing|])

    successfulEvaluation :: Q Match
    successfulEvaluation = newName "action" >>= \action ->
        match (conP (mkName "Right") [varP action])
              (normalB [|toObject <$> $(varE action)|])
              []
    failedEvaluation :: Q Match
    failedEvaluation = newName "e" >>= \e ->
        match (conP (mkName "Left") [varP e])
              (normalB [|err ($(varE e) :: Doc)|])
              []

