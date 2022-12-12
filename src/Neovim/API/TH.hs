{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Neovim.API.TH
Description :  Template Haskell API generation module
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
-}
module Neovim.API.TH (
    generateAPI,
    function,
    function',
    command,
    command',
    autocmd,
    stringListTypeMap,
    textVectorTypeMap,
    bytestringVectorTypeMap,
    createFunction,
    module UnliftIO.Exception,
    module Neovim.Classes,
    module Data.Data,
    module Data.MessagePack,
) where

import Neovim.API.Parser
import Neovim.Classes
import Neovim.Context
import Neovim.Plugin.Classes (
    CommandArguments (..),
    CommandOption (..),
    FunctionName (..),
    FunctionalityDescription (..),
    mkCommandOptions,
 )
import Neovim.Plugin.Internal (ExportedFunctionality (..))
import Neovim.RPC.FunctionCall

import Language.Haskell.TH hiding (conP, dataD, instanceD)
import TemplateHaskell.Compat.V0208

import Control.Applicative
import Control.Arrow (first)
import Control.Concurrent.STM (STM)
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (isUpper, toUpper)
import Data.Data (Data, Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.MessagePack
import Data.Monoid
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector (Vector)
import Prettyprinter (viaShow)
import UnliftIO.Exception

import qualified Data.Text as T
import Prelude

{- | Generate the API types and functions provided by @nvim --api-info@.

 The provided map allows the use of different Haskell types for the types
 defined in the API. The types must be an instance of 'NvimObject' and they
 must form an isomorphism with the sent messages types. Currently, it
 provides a Convenient way to replace the /String/ type with 'Text',
 'ByteString' or 'String'.
-}
generateAPI :: TypeMap -> Q [Dec]
generateAPI typeMap = do
    api <- either (fail . show) return =<< runIO parseAPI
    let exceptionName = mkName "NeovimExceptionGen"
        exceptions = (\(n, i) -> (mkName ("Neovim" <> n), i)) `map` errorTypes api
        customTypesN = first mkName `map` customTypes api
    join
        <$> sequence
            [ join . pure <$> createDataTypeWithByteStringComponent exceptionName (map fst exceptions)
            , exceptionInstance exceptionName
            , customTypeInstance exceptionName exceptions
            , fmap join (mapM ((\n -> createDataTypeWithByteStringComponent n [n]) . fst) customTypesN)
            , join <$> mapM (\(n, i) -> customTypeInstance n [(n, i)]) customTypesN
            , fmap join . mapM (createFunction typeMap) $ functions api
            ]

-- | Maps type identifiers from the neovim API to Haskell types.
data TypeMap = TypeMap
    { typesOfAPI :: Map String (Q Type)
    , list :: Q Type
    }

stringListTypeMap :: TypeMap
stringListTypeMap =
    TypeMap
        { typesOfAPI =
            Map.fromList
                [ ("Boolean", [t|Bool|])
                , ("Integer", [t|Int64|])
                , ("LuaRef", [t|Int64|])
                , ("Float", [t|Double|])
                , ("String", [t|String|])
                , ("Array", [t|[Object]|])
                , ("Dictionary", [t|Map String Object|])
                , ("void", [t|()|])
                ]
        , list = listT
        }

textVectorTypeMap :: TypeMap
textVectorTypeMap =
    stringListTypeMap
        { typesOfAPI = adjustTypeMapForText $ typesOfAPI stringListTypeMap
        , list = [t|Vector|]
        }
  where
    adjustTypeMapForText =
        Map.insert "String" [t|Text|]
            . Map.insert "Array" [t|Vector Object|]
            . Map.insert "Dictionary" [t|Map Text Object|]

bytestringVectorTypeMap :: TypeMap
bytestringVectorTypeMap =
    textVectorTypeMap
        { typesOfAPI = adjustTypeMapForByteString $ typesOfAPI textVectorTypeMap
        }
  where
    adjustTypeMapForByteString =
        Map.insert "String" [t|ByteString|]
            . Map.insert "Array" [t|Vector Object|]
            . Map.insert "Dictionary" [t|Map ByteString Object|]

apiTypeToHaskellType :: TypeMap -> NeovimType -> Q Type
apiTypeToHaskellType typeMap@TypeMap{typesOfAPI, list} at = case at of
    Void -> [t|()|]
    NestedType t Nothing ->
        appT list $ apiTypeToHaskellType typeMap t
    NestedType t (Just n) ->
        foldl appT (tupleT n) . replicate n $ apiTypeToHaskellType typeMap t
    SimpleType t ->
        fromMaybe ((conT . mkName) t) $ Map.lookup t typesOfAPI

{- | This function will create a wrapper function with neovim's function name
 as its name.

 Synchronous function:
 @
 buffer_get_number :: Buffer -> Neovim Int64
 buffer_get_number buffer = scall "buffer_get_number" [toObject buffer]
 @

 Asynchronous function:
 @
 vim_eval :: String -> Neovim (TMVar Object)
 vim_eval str = acall "vim_eval" [toObject str]
 @

 Asynchronous function without a return value:
 @
 vim_feed_keys :: String -> String -> Bool -> Neovim ()
 vim_feed_keys keys mode escape_csi =
     acallVoid "vim_feed_keys" [ toObject keys
                               , toObject mode
                               , toObject escape_csi
                               ]
 @
-}
createFunction :: TypeMap -> NeovimFunction -> Q [Dec]
createFunction typeMap nf = do
    let withDeferred
            | async nf = appT [t|STM|] . appT [t|Either NeovimException|]
            | otherwise = id

        callFn
            | async nf = [|acall|]
            | otherwise = [|scall'|]

        functionName = mkName $ name nf
        toObjVar v = [|toObject $(varE v)|]

    retType <-
        let env = mkName "env"
         in forallT [specifiedPlainTV env] (return [])
                . appT [t|Neovim $(varT env)|]
                . withDeferred
                . apiTypeToHaskellType typeMap
                $ returnType nf

    -- prefix with arg0_, arg1_ etc. to prevent generated code from crashing due
    -- to keywords being used.
    -- see https://github.com/neovimhaskell/nvim-hs/issues/65
    let prefixWithNumber i n = "arg" ++ show i ++ "_" ++ n
        applyPrefixWithNumber =
            zipWith
                (\i (t, n) -> (t, prefixWithNumber i n))
                [0 :: Int ..]
                . parameters
    vars <-
        mapM
            ( \(t, n) ->
                (,)
                    <$> apiTypeToHaskellType typeMap t
                    <*> newName n
            )
            $ applyPrefixWithNumber nf

    sequence
        [ (sigD functionName . return) (foldr ((AppT . AppT ArrowT) . fst) retType vars)
        , funD
            functionName
            [ clause
                (map (varP . snd) vars)
                ( normalB
                    ( callFn
                        `appE` ([|(F . T.pack)|] `appE` (litE . stringL . name) nf)
                        `appE` listE (map (toObjVar . snd) vars)
                    )
                )
                []
            ]
        ]

{- | @ createDataTypeWithObjectComponent SomeName [Foo,Bar]@
 will create this:
 @
 data SomeName = Foo !Object
               | Bar !Object
               deriving (Typeable, Eq, Show)
 @
-}
createDataTypeWithByteStringComponent :: Name -> [Name] -> Q [Dec]
createDataTypeWithByteStringComponent nme cs = do
    tObject <- [t|ByteString|]

    let strictNess = (Bang NoSourceUnpackedness SourceStrict, tObject)

    pure
        [ dataD
            []
            nme
            []
            (map (\n -> NormalC n [strictNess]) cs)
            (mkName <$> ["Typeable", "Eq", "Show", "Generic"])
        , instanceD [] (AppT (ConT (mkName "NFData")) (ConT nme)) []
        ]

{- | If the first parameter is @mkName NeovimException@, this function will
 generate  @instance Exception NeovimException@.
-}
exceptionInstance :: Name -> Q [Dec]
exceptionInstance exceptionName = do
    tException <- [t|Exception|]
    pure [instanceD [] (tException `AppT` ConT exceptionName) []]

{- | @customTypeInstance Foo [(Bar, 1), (Quz, 2)]@
 will create this:
 @
 instance Serializable Foo where
     toObject (Bar bs) = ObjectExt 1 bs
     toObject (Quz bs) = ObjectExt 2 bs
     fromObject (ObjectExt 1 bs) = return $ Bar bs
     fromObject (ObjectExt 2 bs) = return $ Quz bs
     fromObject o = Left $ "Object is not convertible to: Foo Received: " <> show o
 @
-}
customTypeInstance :: Name -> [(Name, Int64)] -> Q [Dec]
customTypeInstance typeName nis = do
    let fromObjectClause :: Name -> Int64 -> Q Clause
        fromObjectClause n i = do
            bs <- newName "bs"
            let objectExtMatch =
                    conP
                        (mkName "ObjectExt")
                        [(LitP . integerL . fromIntegral) i, VarP bs]
            clause
                [pure objectExtMatch]
                (normalB [|return $ $(conE n) $(varE bs)|])
                []
        fromObjectErrorClause :: Q Clause
        fromObjectErrorClause = do
            o <- newName "o"
            let n = nameBase typeName
            clause
                [varP o]
                ( normalB
                    [|
                        throwError $
                            pretty "Object is not convertible to:"
                                <+> viaShow n
                                <+> pretty "Received:"
                                <+> viaShow $(varE o)
                        |]
                )
                []

        toObjectClause :: Name -> Int64 -> Q Clause
        toObjectClause n i = do
            bs <- newName "bs"
            clause
                [pure (conP n [VarP bs])]
                (normalB [|ObjectExt $((litE . integerL . fromIntegral) i) $(varE bs)|])
                []

    tNvimObject <- [t|NvimObject|]
    fToObject <- funD (mkName "toObject") $ map (uncurry toObjectClause) nis
    fFromObject <- funD (mkName "fromObject") $ map (uncurry fromObjectClause) nis <> [fromObjectErrorClause]
    pure [instanceD [] (tNvimObject `AppT` ConT typeName) [fToObject, fFromObject]]

{- | Define an exported function by providing a custom name and referencing the
 function you want to export.

 Note that the name must start with an upper case letter.

 Example: @ $(function \"MyExportedFunction\" 'myDefinedFunction) 'Sync' @
-}
function :: String -> Name -> Q Exp
function [] _ = fail "Empty names are not allowed for exported functions."
function customName@(c : _) functionName
    | (not . isUpper) c = error $ "Custom function name must start with a capital letter: " <> show customName
    | otherwise = do
        (_, fun) <- functionImplementation functionName
        [|\funOpts -> EF (Function (F (T.pack $(litE (StringL customName)))) funOpts, $(return fun))|]

uppercaseFirstCharacter :: Name -> String
uppercaseFirstCharacter name = case nameBase name of
    "" -> ""
    (c : cs) -> toUpper c : cs

{- | Define an exported function. This function works exactly like 'function',
 but it generates the exported name automatically by converting the first
 letter to upper case.
-}
function' :: Name -> Q Exp
function' functionName = function (uppercaseFirstCharacter functionName) functionName

{- | Simply data type used to identify a string-ish type (e.g. 'String', 'Text',
 'ByteString' for a value of type.
-}
data ArgType
    = StringyType
    | ListOfStringyTypes
    | Optional ArgType
    | CommandArgumentsType
    | OtherType
    deriving (Eq, Ord, Show, Read)

{- | Given a value of type 'Type', test whether it can be classified according
 to the constructors of "ArgType".
-}
classifyArgType :: Type -> Q ArgType
classifyArgType t = do
    set <- genStringTypesSet
    maybeType <- [t|Maybe|]
    cmdArgsType <- [t|CommandArguments|]
    case t of
        AppT ListT (ConT str)
            | str `Set.member` set ->
                return ListOfStringyTypes
        AppT m mt@(ConT _)
            | m == maybeType ->
                Optional <$> classifyArgType mt
        ConT str
            | str `Set.member` set ->
                return StringyType
        cmd
            | cmd == cmdArgsType ->
                return CommandArgumentsType
        _ -> return OtherType
  where
    genStringTypesSet = do
        types <- sequence [[t|String|], [t|ByteString|], [t|Text|]]
        return $ Set.fromList [n | ConT n <- types]

{- | Similarly to 'function', this function is used to export a command with a
 custom name.

 Note that commands must start with an upper case letter.

 Due to limitations on the side of (neo)vim, commands can only have one of the
 following five signatures, where you can replace 'String' with 'ByteString'
 or 'Text' if you wish:

 * 'CommandArguments' -> 'Neovim' env ()

 * 'CommandArguments' -> 'Maybe' 'String' -> 'Neovim' env ()

 * 'CommandArguments' -> 'String' -> 'Neovim' env ()

 * 'CommandArguments' -> ['String'] -> 'Neovim' env ()

 * 'CommandArguments' -> 'String' -> ['String'] -> 'Neovim' env ()

 Example: @ $(command \"RememberThePrime\" 'someFunction) ['CmdBang'] @

 Note that the list of command options (i.e. the last argument) removes
 duplicate options by means of some internally convenient sorting. You should
 simply not define the same option twice.
-}
command :: String -> Name -> Q Exp
command [] _ = error "Empty names are not allowed for exported commands."
command customFunctionName@(c : _) functionName
    | (not . isUpper) c = error $ "Custom command name must start with a capital letter: " <> show customFunctionName
    | otherwise = do
        (argTypes, fun) <- functionImplementation functionName
        -- See :help :command-nargs for what the result strings mean
        case argTypes of
            (CommandArgumentsType : _) -> return ()
            _ -> error "First argument for a function exported as a command must be CommandArguments!"
        let nargs = case tail argTypes of
                [] -> [|CmdNargs "0"|]
                [StringyType] -> [|CmdNargs "1"|]
                [Optional StringyType] -> [|CmdNargs "?"|]
                [ListOfStringyTypes] -> [|CmdNargs "*"|]
                [StringyType, ListOfStringyTypes] -> [|CmdNargs "+"|]
                _ ->
                    error $
                        unlines
                            [ "Trying to generate a command without compatible types."
                            , "Due to a limitation burdened on us by vimL, we can only"
                            , "use a limited amount type signatures for commands. See"
                            , "the documentation for 'command' for a more thorough"
                            , "explanation."
                            ]
        [|
            \copts ->
                EF
                    ( Command
                        (F (T.pack $(litE (StringL customFunctionName))))
                        (mkCommandOptions ($(nargs) : copts))
                    , $(return fun)
                    )
            |]

{- | Define an exported command. This function works exactly like 'command', but
 it generates the command name by converting the first letter to upper case.
-}
command' :: Name -> Q Exp
command' functionName = command (uppercaseFirstCharacter functionName) functionName

{- | This function generates an export for autocmd. Since this is a static
 registration, arguments are not allowed here. You can, of course, define a
 fully applied function and pass it as an argument. If you have to add
 autocmds dynamically, it can be done with 'addAutocmd'.

 Example:

 @
 someFunction :: a -> b -> c -> d -> Neovim r st res
 someFunction = ...

 theFunction :: Neovim r st res
 theFunction = someFunction 1 2 3 4
-}

{- $(autocmd 'theFunction) def
 @

 @def@ is of type 'AutocmdOptions'.

 Note that you have to define @theFunction@ in a different module due to
 the use of Template Haskell.
-}

autocmd :: Name -> Q Exp
autocmd functionName = do
    (as, fun) <- functionImplementation functionName
    case as of
        [] ->
            [|\t sync acmdOpts -> EF (Autocmd t (F (T.pack $(litE (StringL (uppercaseFirstCharacter functionName))))) sync acmdOpts, $(return fun))|]
        _ ->
            error "Autocmd functions have to be fully applied (i.e. they should not take any arguments)."

{- | Generate a function of type @[Object] -> Neovim' Object@ from the argument
 function.

 The function
 @
 add :: Int -> Int -> Int
 add = (+)
 @
 will be converted to
 @
 \args -> case args of
     [x,y] -> case pure add <*> fromObject x <*> fromObject y of
         Left e -> err $ "Wrong type of arguments for add: " ++ e
         Right action -> toObject <$> action
     _ -> err $ "Wrong number of arguments for add: " ++ show xs
 @
-}
functionImplementation :: Name -> Q ([ArgType], Exp)
functionImplementation functionName = do
    fInfo <- reify functionName
    nargs <- mapM classifyArgType $ case fInfo of
        VarI _ functionType _ ->
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
            minLength = length [() | Optional _ <- reverse ts]
        args <- newName "args"
        lamE
            [varP args]
            ( caseE
                (varE args)
                (zipWith matchingCase [n, n - 1 ..] [0 .. minLength] ++ [errorCase])
            )

    -- _ -> err "Wrong number of arguments"
    errorCase :: Q Match
    errorCase =
        match
            wildP
            ( normalB
                [|
                    throw . ErrorMessage . pretty $
                        "Wrong number of arguments for function: "
                            ++ $(litE (StringL (nameBase functionName)))
                    |]
            )
            []

    -- [x,y] -> case pure add <*> fromObject x <*> fromObject y of ...
    matchingCase :: Int -> Int -> Q Match
    matchingCase n x = do
        vars <- mapM (\_ -> Just <$> newName "x") [1 .. n]
        let optVars = replicate x (Nothing :: Maybe Name)
        match
            ((listP . map varP . catMaybes) vars)
            ( normalB
                ( caseE
                    ( foldl
                        genArgumentCast
                        [|pure $(varE functionName)|]
                        (zip (vars ++ optVars) (repeat [|(<*>)|]))
                    )
                    [successfulEvaluation, failedEvaluation]
                )
            )
            []

    genArgumentCast :: Q Exp -> (Maybe Name, Q Exp) -> Q Exp
    genArgumentCast e = \case
        (Just v, op) ->
            infixE (Just e) op (Just [|fromObject $(varE v)|])
        (Nothing, op) ->
            infixE (Just e) op (Just [|pure Nothing|])

    successfulEvaluation :: Q Match
    successfulEvaluation =
        newName "action" >>= \action ->
            match
                (pure (conP (mkName "Right") [VarP action]))
                (normalB [|toObject <$> $(varE action)|])
                []
    failedEvaluation :: Q Match
    failedEvaluation =
        newName "e" >>= \e ->
            match
                (pure (conP (mkName "Left") [VarP e]))
                (normalB [|err ($(varE e) :: Doc AnsiStyle)|])
                []
