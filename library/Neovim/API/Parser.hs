{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Neovim.API.Parser
Description :  Parser for the msgpack output stram API
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.API.Parser
    ( NeovimAPI(..)
    , NeovimFunction(..)
    , NeovimType(..)
    , parseAPI
    ) where

import           Neovim.API.Classes

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad.Except
import qualified Data.ByteString          as B
import qualified Data.ByteString.UTF8     as U
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.MessagePack
import           Data.Monoid
import           Data.Serialize
import qualified Data.Text                as T
import           System.IO                (hClose)
import           System.Process
import           Text.Parsec              as P

data NeovimType = SimpleType String
                | NestedType NeovimType (Maybe Int)
                | Void
                deriving (Show, Eq)

-- | This data type contains simple information about a function as received
-- throudh the @nvim --api-info@ command.
data NeovimFunction
    = NeovimFunction
    { name       :: String
    -- ^ function name
    , parameters :: [(NeovimType, String)]
    -- ^ A list of type name and variable name.
    , canFail    :: Bool
    -- ^ Indicator whether the function can fail/throws exceptions.
    , deferred   :: Bool
    -- ^ Indicator whether the this function is asynchronous.
    , returnType :: NeovimType
    -- ^ Functions return type.
    }
    deriving (Show)

-- | This data type represents the top-level structure of the @nvim --api-info@
-- output.
data NeovimAPI
    = NeovimAPI
    { errorTypes  :: [(String, Int64)]
    -- ^ The error types are defined by a name and an identifier.
    , customTypes :: [(String, Int64)]
    -- ^ Extension types defined by neovim.
    , functions   :: [NeovimFunction]
    -- ^ The remotely executable functions provided by the neovim api.
    }
    deriving (Show)

-- | Run @nvim --api-info@ and parse its output.
parseAPI :: IO (Either String NeovimAPI)
parseAPI = join . fmap (runExcept . extractAPI) <$> runExceptT decodeAPI

extractAPI :: Object -> Except String NeovimAPI
extractAPI apiObj = NeovimAPI
    <$> extractErrorTypes apiObj
    <*> extractCustomTypes apiObj
    <*> extractFunctions apiObj

decodeAPI :: ExceptT String IO Object
decodeAPI = bracket queryNeovimAPI clean $ \(out, _) ->
    either throwError return =<< decode <$> lift (B.hGetContents out)

  where
    queryNeovimAPI = do
        (_, Just out, _, ph) <- lift . createProcess $
                (proc "nvim" ["--api-info"]) { std_out = CreatePipe }
        return (out, ph)

    clean (out, ph) = lift $ do
        hClose out
        terminateProcess ph

oMap :: Object -> Except String (Map Object Object)
oMap o = case o of
    ObjectMap m -> return m
    _           -> throwError $ "Object is not a map: " ++ show o

oLookup :: Object -> Object -> Except String Object
oLookup qry o = oMap o
    >>= maybe (throwError ("No entry for" <> show qry)) return
        . Map.lookup qry

oLookupDefault :: Object -> Object -> Object -> Except String Object
oLookupDefault d qry o = oMap o
    >>= maybe (return d) return . Map.lookup qry

-- | Extract a 'String' from on 'Object'.
--
-- Works on @ObjectBinary@ and @ObjectString@ constructor.
oToString :: Object -> Except String String
oToString o = case o of
    ObjectBinary bs -> return $ U.toString bs
    ObjectString t  -> return $ T.unpack t
    _ -> throwError $ show o <> " is not convertible to a String."

-- | Extract an 'Int64' from an @Object@.
--
oInt :: Object -> Except String Int64
oInt o = case o of
    ObjectInt    i  -> return i
    _           -> throwError $ show o <> " is not an Int64."

oArr :: Object -> Except String [Object]
oArr o = case o of
    ObjectArray os -> return os
    _              -> throwError $ show o <> " is not an Array."

oToBool :: Object -> Except String Bool
oToBool o = case o of
    ObjectBool b -> return b
    _            -> throwError $ show o <> " is not a boolean."

extractErrorTypes :: Object -> Except String [(String, Int64)]
extractErrorTypes objAPI =
    extractTypeNameAndID =<< oLookup (ObjectBinary "error_types") objAPI

extractTypeNameAndID :: Object -> Except String [(String, Int64)]
extractTypeNameAndID m = do
    types <- Map.toList <$> oMap m
    forM types $ \(errName, idMap) -> do
        n <- oToString errName
        i <- oInt =<< oLookup (ObjectBinary "id") idMap
        return (n,i)

extractCustomTypes :: Object -> Except String [(String, Int64)]
extractCustomTypes objAPI =
    extractTypeNameAndID =<< oLookup (ObjectBinary "types") objAPI

extractFunctions :: Object -> Except String [NeovimFunction]
extractFunctions objAPI = do
    funList <- oArr =<< oLookup (ObjectBinary "functions") objAPI
    forM funList extractFunction

toParameterlist :: [Object] -> Except String [(NeovimType, String)]
toParameterlist ps = forM ps $ \p -> do
    [t, n] <- mapM oToString =<< oArr p
    t' <- parseType t
    return (t', n)

extractFunction :: Object -> Except String NeovimFunction
extractFunction funDefMap = NeovimFunction
    <$> (oLookup (ObjectBinary "name") funDefMap >>= oToString)
    <*> (oLookup (ObjectBinary "parameters") funDefMap
            >>= oArr >>= toParameterlist)
    <*> (oLookupDefault (ObjectBool False) (ObjectBinary "can_fail") funDefMap
            >>= oToBool)
    <*> (oLookup (ObjectBinary "deferred") funDefMap >>= oToBool)
    <*> (oLookup (ObjectBinary "return_type") funDefMap
            >>= oToString >>= parseType)

parseType :: String -> Except String NeovimType
parseType s = either (throwError . show) return $ parse (pType <* eof) s s

pType :: Parsec String u NeovimType
pType = pArray P.<|> pVoid P.<|> pSimple

pVoid :: Parsec String u NeovimType
pVoid = const Void <$> (P.try (string "void") <* eof)

pSimple :: Parsec String u NeovimType
pSimple = SimpleType <$> many1 (noneOf ",)")

pArray :: Parsec String u NeovimType
pArray = NestedType <$> (P.try (string "ArrayOf(") *> pType)
                    <*> optionMaybe pNum <* char ')'

pNum :: Parsec String u Int
pNum = read <$> (P.try (char ',') *> spaces *> many1 (oneOf ['0'..'9']))

