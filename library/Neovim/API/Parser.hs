{-# LANGUAGE LambdaCase        #-}
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

import           Neovim.Classes

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad.Except
import qualified Data.ByteString              as B
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.MessagePack
import           Data.Monoid
import           Data.Serialize
import           System.IO                    (hClose)
import           System.Process
import           Text.Parsec                  as P
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as P

import           Prelude


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

    , async      :: Bool
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
parseAPI :: IO (Either Doc NeovimAPI)
parseAPI = either (Left . P.text) extractAPI <$> decodeAPI

extractAPI :: Object -> Either Doc NeovimAPI
extractAPI apiObj = fromObject apiObj >>= \apiMap -> NeovimAPI
    <$> extractErrorTypes apiMap
    <*> extractCustomTypes apiMap
    <*> extractFunctions apiMap


decodeAPI :: IO (Either String Object)
decodeAPI = bracket queryNeovimAPI clean $ \(out, _) -> do
    decode <$> B.hGetContents out

  where
    queryNeovimAPI = do
        (_, Just out, _, ph) <- createProcess $
                (proc "nvim" ["--api-info"]) { std_out = CreatePipe }
        return (out, ph)

    clean (out, ph) = do
        hClose out
        terminateProcess ph


oLookup :: (NvimObject o) => String -> Map String Object -> Either Doc o
oLookup qry = maybe throwErrorMessage fromObject . Map.lookup qry
  where
    throwErrorMessage = throwError . P.text $ "No entry for: " <> show qry


oLookupDefault :: (NvimObject o) => o -> String -> Map String Object -> Either Doc o
oLookupDefault d qry m = maybe (return d) fromObject $ Map.lookup qry m


extractErrorTypes :: Map String Object -> Either Doc [(String, Int64)]
extractErrorTypes objAPI = extractTypeNameAndID =<< oLookup "error_types" objAPI


extractTypeNameAndID :: Object -> Either Doc [(String, Int64)]
extractTypeNameAndID m = do
    types <- Map.toList <$> fromObject m
    forM types $ \(errName, idMap) -> do
        i <- oLookup "id" idMap
        return (errName,i)


extractCustomTypes :: Map String Object -> Either Doc [(String, Int64)]
extractCustomTypes objAPI = extractTypeNameAndID =<< oLookup "types" objAPI


extractFunctions :: Map String Object -> Either Doc [NeovimFunction]
extractFunctions objAPI = mapM extractFunction =<< oLookup "functions" objAPI


toParameterlist :: [(String, String)] -> Either Doc [(NeovimType, String)]
toParameterlist ps = forM ps $ \(t,n) -> do
    t' <- parseType t
    return (t', n)


extractFunction :: Map String Object -> Either Doc NeovimFunction
extractFunction funDefMap = NeovimFunction
    <$> (oLookup "name" funDefMap)
    <*> (oLookup "parameters" funDefMap >>= toParameterlist)
    <*> (oLookupDefault False "can_fail" funDefMap)
    <*> (oLookup "async" funDefMap)
    <*> (oLookup "return_type" funDefMap >>= parseType)


parseType :: String -> Either Doc NeovimType
parseType s = either (throwError . P.text . show) return $ parse (pType <* eof) s s


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

