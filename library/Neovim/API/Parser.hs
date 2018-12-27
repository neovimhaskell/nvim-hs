{- |
Module      :  Neovim.API.Parser
Description :  P.Parser for the msgpack output stram API
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
import           Control.Monad.Except
import qualified Data.ByteString                           as B
import qualified Data.ByteString.Lazy                      as LB
import           Data.Map                                  (Map)
import qualified Data.Map                                  as Map
import           Data.MessagePack
import           Data.Serialize
import           Neovim.Compat.Megaparsec                  as P
import           System.IO                                 (hClose)
import           System.Process.Typed
import           UnliftIO.Exception                        (SomeException,
                                                            bracket, catch)

import           Data.Text.Prettyprint.Doc                 (Doc, Pretty(..), (<+>))
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

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
parseAPI :: IO (Either (Doc AnsiStyle) NeovimAPI)
parseAPI = either (Left . pretty) extractAPI <$> (decodeAPI `catch` readFromAPIFile)

extractAPI :: Object -> Either (Doc AnsiStyle) NeovimAPI
extractAPI apiObj = fromObject apiObj >>= \apiMap -> NeovimAPI
    <$> extractErrorTypes apiMap
    <*> extractCustomTypes apiMap
    <*> extractFunctions apiMap

readFromAPIFile :: SomeException -> IO (Either String Object)
readFromAPIFile _ = (decode <$> B.readFile "api") `catch` returnPreviousExceptionAsText
  where
      returnPreviousExceptionAsText :: SomeException -> IO (Either String Object)
      returnPreviousExceptionAsText _ = return . Left $
        "The 'nvim' process could not be started and there is no file named\
        \ 'api' in the working directory as a substitute."

decodeAPI :: IO (Either String Object)
decodeAPI =
   decode . LB.toStrict <$> readProcessStdout_ (proc "nvim" ["--api-info"])


oLookup :: (NvimObject o) => String -> Map String Object -> Either (Doc AnsiStyle) o
oLookup qry = maybe throwErrorMessage fromObject . Map.lookup qry
  where
      throwErrorMessage = throwError $ "No entry for:" <+> pretty qry


oLookupDefault :: (NvimObject o) => o -> String -> Map String Object -> Either (Doc AnsiStyle) o
oLookupDefault d qry m = maybe (return d) fromObject $ Map.lookup qry m


extractErrorTypes :: Map String Object -> Either (Doc AnsiStyle) [(String, Int64)]
extractErrorTypes objAPI = extractTypeNameAndID =<< oLookup "error_types" objAPI


extractTypeNameAndID :: Object -> Either (Doc AnsiStyle) [(String, Int64)]
extractTypeNameAndID m = do
    types <- Map.toList <$> fromObject m
    forM types $ \(errName, idMap) -> do
        i <- oLookup "id" idMap
        return (errName,i)


extractCustomTypes :: Map String Object -> Either (Doc AnsiStyle) [(String, Int64)]
extractCustomTypes objAPI = extractTypeNameAndID =<< oLookup "types" objAPI


extractFunctions :: Map String Object -> Either (Doc AnsiStyle) [NeovimFunction]
extractFunctions objAPI = mapM extractFunction =<< oLookup "functions" objAPI


toParameterlist :: [(String, String)] -> Either (Doc AnsiStyle) [(NeovimType, String)]
toParameterlist ps = forM ps $ \(t,n) -> do
    t' <- parseType t
    return (t', n)


extractFunction :: Map String Object -> Either (Doc AnsiStyle) NeovimFunction
extractFunction funDefMap = NeovimFunction
    <$> (oLookup "name" funDefMap)
    <*> (oLookup "parameters" funDefMap >>= toParameterlist)
    <*> (oLookupDefault True "can_fail" funDefMap)
    <*> (oLookupDefault False "async" funDefMap)
    <*> (oLookup "return_type" funDefMap >>= parseType)


parseType :: String -> Either (Doc AnsiStyle) NeovimType
parseType s = either (throwError . pretty . show) return $ parse (pType <* eof) s s


pType :: P.Parser NeovimType
pType = pArray P.<|> pVoid P.<|> pSimple


pVoid :: P.Parser NeovimType
pVoid = const Void <$> (P.try (string "void") <* eof)


pSimple :: P.Parser NeovimType
pSimple = SimpleType <$> P.some (noneOf [',', ')'])


pArray :: P.Parser NeovimType
pArray = NestedType <$> (P.try (string "ArrayOf(") *> pType)
                    <*> optional pNum <* char ')'


pNum :: P.Parser Int
pNum = read <$> (P.try (char ',') *> space *> P.some (oneOf ['0'..'9']))

