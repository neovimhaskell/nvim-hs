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
    , parseAPI
    ) where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad.Except
import qualified Data.ByteString          as B
import qualified Data.ByteString.UTF8     as U
import           Data.Int                 (Int64)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.MessagePack
import           Data.Monoid
import           Data.Serialize
import qualified Data.Text                as T
import           System.IO                (hClose)
import           System.Process

data NeovimAPI = NeovimAPI
               { errorTypes :: [(String, Int64)]
               -- ^ The error types are defined by a name and an
               -- identifier. (as of 2014-11-22)
               }

parseAPI :: IO (Either String NeovimAPI)
parseAPI = join . fmap (runExcept . extractAPI) <$> runExceptT decodeAPI

extractAPI :: Object -> Except String NeovimAPI
extractAPI apiObj = NeovimAPI
    <$> extractErrorTypes apiObj

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
-- Only works on @ObjnectInt@ constructor.
oInt :: Object -> Except String Int64
oInt o = case o of
    ObjectInt i -> return i
    _           -> throwError $ show o <> " is not an Int64"

extractErrorTypes :: Object -> Except String [(String, Int64)]
extractErrorTypes objAPI = do
    errMap <- oLookup (ObjectBinary "error_types") objAPI
    errTypes <- Map.toList <$> oMap errMap
    forM errTypes $ \(name, idMap) -> do
        n <- oToString name
        i <- oInt =<< oLookup (ObjectBinary "id") idMap
        return (n,i)



