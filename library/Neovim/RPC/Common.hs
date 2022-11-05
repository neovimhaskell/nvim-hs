{-# LANGUAGE RankNTypes, CPP        #-}
{- |
Module      :  Neovim.RPC.Common
Description :  Common functons for the RPC module
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Neovim.RPC.Common
    where

import           Neovim.Context

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Int               (Int64)
import           Data.Map
import           Data.MessagePack
import           Data.Monoid
import           Data.Streaming.Network
import           Data.String
import           Data.Time
import           Network.Socket         as N hiding (SocketType)
import           System.IO              (BufferMode (..), Handle, IOMode(ReadWriteMode),
                                         hClose, hSetBuffering)
import           System.Log.Logger
import           Neovim.Compat.Megaparsec as P

import           Prelude
import UnliftIO.Environment (lookupEnv)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import qualified Data.List as List
import qualified Text.Megaparsec.Char.Lexer as L


-- | Things shared between the socket reader and the event handler.
data RPCConfig = RPCConfig
    { recipients :: TVar (Map Int64 (UTCTime, TMVar (Either Object Object)))
    -- ^ A map from message identifiers (as per RPC spec) to a tuple with a
    -- timestamp and a 'TMVar' that is used to communicate the result back to
    -- the calling thread.

    , nextMessageId :: TVar Int64
    -- ^ Message identifier for the next message as per RPC spec.
    }

-- | Create a new basic configuration containing a communication channel for
-- remote procedure call events and an empty lookup table for functions to
-- mediate.
newRPCConfig :: (Applicative io, MonadIO io) => io RPCConfig
newRPCConfig = RPCConfig
    <$> liftIO (newTVarIO mempty)
    <*> liftIO (newTVarIO 1)

-- | Simple data type defining the kind of socket the socket reader should use.
data SocketType = Stdout Handle
                -- ^ Use the handle for receiving msgpack-rpc messages. This is
                -- suitable for an embedded neovim which is used in test cases.
                | Environment
                -- ^ Read the connection information from the environment
                -- variable @NVIM@.
                | UnixSocket FilePath
                -- ^ Use a unix socket.
                | TCP Int String
                -- ^ Use an IP socket. First argument is the port and the
                -- second is the host name.

-- | Create a 'Handle' from the given socket description.
--
-- The handle is not automatically closed.
createHandle :: (Functor io, MonadIO io)
             => SocketType
             -> io Handle
createHandle = \case
    Stdout h -> do
        liftIO $ hSetBuffering h (BlockBuffering Nothing)
        return h

    UnixSocket f ->
#ifndef WINDOWS
        liftIO $ createHandle . Stdout =<< flip socketToHandle ReadWriteMode =<< getSocketUnix f
#else
        openFile f ReadWriteMode
#endif

    TCP p h ->
        createHandle . Stdout =<< createTCPSocketHandle p h

    Environment ->
        createHandle . Stdout =<< createSocketHandleFromEnvironment

  where
    createTCPSocketHandle :: (MonadIO io) => Int -> String -> io Handle
    createTCPSocketHandle p h = liftIO $ getSocketTCP (fromString h) p
        >>= flip socketToHandle ReadWriteMode . fst

    createSocketHandleFromEnvironment = liftIO $ do
        -- NVIM_LISTEN_ADDRESS is for backwards compatibility
        envValues <- catMaybes <$> mapM lookupEnv ["NVIM", "NVIM_LISTEN_ADDRESS"]
        listenAdresses <- mapM parseNvimEnvironmentVariable envValues
        case listenAdresses of
            (s:_) -> createHandle s
            _  -> do
                let errMsg = unlines
                        [ "Unhandled socket type from environment variable: "
                        , "\t" <> intercalate ", " envValues
                        ]
                liftIO $ errorM "createHandle" errMsg
                error errMsg

parseNvimEnvironmentVariable :: MonadFail m => String -> m SocketType
parseNvimEnvironmentVariable envValue =
  either (fail . show) pure $ parse (P.try pTcpAddress <|> pUnixSocket) envValue envValue

pUnixSocket :: P.Parser SocketType
pUnixSocket = UnixSocket <$> P.some anySingle <* P.eof

pTcpAddress :: P.Parser SocketType
pTcpAddress = do
   prefixes <- P.some (P.try (P.many (P.anySingleBut ':') <* P.single ':'))
   port <- L.lexeme P.eof L.decimal
   P.eof
   pure $ TCP port (List.intercalate ":" prefixes)

  -- fail $ "Unsupported format for $NVIM environment variable: " <> envValue

-- | Close the handle and print a warning if the conduit chain has been
-- interrupted prematurely.
cleanUpHandle :: (MonadIO io) => Handle -> Bool -> io ()
cleanUpHandle h completed = liftIO $ do
    hClose h
    unless completed $
        warningM "cleanUpHandle" "Cleanup called on uncompleted handle."
