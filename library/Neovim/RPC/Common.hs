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

import           Control.Monad
import Data.Monoid
import           Control.Monad.IO.Class
import           Data.Streaming.Network
import           Data.String
import           Network.Socket         as N hiding (SocketType)
import           System.Environment     (getEnv)
import           System.IO              (BufferMode (..), Handle, IOMode,
                                         hClose, hSetBuffering)
import           System.Log.Logger

-- | Simple data type defining the kind of socket the socket reader should use.
data SocketType = Stdout Handle
                -- ^ Use the handle for receiving msgpack-rpc messages. This is
                -- suitable for an embedded neovim which is used in test cases.
                | Environment
                -- ^ Read the connection information from the environment
                -- variable @NVIM_LISTEN_ADDRESS@.
                | UnixSocket FilePath
                -- ^ Use a unix socket.
                | TCP Int String
                -- ^ Use an IP socket. First argument is the port and the
                -- second is the host name.

-- | Create a 'Source' from the given socket description.
createHandle :: (Functor io, MonadIO io)
             => IOMode
             -> SocketType
             -> io Handle
createHandle ioMode socketType = case socketType of
    Stdout h -> do
        liftIO $ hSetBuffering h (BlockBuffering Nothing)
        return h
    UnixSocket f -> createHandle ioMode . Stdout
                    =<< createUnixSocketHandle f
    TCP p h -> createHandle ioMode . Stdout
                    =<< createTCPSocketHandle p h
    Environment -> createHandle ioMode . Stdout
                    =<< createSocketHandleFromEnvironment

  where
    createUnixSocketHandle :: (MonadIO io) => FilePath -> io Handle
    createUnixSocketHandle f =
        liftIO $ getSocketUnix f >>= flip socketToHandle ioMode

    createTCPSocketHandle :: (Functor io, MonadIO io) => Int -> String -> io Handle
    createTCPSocketHandle p h = liftIO $ getSocketTCP (fromString h) p
        >>= flip socketToHandle ioMode . fst

    createSocketHandleFromEnvironment = do
        listenAddress <- liftIO (getEnv "NVIM_LISTEN_ADDRESS")
        case words listenAddress of
            [unixSocket] -> createHandle ioMode (UnixSocket unixSocket)
            [h,p] -> createHandle ioMode (TCP (read p) h)
            _  -> do
                let errMsg = unlines
                        [ "Unhandled socket type from environment variable: "
                        , "\t" <> listenAddress
                        ]
                liftIO $ errorM "createHandle" errMsg
                error errMsg


cleanUpHandle :: (MonadIO io) => Handle -> Bool -> io ()
cleanUpHandle h completed = liftIO $ do
    hClose h
    unless completed $
        warningM "cleanUpHandle" "Cleanup called on uncompleted handle."
