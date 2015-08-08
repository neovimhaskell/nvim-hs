{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{- |
Module      :  Neovim.RPC.Classes
Description :  Data types and classes for the RPC components
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

Import this module qualified as @MsgpackRPC@
-}
module Neovim.RPC.Classes
    ( Message (..),
    ) where

import           Neovim.Classes            (NvimObject (..))
import           Neovim.Plugin.Classes     (FunctionName (..))
import qualified Neovim.Plugin.IPC.Classes as IPC

import           Control.Monad.Error.Class
import           Data.Data                 (Typeable)
import           Data.Int                  (Int64)
import           Data.MessagePack          (Object (..))


-- | See https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md for
-- details about the msgpack rpc specification.
data Message
    = Request IPC.Request
    -- ^ Request in the sense of the msgpack rpc specification
    --
    -- Parameters
    -- * Message identifier that has to be put in the response to this request
    -- * Function name
    -- * Function arguments

    | Response !Int64 (Either Object Object)
    -- ^ Response in the sense of the msgpack rpc specifcation
    --
    -- Parameters
    -- * Mesage identifier which matches a request
    -- * 'Either' an error 'Object' or a result 'Object'

    | Notification IPC.Notification
    -- ^ Notification in the sense of the msgpack rpc specification
    deriving (Eq, Ord, Show, Typeable)


instance IPC.Message Message

instance NvimObject Message where
    toObject = \case
        Request (IPC.Request (F m) i ps) ->
            ObjectArray [ ObjectInt 0, ObjectInt i, ObjectBinary m, ObjectArray ps ]

        Response i (Left e) ->
            ObjectArray [ ObjectInt 1, ObjectInt i, e, ObjectNil]

        Response i (Right r) ->
            ObjectArray [ ObjectInt 1, ObjectInt i, ObjectNil, r]

        Notification (IPC.Notification (F m) ps) ->
            ObjectArray [ ObjectInt 2, ObjectBinary m, ObjectArray ps ]


    fromObject = \case
        ObjectArray [ObjectInt 0, i, m, ps] -> do
            r <- IPC.Request
                    <$> (fmap F (fromObject m))
                    <*> fromObject i
                    <*> fromObject ps
            return $ Request r

        ObjectArray [ObjectInt 1, i, e, r] ->
            let eer = case e of
                        ObjectNil -> Right r
                        _         -> Left e
            in Response <$> fromObject i
                        <*> pure eer

        ObjectArray [ObjectInt 2, m, ps] -> do
            n <- IPC.Notification
                    <$> (fmap F (fromObject m))
                    <*> fromObject ps
            return $ Notification n

        o ->
            throwError $ "Not a known/valid msgpack-rpc message" ++ show o



