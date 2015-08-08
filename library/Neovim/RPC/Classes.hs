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

-}
module Neovim.RPC.Classes
    ( MsgpackRPCMessage (..),
    ) where

import           Neovim.Classes            (NvimObject (..))
import           Neovim.Plugin.Classes     (FunctionName (..))
import           Neovim.Plugin.IPC.Classes

import           Control.Monad.Error.Class
import           Data.Data                 (Typeable)
import           Data.Int                  (Int64)
import           Data.MessagePack          (Object (..))


-- | See https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md for
-- details about the msgpack rpc specification.
data MsgpackRPCMessage
    = Request !Int64 !FunctionName [Object]
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

    | Notification !FunctionName [Object]
    -- ^ Notification in the sense of the msgpack rpc specification
    --
    -- Parameters
    -- * Function name
    -- * Function arguments
    deriving (Eq, Ord, Show, Typeable)


instance Message MsgpackRPCMessage

instance NvimObject MsgpackRPCMessage where
    toObject = \case
        Request i (F m) ps ->
            ObjectArray [ ObjectInt 0, ObjectInt i, ObjectBinary m, ObjectArray ps ]

        Response i (Left e) ->
            ObjectArray [ ObjectInt 1, ObjectInt i, e, ObjectNil]

        Response i (Right r) ->
            ObjectArray [ ObjectInt 1, ObjectInt i, ObjectNil, r]

        Notification (F m) ps ->
            ObjectArray [ ObjectInt 2, ObjectBinary m, ObjectArray ps ]


    fromObject = \case
        ObjectArray [ObjectInt 0, i, m, ps] ->
            Request <$> fromObject i
                    <*> (fmap F (fromObject m))
                    <*> fromObject ps

        ObjectArray [ObjectInt 1, i, e, r] ->
            let eer = case e of
                        ObjectNil -> Right r
                        _         -> Left e
            in Response <$> fromObject i
                        <*> pure eer

        ObjectArray [ObjectInt 2, m, ps] ->
            Notification <$> (fmap F (fromObject m))
                         <*> fromObject ps

        o ->
            throwError $ "Not a known/valid msgpack-rpc message" ++ show o



