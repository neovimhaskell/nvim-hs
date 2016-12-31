{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE DeriveGeneric      #-}
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

import           Neovim.Classes
import           Neovim.Plugin.Classes        (FunctionName (..))
import qualified Neovim.Plugin.IPC.Classes    as IPC

import           Control.Applicative
import           Control.Monad.Error.Class
import           Data.Data                    (Typeable)
import           Data.Int                     (Int64)
import           Data.MessagePack             (Object (..))
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import           Prelude

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
    deriving (Eq, Ord, Show, Typeable, Generic)


instance NFData Message


instance IPC.Message Message


instance NvimObject Message where
    toObject = \case
        Request (IPC.Request (F m) i ps) ->
            ObjectArray $  (0 :: Int64) +: i +: m +: ps +: []

        Response i (Left e) ->
            ObjectArray $ (1 :: Int64) +: i +: e +: () +: []

        Response i (Right r) ->
            ObjectArray $ (1 :: Int64) +: i +: () +: r +: []

        Notification (IPC.Notification (F m) ps) ->
            ObjectArray $ (2 :: Int64) +: m +: ps +: []


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
            throwError . text $ "Not a known/valid msgpack-rpc message" ++ show o


instance Pretty Message where
    pretty = \case
        Request request ->
            pretty request

        Response i ret ->
            nest 2 $ text "Response" <+> (yellow . text . ('#':) . show) i
                <$$> either (red . text . show) (green . text. show) ret

        Notification notification ->
            pretty notification


