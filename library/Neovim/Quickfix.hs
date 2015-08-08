{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{- |
Module      :  Neovim.Quickfix
Description :  API for interacting with the quickfix list
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Quickfix
    where

import           Control.Applicative
import           Control.Monad           (void)
import           Data.ByteString         as BS (ByteString, all, elem)
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.MessagePack
import           Data.Monoid
import           Neovim.API.String
import           Neovim.Classes
import           Neovim.Context
import           Neovim.RPC.FunctionCall

import           Prelude

setqflist :: (Monoid strType, NvimObject strType)
          => [QuickfixListItem strType] -> QuickfixAction -> Neovim r st ()
setqflist qs a =
    void . wait' $ vim_call_function "setqflist" [toObject qs, toObject a]

-- | Quickfix list item. The parameter names should mostly conform to those in
-- @:h setqflist()@. Some fields are merged to explicitly state mutually
-- exclusive elements or some other behavior of the fields.
--
-- see 'quickfixListItem' for creating a value of this type without typing too
-- much.
data QuickfixListItem strType = QFItem
    { bufOrFile     :: Either Int strType
    -- ^ Since the filename is only used if no buffer can be specified, this
    -- field is a merge of @bufnr@ and @filename@.
    , lnumOrPattern :: Either Int strType
    -- ^ Line number or search pattern to locate the error.
    , col           :: Maybe (Int, Bool)
    -- ^ A tuple of a column number and a boolean indicating which kind of
    -- indexing should be used. 'True' means that the visual column should be
    -- used. 'False' means to use the byte index.
    , nr            :: Maybe Int
    -- ^ Error number.
    , text          :: strType
    -- ^ Description of the error.
    , errorType     :: QuickfixErrorType
    -- ^ Type of error.
    } deriving (Eq, Show)


data QuickfixErrorType = Warning | Error
    deriving (Eq, Ord, Show, Read, Enum, Bounded)


instance NvimObject QuickfixErrorType where
    toObject = \case
        Warning -> ObjectBinary "W"
        Error   -> ObjectBinary "E"

    fromObject o = case fromObject o :: Either String String of
        Right "W" -> return Warning
        Right "E" -> return Error
        _         -> return Error


-- | Create a 'QuickfixListItem' by providing the minimal amount of arguments
-- needed.
quickfixListItem :: (Monoid strType)
                 => Either Int strType -- ^ buffer of file name
                 -> Either Int strType -- ^ line number or pattern
                 -> QuickfixListItem strType
quickfixListItem bufferOrFile lineOrPattern = QFItem
    { bufOrFile = bufferOrFile
    , lnumOrPattern = lineOrPattern
    , col = Nothing
    , nr = Nothing
    , text = mempty
    , errorType = Error
    }


instance (Monoid strType, NvimObject strType)
            => NvimObject (QuickfixListItem strType) where
    toObject QFItem{..} =
        (toObject :: Map.Map ByteString Object -> Object) . Map.fromList $
            [ either (\b -> ("bufnr", toObject b))
                     (\f -> ("filename", toObject f))
                     bufOrFile
            , either (\l -> ("lnum", toObject l))
                     (\p -> ("pattern", toObject p))
                     lnumOrPattern
            , ("type", toObject errorType)
            , ("text", toObject text)
            ] ++ catMaybes
            [ (\n -> ("nr", toObject n)) <$> nr
            , (\(c,_) -> ("col", toObject c)) <$> col
            , (\(_,t) -> ("vcol", toObject t)) <$> col
            ]

    fromObject objectMap@(ObjectMap _) = do
        m <- fromObject objectMap
        let l :: NvimObject o => ByteString -> Either String o
            l key = case Map.lookup key m of
                Just o -> fromObject o
                Nothing -> Left "Key not found."
        bufOrFile <- case (l "bufnr", l "filename") of
            (Right b, _) -> return $ Left b
            (_, Right f) -> return $ Right f
            _           -> throwError "No buffer number or file name inside quickfix list item."
        lnumOrPattern <- case (l "lnum", l "pattern") of
            (Right lnum, _) -> return $ Left lnum
            (_, Right pat)  -> return $ Right pat
            _              -> throwError "No line number of search pattern inside quickfix list item."
        let l' :: NvimObject o => ByteString -> Either String (Maybe o)
            l' key = case Map.lookup key m of
                Just o -> Just <$> fromObject o
                Nothing -> return Nothing
        nr <- l' "nr" >>= \case
                Just 0 -> return Nothing
                nr' -> return nr'
        c <- l' "col"
        v <- l' "vcol"
        let col = do
                c' <- c
                v' <- v
                case c' of
                    0 -> Nothing
                    _ -> Just (c',v')
        text <- fromMaybe mempty <$> l' "text"
        errorType <- fromMaybe Error <$> l' "type"
        return QFItem{..}
    fromObject o = throwError $ "Could not deserialize QuickfixListItem, expected a map but received: " ++ show o

data QuickfixAction
    = Append -- ^ Add items to the current list (or create a new one if none exists).
    | Replace -- ^ Replace current list (or create a new one if none exists).
    | New    -- ^ Create a new list.
    deriving (Eq, Ord, Enum, Bounded, Show)

instance NvimObject QuickfixAction where
    toObject = \case
        Append  -> ObjectBinary "a"
        Replace -> ObjectBinary "r"
        New     -> ObjectBinary ""

    fromObject o = case fromObject o of
        Right "a" -> return Append
        Right "r" -> return Replace
        Right s | BS.all (`BS.elem` " \t\n\r") s -> return New
        _   -> Left "Could not convert to QuickfixAction"

