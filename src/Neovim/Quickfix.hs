{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Neovim.Quickfix
Description :  API for interacting with the quickfix list
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC
-}
module Neovim.Quickfix where

import Neovim.API.String ( vim_call_function )
import Neovim.Classes
    ( Generic,
      NFData,
      (<+>),
      Doc,
      AnsiStyle,
      NvimObject(toObject, fromObject),
      (+:) )
import Neovim.Context ( throwError, Neovim )


import Control.Monad (void)
import Data.ByteString as BS (ByteString, all, elem)
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import Data.MessagePack ( Object(ObjectBinary, ObjectMap) )
import Prettyprinter (viaShow)
import Prelude

{- | This is a wrapper around neovim's @setqflist()@. @strType@ can be any
 string that you can append to (hence 'Monoid') that is also an instance
 of 'NvimObject'. You can e.g. use the plain old 'String'.
-}
setqflist ::
    (Monoid strType, NvimObject strType) =>
    [QuickfixListItem strType] ->
    QuickfixAction ->
    Neovim env ()
setqflist qs a =
    void $ vim_call_function "setqflist" $ qs +: a +: []

data ColumnNumber
    = VisualColumn Int
    | ByteIndexColumn Int
    | NoColumn
    deriving (Eq, Ord, Show, Generic)

instance NFData ColumnNumber

data SignLocation strType
    = LineNumber Int
    | SearchPattern strType
    deriving (Eq, Ord, Show, Generic)

instance (NFData strType) => NFData (SignLocation strType)

{- | Quickfix list item. The parameter names should mostly conform to those in
 @:h setqflist()@. Some fields are merged to explicitly state mutually
 exclusive elements or some other behavior of the fields.

 see 'quickfixListItem' for creating a value of this type without typing too
 much.
-}
data QuickfixListItem strType = QFItem
    { -- | Since the filename is only used if no buffer can be specified, this
      -- field is a merge of @bufnr@ and @filename@.
      bufOrFile :: Either Int strType
    , -- | Line number or search pattern to locate the error.
      lnumOrPattern :: Either Int strType
    , -- | A tuple of a column number and a boolean indicating which kind of
      -- indexing should be used. 'True' means that the visual column should be
      -- used. 'False' means to use the byte index.
      col :: ColumnNumber
    , -- | Error number.
      nr :: Maybe Int
    , -- | Description of the error.
      text :: strType
    , -- | Type of error.
      errorType :: QuickfixErrorType
    }
    deriving (Eq, Show, Generic)

instance (NFData strType) => NFData (QuickfixListItem strType)

-- | Simple error type enum.
data QuickfixErrorType = Warning | Error
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance NFData QuickfixErrorType

instance NvimObject QuickfixErrorType where
    toObject = \case
        Warning -> ObjectBinary "W"
        Error -> ObjectBinary "E"

    fromObject o = case fromObject o :: Either (Doc AnsiStyle) String of
        Right "W" -> return Warning
        Right "E" -> return Error
        _ -> return Error

{- | Create a 'QuickfixListItem' by providing the minimal amount of arguments
 needed.
-}
quickfixListItem ::
    (Monoid strType) =>
    -- | buffer of file name
    Either Int strType ->
    -- | line number or pattern
    Either Int strType ->
    QuickfixListItem strType
quickfixListItem bufferOrFile lineOrPattern =
    QFItem
        { bufOrFile = bufferOrFile
        , lnumOrPattern = lineOrPattern
        , col = NoColumn
        , nr = Nothing
        , text = mempty
        , errorType = Error
        }

instance
    (Monoid strType, NvimObject strType) =>
    NvimObject (QuickfixListItem strType)
    where
    toObject QFItem{..} =
        (toObject :: Map.Map ByteString Object -> Object) . Map.fromList $
            [ either
                (\b -> ("bufnr", toObject b))
                (\f -> ("filename", toObject f))
                bufOrFile
            , either
                (\l -> ("lnum", toObject l))
                (\p -> ("pattern", toObject p))
                lnumOrPattern
            , ("type", toObject errorType)
            , ("text", toObject text)
            ]
                ++ case col of
                    NoColumn -> []
                    ByteIndexColumn i -> [("col", toObject i), ("vcol", toObject False)]
                    VisualColumn i -> [("col", toObject i), ("vcol", toObject True)]

    fromObject objectMap@(ObjectMap _) = do
        m <- fromObject objectMap
        let l :: NvimObject o => ByteString -> Either (Doc AnsiStyle) o
            l key = case Map.lookup key m of
                Just o -> fromObject o
                Nothing -> throwError $ "Key not found."
        bufOrFile <- case (l "bufnr", l "filename") of
            (Right b, _) -> return $ Left b
            (_, Right f) -> return $ Right f
            _ -> throwError $ "No buffer number or file name inside quickfix list item."
        lnumOrPattern <- case (l "lnum", l "pattern") of
            (Right lnum, _) -> return $ Left lnum
            (_, Right pat) -> return $ Right pat
            _ -> throwError $ "No line number or search pattern inside quickfix list item."
        let l' :: NvimObject o => ByteString -> Either (Doc AnsiStyle) (Maybe o)
            l' key = case Map.lookup key m of
                Just o -> Just <$> fromObject o
                Nothing -> return Nothing
        nr <-
            l' "nr" >>= \case
                Just 0 -> return Nothing
                nr' -> return nr'
        c <- l' "col"
        v <- l' "vcol"
        let col = fromMaybe NoColumn $ do
                c' <- c
                v' <- v
                case (c', v') of
                    (0, _) -> return NoColumn
                    (_, True) -> return $ VisualColumn c'
                    (_, False) -> return $ ByteIndexColumn c'
        text <- fromMaybe mempty <$> l' "text"
        errorType <- fromMaybe Error <$> l' "type"
        return QFItem{..}
    fromObject o =
        throwError $
            "Could not deserialize QuickfixListItem,"
                <+> "expected a map but received:"
                <+> viaShow o

data QuickfixAction
    = -- | Add items to the current list (or create a new one if none exists).
      Append
    | -- | Replace current list (or create a new one if none exists).
      Replace
    | -- | Create a new list.
      New
    deriving (Eq, Ord, Enum, Bounded, Show, Generic)

instance NFData QuickfixAction

instance NvimObject QuickfixAction where
    toObject = \case
        Append -> ObjectBinary "a"
        Replace -> ObjectBinary "r"
        New -> ObjectBinary ""

    fromObject o = case fromObject o of
        Right "a" -> return Append
        Right "r" -> return Replace
        Right s | BS.all (`BS.elem` " \t\n\r") s -> return New
        _ -> Left "Could not convert to QuickfixAction"
