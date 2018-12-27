{-# LANGUAGE DeriveDataTypeable #-}
{- |
Module      :  Neovim.Exceptions
Description :  General Exceptions
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Exceptions
    ( NeovimException(..)
    , exceptionToDoc
    ) where

import           Control.Exception                         (Exception)
import           Data.MessagePack                          (Object (..))
import           Data.String                               (IsString (..))
import           Data.Text.Prettyprint.Doc                 (Doc, (<+>), viaShow)
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import           Data.Typeable                             (Typeable)

-- | Exceptions specific to /nvim-hs/.
data NeovimException
    = ErrorMessage (Doc AnsiStyle)
    -- ^ Simply error message that is passed to neovim. It should currently only
    -- contain one line of text.
    | ErrorResult Object
    -- ^ Error that can be returned by a remote API call. A call of 'fromObject'
    -- on this value could be converted to a value of 'NeovimExceptionGen'.
    deriving (Typeable, Show)


instance Exception NeovimException


instance IsString NeovimException where
    fromString = ErrorMessage . fromString


exceptionToDoc :: NeovimException -> Doc AnsiStyle
exceptionToDoc = \case
    ErrorMessage e ->
        "Error message:" <+> e

    ErrorResult o ->
        "Result representing an error:" <+> viaShow o

