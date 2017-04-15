{-# LANGUAGE LambdaCase #-}
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
    ) where

import           Control.Exception            (Exception)
import           Data.MessagePack             (Object (..))
import           Data.String                  (IsString (..))
import           Data.Typeable                (Typeable)
import           Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..))

-- | Exceptions specific to /nvim-hs/.
data NeovimException
    = ErrorMessage Doc
    -- ^ Simply error message that is passed to neovim. It should currently only
    -- contain one line of text.
    | ErrorResult Object
    -- ^ Error that can be returned by a remote API call. A call of 'fromObject'
    -- on this value could be converted to a value of 'NeovimExceptionGen'.
    deriving (Typeable, Show)


instance Exception NeovimException


instance IsString NeovimException where
    fromString = ErrorMessage . fromString


instance Pretty NeovimException where
    pretty = \case
        ErrorMessage s -> s
        ErrorResult e -> pretty $ show e
