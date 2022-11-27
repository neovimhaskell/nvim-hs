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
    , catchNeovimException
    ) where

import           Control.Exception                         (Exception)
import           Data.MessagePack                          (Object (..))
import           Data.String                               (IsString (..))
import           Prettyprinter                             (Doc, (<+>), viaShow)
import           Prettyprinter.Render.Terminal             (AnsiStyle)
import           Data.Typeable                             (Typeable)
import           UnliftIO                                  (MonadUnliftIO, catch)

-- | Exceptions specific to /nvim-hs/.
data NeovimException
    = ErrorMessage (Doc AnsiStyle)
    -- ^ Simple error message that is passed to neovim. It should currently only
    -- contain one line of text.
    | ErrorResult (Doc AnsiStyle) Object
    -- ^ Error that can be returned by a remote API call. The 'Doc' argument is
    -- the name of the remote function that threw this exception.
    deriving (Typeable, Show)


instance Exception NeovimException


instance IsString NeovimException where
    fromString = ErrorMessage . fromString


exceptionToDoc :: NeovimException -> Doc AnsiStyle
exceptionToDoc = \case
    ErrorMessage e ->
        "Error message:" <+> e

    ErrorResult fn o ->
        "Function" <+> fn <+> "has thrown an error:" <+> viaShow o

-- | Specialization of 'catch' for 'NeovimException's.
catchNeovimException :: MonadUnliftIO io => io a -> (NeovimException -> io a) -> io a
catchNeovimException action exceptionHandler = action `catch` exceptionHandler
