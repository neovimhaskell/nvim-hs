{-# LANGUAGE LambdaCase #-}
{- |
Module      :  Neovim.Util
Description :  Utility functions
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Util (
    withCustomEnvironment,
    whenM,
    unlessM,
    oneLineErrorMessage,
    ) where

import           Control.Monad       (forM, forM_, when, unless)
import           Neovim.Context
import           System.SetEnv
import           System.Environment
import qualified Text.PrettyPrint.ANSI.Leijen as P
import           UnliftIO (MonadUnliftIO)
import           UnliftIO.Exception  (bracket)


-- | Execute the given action with a changed set of environment variables and
-- restore the original state of the environment afterwards.
withCustomEnvironment
    :: (Traversable t, MonadUnliftIO m)
    => t (String, Maybe String)
    -> m c
    -> m c
withCustomEnvironment modifiedEnvironment action =
    bracket saveAndSet unset (const action)

  where
    saveAndSet = do
        preservedValues <- forM modifiedEnvironment $ \(var, val) -> liftIO $ do
            old <- lookupEnv var
            maybe (unsetEnv var) (setEnv var) val
            return (var, old)
        return preservedValues

    unset preservedValues = forM_ preservedValues $ \(var, val) -> liftIO $
        maybe (unsetEnv var) (setEnv var) val


-- | 'when' with a monadic predicate.
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mp a = mp >>= \p -> when p a


-- | 'unless' with a monadic predicate.
unlessM :: (Monad m) => m Bool -> m () -> m ()
unlessM mp a = mp >>= \p -> unless p a


oneLineErrorMessage :: P.Doc -> String
oneLineErrorMessage d = case lines $ P.displayS (P.renderCompact d) "" of
    (x:_) -> x
    []    -> ""
