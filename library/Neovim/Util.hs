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
    ) where

import           Control.Monad       (forM, forM_, when, unless)
import           Control.Monad.Catch (MonadMask, bracket)
import           Neovim.Context
import           System.SetEnv
import           System.Environment


-- | Execute the given action with a changed set of environment variables and
-- restore the original state of the environment afterwards.
withCustomEnvironment :: (MonadMask io, MonadIO io)
                      => [(String, Maybe String)] -> io a -> io a
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
