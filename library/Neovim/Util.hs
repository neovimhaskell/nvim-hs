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
    ) where

import           Control.Monad       (forM, forM_)
import           Control.Monad.Catch (MonadMask, bracket)
import           Neovim.Context
import           System.Environment
import           System.SetEnv


-- | Execute the given action with a changed set of environment variables and
-- restore the original state of the environment afterwards.
--
-- TODO use some lifted vriant of bracket since the environment variables are
--      not reset if @action@ has thrown an error.
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

