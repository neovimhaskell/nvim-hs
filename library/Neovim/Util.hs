{- |
Module      :  Neovim.Util
Description :  Utility functions
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Util
    where

import Control.Monad (forM, forM_)
import System.Environment
import Control.Monad.IO.Class


-- | Execute the given action with a changed set of environment variables and
-- restore the original state of the environment afterwards.
withCustomEnvironment :: MonadIO io => [(String, Maybe String)] -> io a -> io a
withCustomEnvironment modifiedEnvironment act = do
    preservedValues <- forM modifiedEnvironment $ \(var, val) -> liftIO $ do
        old <- lookupEnv var
        maybe (unsetEnv var) (setEnv var) val
        return (var, old)

    a <- act

    forM_ preservedValues $ \(var, val) -> liftIO $
        maybe (unsetEnv var) (setEnv var) val

    return a


