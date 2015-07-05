{-# LANGUAGE LambdaCase #-}
{- |
Module      :  Neovim.Plugin.ConfigHelper.Internal
Description :  Internals for a config helper plugin that helps recompiling nvim-hs
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Plugin.ConfigHelper.Internal
    where

import           Neovim.Context
import           Neovim.Config

import           Config.Dyre         (Params)
import           Config.Dyre.Compile

import           System.Log.Logger

pingNvimhs :: Neovim' String
pingNvimhs = return "Pong"

recompileNvimhs :: Neovim (Params NeovimConfig) (Maybe String) ()
recompileNvimhs = do
    cfg <- ask
    liftIO (customCompile cfg >> getErrorString cfg) >>= \case
        Nothing -> return ()
        Just e -> put (Just e) -- TODO open the quickfix window

-- | Note that restarting the plugin provider implies compilation because Dyre
-- does this automatically. However, if the recompilation fails, the previously
-- compiled bynary is executed. This essentially means that restarting may take
-- more time then you might expect.
restartNvimhs :: Neovim r st ()
restartNvimhs = do
    liftIO $ debugM "ConfigHelper" "Issuing restart"
    restart
