{-# LANGUAGE OverloadedStrings #-}
module TestPlugins.TestFunctions
    where

import Neovim

import System.Log.Logger

complicatedCommand :: CommandArguments -> Neovim r st ()
complicatedCommand _ = return ()

rf :: Neovim cfg [Int16] Object
rf = do
    r <- gets head
    modify tail
    return $ ObjectInt (fromIntegral r)

inj :: Int16 -> Neovim cfg [Int16] ()
inj n = do
    liftIO . debugM "TestPlugins.hs" $ "updating map"
    modify (n:)
    startofints <- gets (take 10)
    liftIO . debugM "TestPlugins.hs" $ "Updated map to: " <> show startofints

randoom :: Neovim cfg st ()
randoom = err "Function not supported"

const42 :: Neovim cfg st Int
const42 = return 42

pingNvimhs :: Neovim cfg st String
pingNvimhs = return "Pong"

registerFunctionLazily :: Neovim r [Int16] ()
registerFunctionLazily =
    void $ addAutocmd "FuncUndefined" (def { acmdGroup = Just "shizzle" }) (inj 1337)
