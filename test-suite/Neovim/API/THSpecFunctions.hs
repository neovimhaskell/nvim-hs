module Neovim.API.THSpecFunctions where

import qualified Data.Map as Map
import Neovim

testFunction0 :: Neovim env Int
testFunction0 = return 42

testFunction2 :: CommandArguments -> String -> [String] -> Neovim env Double
testFunction2 _ _ _ = return 2

testFunctionMap :: Map.Map String Int -> String -> Neovim env (Maybe Int)
testFunctionMap m k = return $ Map.lookup k m

testSucc :: Int -> Neovim env Int
testSucc = return . succ

testCommandOptArgument :: CommandArguments -> Maybe String -> Neovim env String
testCommandOptArgument _ ms = case ms of
    Just x -> return x
    Nothing -> return "default"
