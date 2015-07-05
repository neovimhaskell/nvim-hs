module Neovim.API.THSpecFunctions
    where

import Neovim.Context
import qualified Data.Map as Map

testFunction0 :: Neovim' Int
testFunction0 = return 42

testFunction2 :: String -> Int -> Neovim' Double
testFunction2 _ _ = return 2

testFunctionMap :: Map.Map String Int -> String -> Neovim' (Maybe Int)
testFunctionMap m k = return $ Map.lookup k m

testSucc :: Int -> Neovim r st Int
testSucc = return . succ
