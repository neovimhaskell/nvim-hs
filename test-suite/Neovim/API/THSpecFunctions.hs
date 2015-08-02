module Neovim.API.THSpecFunctions
    where

import qualified Data.Map as Map
import           Neovim

testFunction0 :: Neovim' Int
testFunction0 = return 42

testFunction2 :: CommandArguments -> String -> [String] -> Neovim' Double
testFunction2 _ _ _ = return 2

testFunctionMap :: Map.Map String Int -> String -> Neovim' (Maybe Int)
testFunctionMap m k = return $ Map.lookup k m

testSucc :: Int -> Neovim r st Int
testSucc = return . succ

testCommandOptArgument :: CommandArguments -> Maybe String -> Neovim' String
testCommandOptArgument _ ms = case ms of
    Just x  -> return x
    Nothing -> return "default"
