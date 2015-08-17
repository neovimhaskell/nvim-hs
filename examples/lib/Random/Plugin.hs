module Random.Plugin (nextRandom, setNextRandom) where

import Neovim

-- | Neovim isn't so good with big numbers here either.
nextRandom :: Neovim r [Int16] Int16
nextRandom = do
    r <- gets head -- get the head of the infinite random number list
    modify tail    -- set the list to its tail
    return r

setNextRandom :: Int16 -> Neovim r [Int16] ()
setNextRandom n = modify (n:) -- cons to the front of the infinite list

