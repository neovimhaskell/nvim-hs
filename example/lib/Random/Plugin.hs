module Random.Plugin (nextRandom, setNextRandom) where

import Neovim

import System.Random (newStdGen, randoms)
import UnliftIO.STM  (TVar, atomically, readTVar, modifyTVar, newTVarIO)

-- You may want to define a type alias for your plugin, so that if you change
-- your environment, you don't have to change all type signatures.
--
-- If I were to write a real plugin, I would probably also create a data type
-- instead of directly using a TVar here.
--
type MyNeovim a = Neovim (TVar [Int16]) a

-- This function will create an initial environment for our random number
-- generator. Note that the return type is the type of our environment.
randomNumbers :: Neovim startupEnv (TVar [Int16])
randomNumbers = do
    g <- liftIO newStdGen -- Create a new seed for a pseudo random number generator
    newTVarIO (randoms g) -- Put an infinite list of random numbers into a TVar

-- | Get the next random number and update the state of the list.
nextRandom :: MyNeovim Int16
nextRandom = do
    tVarWithRandomNumbers <- ask
    atomically $ do
        -- pick the head of our list of random numbers
        r <- head <$ readTVar tVarWithRandomNumbers

        -- Since we do not want to return the same number all over the place
        -- remove the head of our list of random numbers
        modifyTVar tVarWithRandomNumbers tail

        return r

