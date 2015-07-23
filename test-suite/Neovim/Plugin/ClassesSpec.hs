{-# LANGUAGE RecordWildCards #-}
module Neovim.Plugin.ClassesSpec
    where

import Neovim

import Test.Hspec
import Test.QuickCheck


newtype RandomCommandArguments = RCA { getRandomCommandArguments :: CommandArguments }
    deriving (Eq, Ord, Show, Read)


instance Arbitrary RandomCommandArguments where
    arbitrary = do
        bang <- arbitrary
        range <- arbitrary
        count <- arbitrary
        register <- fmap (fmap getNonEmpty) arbitrary
        return . RCA $ CommandArguments{..}


spec :: Spec
spec = do
  describe "Deserializing ans serializing" $ do
    it "should be id for CommandArguments" . property $ do
      \args -> (fromObject . toObject . getRandomCommandArguments) args
        `shouldBe` Right (getRandomCommandArguments args)

