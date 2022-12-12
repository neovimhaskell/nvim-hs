{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Plugin.ClassesSpec where

import Neovim
import Neovim.Plugin.Classes

import Test.Hspec
import Test.QuickCheck

newtype RandomCommandArguments = RCA {getRandomCommandArguments :: CommandArguments}
    deriving (Eq, Ord, Show, Read)

instance Arbitrary RandomCommandArguments where
    arbitrary = do
        bang <- arbitrary
        range <- arbitrary
        count <- arbitrary
        register <- fmap (fmap getNonEmpty) arbitrary
        return . RCA $ CommandArguments{..}

newtype RandomCommandOption = RCO {getRandomCommandOption :: CommandOption}
    deriving (Eq, Ord, Show, Read)

instance Arbitrary RandomCommandOption where
    arbitrary = do
        a <- choose (0, 5) :: Gen Int
        o <- case a of
            -- XXX Most constructor arguments are not tested anyway, so they are
            --     hardcoded for now.
            0 -> CmdSync <$> elements [Sync, Async]
            1 -> return CmdRegister
            2 -> return $ CmdNargs ""
            3 -> CmdRange <$> elements [CurrentLine, WholeFile, RangeCount 1]
            4 -> CmdCount <$> arbitrary
            _ -> return CmdBang
        return $ RCO o

newtype RandomCommandOptions = RCOs {getRandomCommandOptions :: CommandOptions}
    deriving (Eq, Ord, Show, Read)

instance Arbitrary RandomCommandOptions where
    arbitrary = do
        l <- choose (0, 20)
        RCOs . mkCommandOptions . map getRandomCommandOption <$> vectorOf l arbitrary

spec :: Spec
spec = do
    describe "Deserializing and serializing" $ do
        it "should be id for CommandArguments" . property $ do
            \args ->
                (fromObjectUnsafe . toObject . getRandomCommandArguments) args
                    `shouldBe` getRandomCommandArguments args

    describe "If a sync option is set for commands" $ do
        let isSyncOption = \case
                CmdSync _ -> True
                _ -> False
        it "must be at the head of the list" . property $ do
            \(RCOs opts) ->
                any isSyncOption (getCommandOptions opts) ==> do
                    length (filter isSyncOption (getCommandOptions opts)) `shouldBe` 1
                    head (getCommandOptions opts) `shouldSatisfy` isSyncOption
