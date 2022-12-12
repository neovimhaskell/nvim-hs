module RPC.CommonSpec where

import Neovim.RPC.Common

import Test.Hspec

spec :: Spec
spec = do
    describe "Parsing of $NVIM environment variable" $ do
        it "is a UnixSocket if it doesn't contain a colon" $ do
            UnixSocket actual <- parseNvimEnvironmentVariable "/some/file"
            actual `shouldBe` "/some/file"
        it "is a tcp connection if it contains a colon" $ do
            TCP actualPort actualHostname <- parseNvimEnvironmentVariable "localhost:12345"
            actualPort `shouldBe` 12345
            actualHostname `shouldBe` "localhost"
        it "the last number after many colons is the port" $ do
            TCP actualPort actualHostname <- parseNvimEnvironmentVariable "the:cake:is:a:lie:777"
            actualPort `shouldBe` 777
            actualHostname `shouldBe` "the:cake:is:a:lie"
