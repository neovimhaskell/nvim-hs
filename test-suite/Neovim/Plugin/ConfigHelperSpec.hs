module Neovim.Plugin.ConfigHelperSpec
    where

import Test.Hspec

import Neovim.Quickfix
import Neovim.Plugin.ConfigHelper.Internal

import Text.Parsec

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft

spec :: Spec
spec = do
  describe "parseQuickFfixItems" $ do
    it "should match this test file 1" $ do
      e <- readFile "./test-files/compile-error-for-quickfix-test1"
      let [qs] = parseQuickfixItems e
      bufOrFile qs `shouldBe` Right "/.config/nvim/nvim.hs"
      lnumOrPattern qs `shouldBe` Left 25
      col qs `shouldBe` Just (30, True)
      nr qs `shouldBe` Nothing
      errorType qs `shouldBe` Just 'E'
    it "should match this test file 2" $ do
      e <- readFile "./test-files/compile-error-for-quickfix-test2"
      let [qs] = parseQuickfixItems e
      bufOrFile qs `shouldBe` Right "/.config/nvim/lib/TestPlugins.hs"
      lnumOrPattern qs `shouldBe` Left 11
      col qs `shouldBe` Just (18, True)
      nr qs `shouldBe` Nothing
      errorType qs `shouldBe` Just 'E'
    it "should match this test file 3" $ do
      e <- readFile "./test-files/compile-error-for-quickfix-test3"
      let [qs] = parseQuickfixItems e
      bufOrFile qs `shouldBe` Right "/.config/nvim/nvim.hs"
      lnumOrPattern qs `shouldBe` Left 25
      col qs `shouldBe` Just (30, True)
      nr qs `shouldBe` Nothing
      errorType qs `shouldBe` Just 'E'

  describe "pShortDesrciption" $ do
    it "should fail for an empty input" $
      parse pShortDesrciption "" "" `shouldSatisfy` isLeft
    it "should fail for an imput solely consistting of spaces" $
      parse pShortDesrciption "" "  \t " `shouldSatisfy` isLeft
    it "should fail for an input solely consisting of spaces followed by a newline" $
      parse pShortDesrciption "" " \t \t\t \n" `shouldSatisfy` isLeft
    it "should parse this simple expression" $
      parse pShortDesrciption "" "this simple expression\n"
        `shouldBe` Right "this simple expression"

  describe "pLongDescription" $ do
    it "should parse anything until there is a blank line" $ do
      parse pLongDescription "" "\n\n"
        `shouldBe` Right ""
      parse pLongDescription "" "test\n    \n"
        `shouldBe` Right "test"
      parse pLongDescription "" "test\n    ibus\n \n"
        `shouldBe` Right "test\n    ibus"

