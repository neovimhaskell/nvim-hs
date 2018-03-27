module Neovim.Plugin.ConfigHelperSpec
    where

import           Test.Hspec
import           Test.HUnit                          (assertFailure)

import           Neovim.Plugin.ConfigHelper.Internal
import           Neovim.Quickfix

import           Text.Megaparsec

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft


readFileUTF8 :: FilePath -> IO String
readFileUTF8 = fmap BS.toString . BS.readFile

spec :: Spec
spec = do
  describe "parseQuickFfixItems" $ do
    it "should match this test file 1" $ do
      e <- readFileUTF8 "./test-files/compile-error-for-quickfix-test1"
      let [qs] = parseQuickfixItems e
      bufOrFile qs `shouldBe` Right "/.config/nvim/nvim.hs"
      lnumOrPattern qs `shouldBe` Left 25
      col qs `shouldBe` VisualColumn 30
      nr qs `shouldBe` Nothing
      errorType qs `shouldBe` Error

    it "should match this test file 2" $ do
      e <- readFileUTF8 "./test-files/compile-error-for-quickfix-test2"
      let [qs] = parseQuickfixItems e
      bufOrFile qs `shouldBe` Right "/.config/nvim/lib/TestPlugins.hs"
      lnumOrPattern qs `shouldBe` Left 11
      col qs `shouldBe` VisualColumn 18
      nr qs `shouldBe` Nothing
      errorType qs `shouldBe` Error

    it "should match this test file 3" $ do
      e <- readFileUTF8 "./test-files/compile-error-for-quickfix-test3"
      let [qs] = parseQuickfixItems e
      bufOrFile qs `shouldBe` Right "/.config/nvim/nvim.hs"
      lnumOrPattern qs `shouldBe` Left 25
      col qs `shouldBe` VisualColumn 30
      nr qs `shouldBe` Nothing
      errorType qs `shouldBe` Error

    it "should match this test file 4" $ do
      e <- readFileUTF8 "./test-files/compile-error-for-quickfix-test4"
      let [qs] = parseQuickfixItems e
      bufOrFile qs `shouldBe` Right "/.config/nvim/lib/TestPlugins.hs"
      lnumOrPattern qs `shouldBe` Left 23
      col qs `shouldBe` VisualColumn 9
      nr qs `shouldBe` Nothing
      errorType qs `shouldBe` Error

    it "should match this test file 5" $ do
      e <- readFileUTF8 "./test-files/compile-error-for-quickfix-test5"
      let [q1,q2] = parseQuickfixItems e
      bufOrFile q1 `shouldBe` Right "/.config/nvim/lib/TestPlugins.hs"
      bufOrFile q2 `shouldBe` Right "/.config/nvim/lib/TestPlugins.hs"
      lnumOrPattern q1 `shouldBe` Left 23
      lnumOrPattern q2 `shouldBe` Left 23
      col q1 `shouldBe` VisualColumn 9
      col q2 `shouldBe` VisualColumn 32
      nr q1 `shouldBe` Nothing
      nr q2 `shouldBe` Nothing
      errorType q1 `shouldBe` Warning
      errorType q2 `shouldBe` Warning

    it "should match this test file 6" $ do
      e <- readFileUTF8 "./test-files/compile-error-for-quickfix-test6"
      let [q1] = parseQuickfixItems e
      bufOrFile q1 `shouldBe` Right "/home/test/.config/nvim/nvim.hs"
      lnumOrPattern q1 `shouldBe` Left 8
      col q1 `shouldBe` VisualColumn 7
      nr q1 `shouldBe` Nothing
      errorType q1 `shouldBe` Error

    it "should parse all files concatenated" $ do
      e1 <- readFileUTF8 "./test-files/compile-error-for-quickfix-test1"
      e2 <- readFileUTF8 "./test-files/compile-error-for-quickfix-test2"
      e3 <- readFileUTF8 "./test-files/compile-error-for-quickfix-test3"
      let qs = parseQuickfixItems $ unlines [e1,e2,e3]
      length qs `shouldBe` 3
      case qs of
          [q1, q2, q3] -> do
              lnumOrPattern q1 `shouldBe` Left 25
              lnumOrPattern q2 `shouldBe` Left 11
              lnumOrPattern q3 `shouldBe` Left 25
          _ -> assertFailure "Expected three quickfix list items."

  describe "pShortDesrciption" $ do
    it "should fail for an empty input" $
      parse pShortDesrciption "" "" `shouldSatisfy` isLeft
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

