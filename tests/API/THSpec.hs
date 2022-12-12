{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.THSpec where

import API.THSpecFunctions

import Neovim.API.TH hiding (function)
import qualified Neovim.API.TH as TH
import Neovim.Context
import qualified Neovim.Context.Internal as Internal
import Neovim.Plugin.Classes
import Neovim.Plugin.Internal

import Data.Default
import qualified Data.Map as Map

import Test.Hspec
import Test.QuickCheck

call ::
    ([Object] -> Neovim () Object) ->
    [Object] ->
    IO Object
call f args = do
    cfg <- Internal.newConfig (pure Nothing) (pure ())
    res <- runNeovim cfg (f args)
    case res of
        Right x -> return x
        Left e -> (throwIO . ErrorMessage) e

isNeovimException :: NeovimException -> Bool
isNeovimException _ = True

spec :: Spec
spec = do
    describe "calling function without an argument" $ do
        let EF (Function fname _, testFun) = $(TH.function "TestFunction0" 'testFunction0) Sync
        it "should have a capitalized prefix" $
            fname `shouldBe` F "TestFunction0"

        it "should return the consant value" $
            call testFun [] `shouldReturn` ObjectInt 42

        it "should fail if supplied an argument" $
            call testFun [ObjectNil] `shouldThrow` isNeovimException

    describe "calling testFunction with two arguments" $ do
        let EF (Function fname _, testFun) = $(function' 'testFunction2) Sync
        it "should have a capitalized prefix" $
            fname `shouldBe` F "TestFunction2"

        it "should return 2 for proper arguments" $
            call
                testFun
                [ ObjectNil
                , ObjectString "ignored"
                , ObjectArray [ObjectString "42"]
                ]
                `shouldReturn` ObjectDouble 2

        it "should throw an exception for the wrong number of arguments" $
            call testFun [ObjectNil] `shouldThrow` isNeovimException

        it "should throw an exception for incompatible types" $
            call testFun [ObjectNil, ObjectBinary "ignored", ObjectString "42"]
                `shouldThrow` isNeovimException

        it "should cast arguments to similar types" $
            call testFun [ObjectNil, ObjectString "ignored", ObjectArray []]
                `shouldReturn` ObjectDouble 2

    describe "generating a command from the two argument test function" $ do
        let EF (Command fname _, _) = $(command' 'testFunction2) []
        it "should capitalize the first character" $
            fname `shouldBe` F "TestFunction2"

    describe "generating the test successor functions" $ do
        let EF (Function fname _, testFun) = $(function' 'testSucc) Sync
        it "should be named TestSucc" $
            fname `shouldBe` F "TestSucc"

        it "should return the old value + 1" . property $
            \x -> call testFun [ObjectInt x] `shouldReturn` ObjectInt (x + 1)

    describe "calling test function with a map argument" $ do
        let EF (Function fname _, testFun) = $(TH.function "TestFunctionMap" 'testFunctionMap) Sync
        it "should capitalize the first letter" $
            fname `shouldBe` F "TestFunctionMap"

        it "should fail for the wrong number of arguments" $
            call testFun [] `shouldThrow` isNeovimException

        it "should fail for the wrong type of arguments" $
            call testFun [ObjectInt 7, ObjectString "FOO"] `shouldThrow` isNeovimException

        it "should return Nothing for an empty map" $
            call testFun [toObject (Map.empty :: Map.Map String Int), ObjectString "FOO"]
                `shouldReturn` ObjectNil

        it "should return just the value for the singletion entry" $
            call testFun [toObject (Map.singleton "FOO" 7 :: Map.Map String Int), ObjectString "FOO"]
                `shouldReturn` ObjectInt 7

    describe "Calling function with an optional argument" $ do
        let EF (Command cname _, testFun) = $(command' 'testCommandOptArgument) []
            defCmdArgs = toObject (def :: CommandArguments)
        it "should capitalize the first letter" $
            cname `shouldBe` F "TestCommandOptArgument"

        it "should return \"default\" when passed no argument" $ do
            call testFun [defCmdArgs] `shouldReturn` toObject ("default" :: String)

        it "should return what is passed otherwise" . property $ do
            \str ->
                call testFun [defCmdArgs, toObject str]
                    `shouldReturn` toObject (str :: String)
