{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Neovim.API.THSpec
    where

import Neovim.API.THSpecFunctions

import Neovim.API.Classes
import Neovim.API.TH
import Neovim.API.Context

import qualified Data.Map as Map

import Test.Hspec

import Control.Applicative
import Control.Concurrent.STM

call :: NvimObject a => ([Object] -> Neovim () () a) -> [Object]
     -> IO (Either String a)
call f args = do
    cfg <- ConfigWrapper <$> newTQueueIO <*> return ()
    fst <$> runNeovim cfg () (f args)

isNeovimException :: NeovimException -> Bool
isNeovimException _ = True

spec :: Spec
spec = do
  describe "calling function without an argument" $ do
    let testFun = $(function 'testFunction0)
    it "should return the consant value" $ do
      call testFun [] `shouldReturn` Right 42
    it "should fail if supplied an argument" $ do
        call testFun [ObjectNil] `shouldThrow` isNeovimException

  describe "calling testFunction" $ do
    let testFun = $(function 'testFunction2)
    it "should return 2 for proper arguments" $ do
      call testFun [ObjectBinary "ignored", ObjectInt 42] `shouldReturn` Right 2
    it "should throw an exception for the wrong number of arguments" $ do
      call testFun [] `shouldThrow` isNeovimException
    it "should throw an exception for incompatible types" $ do
      call testFun [ObjectBinary "ignored", ObjectNil] `shouldThrow` isNeovimException
    it "should cast arguments to similar types" $ do
      call testFun [ObjectString "ignored", ObjectFloat 42] `shouldReturn` Right 2

  describe "calling test function with a map argument" $ do
      let testFun = $(function 'testFunctionMap)
      it "should fail for the wrong number of arguments" $ do
        call testFun [] `shouldThrow` isNeovimException
      it "should fail for the wrong type of arguments" $ do
        call testFun [ObjectInt 7, ObjectString "FOO"] `shouldThrow` isNeovimException
      it "should return Nothing for an empty map" $ do
          call testFun [toObject (Map.empty :: Map.Map String Int), ObjectString "FOO"]
            `shouldReturn` Right Nothing
      it "should return just the value for the singletion entry" $ do
          call testFun [toObject (Map.singleton "FOO" 7 :: Map.Map String Int), ObjectString "FOO"]
            `shouldReturn` Right (Just 7)

