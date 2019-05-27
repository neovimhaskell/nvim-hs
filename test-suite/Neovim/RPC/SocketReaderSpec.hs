{-# LANGUAGE OverloadedStrings #-}
module Neovim.RPC.SocketReaderSpec
    where

import           Neovim
import           Neovim.Plugin.Classes
import           Neovim.RPC.SocketReader (parseParams)

import           Test.Hspec

spec :: Spec
spec = do
  describe "parseParams" $ do

    it "should pass the inner argument list as is for functions" $ do
      parseParams (Function (F "") Sync) [ObjectArray [ObjectNil, ObjectBinary "ABC"]]
        `shouldBe` [ObjectNil, ObjectBinary "ABC"]
      parseParams (Function (F "") Sync) [ObjectNil, ObjectBinary "ABC"]
        `shouldBe` [ObjectNil, ObjectBinary "ABC"]
      parseParams (Function (F "") Sync) []
        `shouldBe` []

    let defCmdArgs = def :: CommandArguments
    it "should filter out implicit arguments" $ do
      parseParams (Command (F "") (mkCommandOptions [CmdSync Sync, CmdNargs "*"]))
          [ObjectArray []]
        `shouldBe` [toObject defCmdArgs]
      parseParams (Command (F "") (mkCommandOptions [CmdSync Sync, CmdNargs "*"]))
          [ObjectArray [ObjectBinary "7", ObjectInt 7]]
        `shouldBe` [toObject defCmdArgs, ObjectBinary "7", ObjectInt 7]

    it "should set the CommandOptions argument as expected" $ do
      parseParams (Command (F "") (mkCommandOptions
                    [ CmdRange WholeFile, CmdBang, CmdNargs "*"]))
          [ ObjectArray [ObjectBinary "7", ObjectBinary "8", ObjectNil]
          , ObjectArray [ObjectInt 1, ObjectInt 12]
          , ObjectInt 1
          ]
        `shouldBe` [ toObject (defCmdArgs { bang = Just True, range = Just (1,12) })
                   , ObjectBinary "7"
                   , ObjectBinary "8"
                   , ObjectNil]

    it "should pass this test" $ do
        parseParams (Command (F "") (mkCommandOptions [CmdNargs "+", CmdRange WholeFile, CmdBang]))
            [ ObjectArray [ ObjectBinary "me"
                          , ObjectBinary "up"
                          , ObjectBinary "before"
                          , ObjectBinary "you"
                          , ObjectBinary "go"
                          , ObjectBinary "go"
                          ]
            , ObjectArray [ ObjectInt 1
                          , ObjectInt 27
                          ]
            , ObjectInt 0
            ]
          `shouldBe` [ toObject (defCmdArgs { bang = Just False, range = Just (1,27) })
                     , ObjectBinary "me"
                     , ObjectArray [ ObjectBinary "up"
                                   , ObjectBinary "before"
                                   , ObjectBinary "you"
                                   , ObjectBinary "go"
                                   , ObjectBinary "go"
                                   ]
                     ]

