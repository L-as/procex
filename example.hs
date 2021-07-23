-- You will need this
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- You might not need this
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import "procex" Procex.Prelude
import Control.Exception
import System.Exit
import Control.Concurrent.Async (wait)
import System.Posix.Process
import Control.Monad
import Test.Hspec
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = hspec $ do
  describe "capture" $ do
    it "captures stdout" $ capture (mq "echo" "xjjj") `shouldReturn` "xjjj\n"

  describe "captureLazy" $ do
    it "captures stdout lazily" $ do
      out <- captureLazy $ mq "cat" "/dev/zero"
      B.take 16 out `shouldBe` B.pack (take 16 $ repeat 0)

  describe "captureLazyNoThrow" $ do
    it "captures stdout lazily without throwing" $ do
      out <- captureLazyNoThrow $ mq "diff" (pipeArgStrIn "ab\ncd") (pipeArgStrIn "ab\nce")
      out `shouldBe` "2c2\n< cd\n\\ No newline at end of file\n---\n> ce\n\\ No newline at end of file\n"

  describe "run" $ do
    it "throws when cmd fails" $ do
      run (mq "false") `shouldThrow` \(_ :: CmdException) -> True
