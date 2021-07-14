module Procex.Quick (mq, ξ, QuickCmd, QuickCmdArg, quickCmd, quickCmdArg, ToByteString, toByteString) where

import Procex.Process
import Procex.Core
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.ByteString as BS

class ToByteString a where
  toByteString :: a -> B.ByteString

instance a ~ Char => ToByteString [a] where
  toByteString = B.fromString

instance ToByteString B.ByteString where
  toByteString = id

instance ToByteString BS.ByteString where
  toByteString = B.fromStrict

class QuickCmdArg a where
  quickCmdArg :: a -> Cmd -> Cmd

class QuickCmd a where
  quickCmd :: Cmd -> a

instance QuickCmdArg String where
  quickCmdArg s = arg $ B.fromString s

instance QuickCmdArg ByteString where
  quickCmdArg = arg

instance QuickCmdArg (Cmd -> Cmd) where
  quickCmdArg = id

instance {-# OVERLAPPABLE #-} (QuickCmdArg a, QuickCmd b) => QuickCmd (a -> b) where
  quickCmd cmd arg = quickCmd $ quickCmdArg arg cmd

instance (a ~ ()) => QuickCmd (IO a) where
  quickCmd = run

instance QuickCmd Cmd where
  quickCmd = id

mq :: (QuickCmd a, ToByteString b) => b -> a
mq path = quickCmd $ makeCmd (toByteString path)

ξ :: (QuickCmd a, ToByteString b) => b -> a
ξ = mq
