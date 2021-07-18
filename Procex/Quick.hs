module Procex.Quick (mq, ξ, QuickCmd, QuickCmdArg, quickCmd, quickCmdArg, ToByteString, toByteString, (<|), (|>), (<!|), (|!>), (<<<), (>>>), (!>>>)) where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Procex.Core
import Procex.Process
import System.IO (hClose)

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
  quickCmdArg s = passArg $ B.fromString s

instance QuickCmdArg ByteString where
  quickCmdArg = passArg

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

infixl 1 <|

infixl 1 <!|

infixl 1 |>

infixl 1 |!>

infixl 1 <<<

infixl 1 >>>

infixl 1 !>>>

(<|) :: QuickCmd a => Cmd -> Cmd -> a
(<|) x y = quickCmd $ pipeIn 1 0 y x

(<!|) :: QuickCmd a => Cmd -> Cmd -> a
(<!|) x y = quickCmd $ pipeIn 2 0 y x

(|>) :: QuickCmd a => Cmd -> Cmd -> a
(|>) x y = quickCmd $ pipeOut 0 1 y x

(|!>) :: QuickCmd a => Cmd -> Cmd -> a
(|!>) x y = quickCmd $ pipeOut 0 2 y x

(<<<) :: (QuickCmd a, ToByteString b) => Cmd -> b -> a
(<<<) cmd str = quickCmd $ pipeHIn 0 (\_ h -> B.hPut h (toByteString str) >> hClose h) cmd

(>>>) :: QuickCmd a => Cmd -> (ByteString -> IO ()) -> a
(>>>) cmd handler = quickCmd $ pipeHOut 1 (\_ h -> B.hGetContents h >>= handler >> hClose h) cmd

(!>>>) :: QuickCmd a => Cmd -> (ByteString -> IO ()) -> a
(!>>>) cmd handler = quickCmd $ pipeHOut 2 (\_ h -> B.hGetContents h >>= handler >> hClose h) cmd
