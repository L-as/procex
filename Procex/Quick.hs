module Procex.Quick (mq, QuickCmd, QuickCmdArg, quickCmd, quickCmdArg, ToByteString, toByteString, (<|), (|>), (<!|), (|!>), (<<<), (>>>), (!>>>), pipeArgStrIn, pipeArgStrOut, capture, captureErr) where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Procex.Core
import Procex.Process
import System.IO (hClose)

-- | A helper class to convert to bytestrings with UTF-8 encoding
class ToByteString a where
  toByteString :: a -> B.ByteString

instance a ~ Char => ToByteString [a] where
  toByteString = B.fromString

instance ToByteString B.ByteString where
  toByteString = id

instance ToByteString BS.ByteString where
  toByteString = B.fromStrict

-- | A helper class to allow lightweight syntax for executing commands
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

-- | >>> mq "cat" "/dev/null" (pipeArgIn 1 $ mq "cat" "/dev/null") <<< "somestr"
--
-- The first argument is the path, and the subsequent arguments are 'QuickCmdArg'.
-- At the end you will either have an @IO ()@ (synchronous execution) or 'Cmd' (which you can further use).
mq ::
  (QuickCmd a, ToByteString b) =>
  -- | The path to the executable, uses PATH
  b ->
  -- | Either a 'Cmd', an @IO ()@, or a function that takes @Cmd -> Cmd@ , 'String' or 'ByteString'
  a
mq path = quickCmd $ makeCmd (toByteString path)

-- | Pipe from the right command to the left command.
-- Returns the left command modified.
infixl 1 <|

(<|) :: QuickCmd a => Cmd -> Cmd -> a
(<|) x y = quickCmd $ pipeIn 1 0 y x

-- | Pipe from the right command's stderr to the left command.
-- Returns the left command modified.
infixl 1 <!|

(<!|) :: QuickCmd a => Cmd -> Cmd -> a
(<!|) x y = quickCmd $ pipeIn 2 0 y x

-- | Pipe from the left command to the right command.
-- Returns the left command modified.
infixl 1 |>

(|>) :: QuickCmd a => Cmd -> Cmd -> a
(|>) x y = quickCmd $ pipeOut 0 1 y x

-- | Pipe from the left command's stderr to the right command.
-- Returns the left command modified.
infixl 1 |!>

(|!>) :: QuickCmd a => Cmd -> Cmd -> a
(|!>) x y = quickCmd $ pipeOut 0 2 y x

-- | Pass a string as stdin.
infixl 1 <<<

(<<<) :: (QuickCmd a, ToByteString b) => Cmd -> b -> a
(<<<) cmd str = quickCmd $ pipeHIn 0 (\_ h -> B.hPut h (toByteString str) >> hClose h) cmd

-- This function is pretty much never useful. If you want to handle the output
-- of the command, use `capture` or similar.
-- The problem is that it creates a new thread in the background, when
-- what we really want is to handle the output in the foreground, because
-- when our foreground is done executing, it will not wait for the background threads
-- to stop executing too.
---- | Handle the output from stdout.
--infixl 1 >>>
--
--(>>>) :: QuickCmd a => Cmd -> (ByteString -> IO ()) -> a
--(>>>) cmd handler = quickCmd $ pipeHOut 1 (\_ h -> B.hGetContents h >>= handler >> hClose h) cmd

-- Disabled with same reason as for `>>>`.
---- | Handle the output from stderr.
----infixl 1 !>>>
----
----(!>>>) :: QuickCmd a => Cmd -> (ByteString -> IO ()) -> a
----(!>>>) cmd handler = quickCmd $ pipeHOut 2 (\_ h -> B.hGetContents h >>= handler >> hClose h) cmd

-- | Pass an argument of the form @\/proc\/self\/fd\/\<n\>@ to the process,
-- where `n` is the reader end of a pipe which the passed string is written to.
pipeArgStrIn :: ToByteString b => b -> Cmd -> Cmd
pipeArgStrIn str = pipeArgHIn (\_ h -> B.hPut h (toByteString str) >> hClose h)

-- Disabled with same reason as for `>>>`.
--pipeArgStrOut :: (ByteString -> IO ()) -> Cmd -> Cmd
--pipeArgStrOut handler = pipeArgHOut (\_ h -> B.hGetContents h >>= handler >> hClose h)

-- | Capture the stdout of the command lazily
capture :: Cmd -> IO ByteString
capture = captureFd 1

-- | Capture the stderr of the command lazily
captureErr :: Cmd -> IO ByteString
captureErr = captureFd 2
