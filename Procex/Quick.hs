{-# LANGUAGE BangPatterns #-}

module Procex.Quick
  ( (<!|),
    (<<<),
    (<|),
    (|!>),
    (|>),
    capture,
    captureNoThrow,
    captureLazy,
    captureLazyNoThrow,
    captureErr,
    captureErrNoThrow,
    captureErrLazy,
    captureErrLazyNoThrow,
    captureFd,
    captureFdNoThrow,
    captureFdLazy,
    captureFdLazyNoThrow,
    pipeArgStrIn,
    mq,
    quickCmd,
    QuickCmd,
    quickCmdArg,
    QuickCmdArg,
    toByteString,
    ToByteString,
  )
where

import Control.Concurrent.Async (Async)
import Control.DeepSeq (force)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Procex.Core
import Procex.Process
import System.IO (hClose)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Process (ProcessStatus)
import System.Posix.Types (Fd)

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
--(>>>) cmd handler = quickCmd $ pipeHOut 1 (\_ h -> B.hGetContents h >>= handler) cmd

-- Disabled with same reason as for `>>>`.
---- | Handle the output from stderr.
----infixl 1 !>>>
----
----(!>>>) :: QuickCmd a => Cmd -> (ByteString -> IO ()) -> a
----(!>>>) cmd handler = quickCmd $ pipeHOut 2 (\_ h -> B.hGetContents h >>= handler) cmd

-- | Pass an argument of the form @\/proc\/self\/fd\/\<n\>@ to the process,
-- where `n` is the reader end of a pipe which the passed string is written to.
pipeArgStrIn :: ToByteString b => b -> Cmd -> Cmd
pipeArgStrIn str = pipeArgHIn (\_ h -> B.hPut h (toByteString str) >> hClose h)

-- Disabled with same reason as for `>>>`.
--pipeArgStrOut :: (ByteString -> IO ()) -> Cmd -> Cmd
--pipeArgStrOut handler = pipeArgHOut (\_ h -> B.hGetContents h >>= handler)

attachFinalizer :: IO () -> ByteString -> IO ByteString
attachFinalizer finalizer str = B.fromChunks <$> go (B.toChunks str)
  where
    go' :: [BS.ByteString] -> IO [BS.ByteString]
    go' [] = finalizer >> pure []
    go' (x : xs) = (x :) <$> go xs

    go :: [BS.ByteString] -> IO [BS.ByteString]
    go = unsafeInterleaveIO . go'

-- | Capture the output of the fd of the command lazily.
-- If the process exits with a non-zero exit code,
-- reading from the bytestring will throw 'Procex.Process.CmdException'.
-- Garbage collection will close the pipe.
captureFdLazy :: Fd -> Cmd -> IO ByteString
captureFdLazy fd cmd = do
  (status, [h]) <- captureFdsAsHandles [fd] cmd
  out <- B.hGetContents h
  attachFinalizer (waitCmd status) out

-- | Capture the output of the fd of the command lazily. Ignores process exit code.
-- Garbage collection will close the pipe.
captureFdLazyNoThrow :: Fd -> Cmd -> IO ByteString
captureFdLazyNoThrow fd cmd = do
  (_, [h]) <- captureFdsAsHandles [fd] cmd
  B.hGetContents h

-- | 'captureFdLazy' for stdout.
captureLazy :: Cmd -> IO ByteString
captureLazy = captureFdLazy 1

-- | 'captureFdLazy' for stderr..
captureErrLazy :: Cmd -> IO ByteString
captureErrLazy = captureFdLazy 2

-- | 'captureFdLazyNoThrow' for stdout.
captureLazyNoThrow :: Cmd -> IO ByteString
captureLazyNoThrow = captureFdLazyNoThrow 1

-- | 'captureFdLazyNoThrow' for stderr.
captureErrLazyNoThrow :: Cmd -> IO ByteString
captureErrLazyNoThrow = captureFdLazyNoThrow 2

captureFd' :: Fd -> Cmd -> IO (Async ProcessStatus, ByteString)
captureFd' fd cmd = do
  (status, [h]) <- captureFdsAsHandles [fd] cmd
  !out <- force <$> B.hGetContents h
  pure (status, out)

-- | Capture the output of the fd of the command strictly, err if the command exits with a non-zero exit code.
captureFd :: Fd -> Cmd -> IO ByteString
captureFd fd cmd = do
  (status, out) <- captureFd' fd cmd
  waitCmd status
  pure out

-- | Capture the output of the fd of the command strictly. Ignores process exit code.
captureFdNoThrow :: Fd -> Cmd -> IO ByteString
captureFdNoThrow fd cmd = do
  (_, out) <- captureFd' fd cmd
  pure out

-- | 'captureFd' for stdout.
capture :: Cmd -> IO ByteString
capture = captureFd 1

-- | 'captureFdNoThrow' for stdout.
captureNoThrow :: Cmd -> IO ByteString
captureNoThrow = captureFdNoThrow 1

-- | 'captureFd' for stderr.
captureErr :: Cmd -> IO ByteString
captureErr = captureFd 2

-- | 'captureFdNoThrow' for stderr.
captureErrNoThrow :: Cmd -> IO ByteString
captureErrNoThrow = captureFdNoThrow 2
