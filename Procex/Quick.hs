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

import Control.Concurrent.Async (Async, async, cancel)
import Control.DeepSeq (force)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Procex.Core
import Procex.Process
import System.IO (Handle, hClose)
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
    go :: [BS.ByteString] -> IO [BS.ByteString]
    go [] = unsafeInterleaveIO $ finalizer >> pure []
    go (x : xs) = (x :) <$> go xs

captureFdLazy' :: Fd -> (Async ProcessStatus -> IO ()) -> Cmd -> IO ByteString
captureFdLazy' fd finalizer cmd = do
  (status, [h]) <- captureFdsAsHandles [fd] cmd
  out <- B.hGetContents h
  attachFinalizer (finalizer status) out

-- | Capture the output of the fd of the command lazily.
-- If the process exits with a non-zero exit code,
-- reading from the bytestring will throw 'Procex.Process.CmdException'.
captureFdLazy :: Fd -> Cmd -> IO ByteString
captureFdLazy fd = captureFdLazy' fd waitCmd

-- | Capture the output of the fd of the command lazily. Ignores process exit code.
captureFdLazyNoThrow :: Fd -> Cmd -> IO ByteString
captureFdLazyNoThrow fd = captureFdLazy' fd (const $ pure ())

-- | Capture the stdout of the command lazily.
-- If the process exits with a non-zero exit code,
-- reading from the bytestring will throw 'Procex.Process.CmdException'.
captureLazy :: Cmd -> IO ByteString
captureLazy = captureFdLazy 1

-- | Capture the stderr of the command lazily.
-- If the process exits with a non-zero exit code,
-- reading from the bytestring will throw 'Procex.Process.CmdException'.
captureErrLazy :: Cmd -> IO ByteString
captureErrLazy = captureFdLazy 2

-- | Capture the stdout of the command lazily. Ignores process exit code.
captureLazyNoThrow :: Cmd -> IO ByteString
captureLazyNoThrow = captureFdLazyNoThrow 1

-- | Capture the stderr of the command lazily. Ignores process exit code.
captureErrLazyNoThrow :: Cmd -> IO ByteString
captureErrLazyNoThrow = captureFdLazyNoThrow 2

captureFd :: Fd -> Cmd -> IO (Async ProcessStatus, Handle)
captureFd fd cmd = (\(status, [h]) -> (status, h)) <$> captureFdsAsHandles [fd] cmd

-- | Capture the stdout of the command strictly, err if the command exits with a non-zero exit code.
capture :: Cmd -> IO ByteString
capture cmd = do
  (status, [h]) <- captureFdsAsHandles [1] cmd
  out <- force <$> B.hGetContents h
  waitCmd status
  pure out

-- | Capture the stdout of the command strictly. Ignores process exit code.
captureNoThrow :: Cmd -> IO ByteString
captureNoThrow cmd = do
  (status, [h]) <- captureFdsAsHandles [1] cmd
  out <- force <$> B.hGetContents h
  _ <- async . cancel $ status
  pure out

-- | Capture the stderr of the command strictly, err if the command exits with a non-zero exit code.
captureErr :: Cmd -> IO ByteString
captureErr cmd = do
  (status, [h]) <- captureFdsAsHandles [2] cmd
  out <- force <$> B.hGetContents h
  waitCmd status
  pure out

-- | Capture the stderr of the command strictly. Ignores process exit code.
captureErrNoThrow :: Cmd -> IO ByteString
captureErrNoThrow cmd = do
  (status, [h]) <- captureFdsAsHandles [2] cmd
  out <- force <$> B.hGetContents h
  _ <- async . cancel $ status
  pure out
