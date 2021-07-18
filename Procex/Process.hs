module Procex.Process (makeCmd, run, pipeArgIn, pipeArgOut, pipeHIn, pipeHOut, pipeIn, pipeOut, captureFd) where

import Control.Concurrent.Async
import Control.Exception.Base
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Char (ord)
import Data.Function
import Data.Tuple
import Procex.Core
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.Posix.ByteString

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f (x : xs) =
  f x >>= \b -> case b of
    True -> pure $ Just x
    False -> findM f xs
findM _ [] = pure Nothing

-- | A version of 'Procex.Core.makeCmd'' that resolves the path
-- according to PATH and passes through stdin, stdout and stderr (unless overrided).
makeCmd :: ByteString -> Cmd
makeCmd path = unIOCmd $ do
  fullpath :: ByteString <-
    if B.any ((fromIntegral . ord $ '/') ==) path
      then pure path
      else do
        pathvar <- B.fromStrict <$> getEnvDefault "PATH" ""
        fullpath <- findM fileExist . fmap (\x -> B.toStrict $ x <> "/" <> path) . (<> ["/", "."]) . B.split (fromIntegral $ ord ':') $ pathvar
        case fullpath of
          Just p -> pure . B.fromStrict $ p
          Nothing -> throwIO $ userError (show path <> " does not exist")
  pure $ makeCmd' fullpath & passArg path & passFd (0, 0) & passFd (1, 1) & passFd (2, 2)

-- | Thrown when the return code of a command isn't 0.
newtype CmdException = CmdException ProcessStatus deriving Show
instance Exception CmdException where
  displayException (CmdException status) = "Command failed: " <> show status

-- | Runs a command synchronously. See also 'Procex.Core.run''.
-- 'CmdException' will be thrown if the command fails.
run :: Cmd -> IO ()
run cmd =
  run' cmd >>= wait >>= \case
    Exited ExitSuccess -> pure ()
    e -> throwIO (CmdException e)

pipeArgFd :: Bool -> Fd -> Cmd -> Cmd -> Cmd
pipeArgFd dir fd cmd1 cmd2 = unIOCmd $ do
  bracketOnError ((if dir then swap else id) <$> createPipe) (\(x, y) -> closeFd x >> closeFd y) $ \(x, y) -> do
    bracketOnError (run' $ cmd1 & passFd (fd, x)) (async . cancel) $ \status1 -> do
      pure $
        flip postCmd (cmd2 & passArgFd y) $ \status2 -> do
          closeFd x
          closeFd y
          _ <- async $ (either throwIO pure status2 >>= wait) `finally` cancel status1
          pure ()

-- | Pass an argument of the form @\/proc\/self\/fd\/\<n\>@ to the process,
-- where `n` is the reader end of a pipe which the command
-- writes to through the specified fd.
pipeArgIn ::
  -- | The fd the command will write to
  Fd ->
  -- | The command that will write to the fd
  Cmd ->
  -- | The command you're modifying
  Cmd ->
  Cmd
pipeArgIn = pipeArgFd True

-- | Pass an argument of the form @\/proc\/self\/fd\/\<n\>@ to the process,
-- where `n` is the writer end of a pipe which the command
-- reads from through the specified fd.
pipeArgOut ::
  -- | The fd the command will read from
  Fd ->
  -- | The command that will read from the fd
  Cmd ->
  -- | The command you're modifying
  Cmd ->
  Cmd
pipeArgOut = pipeArgFd False

pipeFd :: Bool -> Fd -> Fd -> Cmd -> Cmd -> Cmd
pipeFd dir fd1 fd2 cmd1 cmd2 = unIOCmd $ do
  bracketOnError ((if dir then swap else id) <$> createPipe) (\(x, y) -> closeFd x >> closeFd y) $ \(x, y) -> do
    bracketOnError (run' $ cmd1 & passFd (fd1, x)) (async . cancel) $ \status1 -> do
      pure $
        flip postCmd (cmd2 & passFd (fd2, y)) $ \status2 -> do
          closeFd x
          closeFd y
          _ <- async $ (either throwIO pure status2 >>= wait) `finally` cancel status1
          pure ()

-- | Pipes from the first command to the second command
pipeIn ::
  -- | The writing end
  Fd ->
  -- | The reading end
  Fd ->
  -- | The writer command
  Cmd ->
  -- | The reader command
  Cmd ->
  Cmd
pipeIn = pipeFd True

-- | Pipes from the second command to the first command
pipeOut ::
  -- | The reading end
  Fd ->
  -- | The writing end
  Fd ->
  -- | The reader command
  Cmd ->
  -- | The writer command
  Cmd ->
  Cmd
pipeOut = pipeFd False

pipeH :: Bool -> Fd -> (Async ProcessStatus -> Handle -> IO ()) -> Cmd -> Cmd
pipeH dir fd handler cmd = unIOCmd $
  bracketOnError ((if dir then swap else id) <$> createPipe) (\(x, y) -> closeFd x >> closeFd y) $ \(x, y) -> do
    pure $
      flip postCmd (cmd & passFd (fd, y)) $ \status -> do
        closeFd y
        case status of
          Right status -> do
            x <- fdToHandle x
            _ <- async $ handler status x
            pure ()
          Left e -> do
            closeFd x
            throwIO e

-- | Pipes from the handle to the fd.
pipeHIn :: Fd -> (Async ProcessStatus -> Handle -> IO ()) -> Cmd -> Cmd
pipeHIn = pipeH True

-- | Pipes from the fd to the handle.
pipeHOut :: Fd -> (Async ProcessStatus -> Handle -> IO ()) -> Cmd -> Cmd
pipeHOut = pipeH False

-- | Captures the output to the specified fd
captureFd :: Fd -> Cmd -> IO ByteString
captureFd fd cmd =
  bracketOnError createPipe (\(r, w) -> closeFd r >> closeFd w) $ \(r, w) -> do
    _ <- run' $ cmd & passFd (fd, w) -- TODO terminate eventually?
    closeFd w
    r' <- fdToHandle r
    B.hGetContents r'
