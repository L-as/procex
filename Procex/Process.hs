module Procex.Process (makeCmd, run, pipeArgIn, pipeArgOut, pipeHIn, pipeHOut, pipeIn, pipeOut, capture) where

import Control.Concurrent.Async
import Control.Exception.Base
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Char (ord)
import Data.Function
import Data.Tuple
import Procex.Core
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hSetBinaryMode)
import System.Posix.ByteString

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM f (x : xs) = f x >>= \b -> case b of
  True -> pure $ Just x
  False -> findM f xs
findM _ [] = pure Nothing

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

run :: Cmd -> IO ()
run cmd =
  run' cmd >>= wait >>= \case
    Exited ExitSuccess -> pure ()
    e -> throwIO . userError $ "Cmd failed " <> show e

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

pipeArgIn :: Fd -> Cmd -> Cmd -> Cmd
pipeArgIn = pipeArgFd True

pipeArgOut :: Fd -> Cmd -> Cmd -> Cmd
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

pipeIn :: Fd -> Fd -> Cmd -> Cmd -> Cmd
pipeIn = pipeFd True

pipeOut :: Fd -> Fd -> Cmd -> Cmd -> Cmd
pipeOut = pipeFd False

pipeH :: Bool -> Fd -> (Async ProcessStatus -> Handle -> IO ()) -> Cmd -> Cmd
pipeH dir fd handler cmd = unIOCmd $
  bracketOnError ((if dir then swap else id) <$> createPipe) (\(x, y) -> closeFd x >> closeFd y) $ \(x, y) -> do
    pure $ flip postCmd (cmd & passFd (fd, y)) $ \status -> do
      closeFd y
      case status of
        Right status -> do
          x <- fdToHandle x
          _ <- async $ handler status x
          pure ()
        Left e -> do
          closeFd x
          throwIO e

pipeHIn :: Fd -> (Async ProcessStatus -> Handle -> IO ()) -> Cmd -> Cmd
pipeHIn = pipeH True

pipeHOut :: Fd -> (Async ProcessStatus -> Handle -> IO ()) -> Cmd -> Cmd
pipeHOut = pipeH False

capture :: Cmd -> IO ByteString
capture cmd =
  bracketOnError createPipe (\(r, w) -> closeFd r >> closeFd w) $ \(r, w) -> do
    _ <- run' $ cmd & passFd (1, w) -- TODO terminate eventually?
    closeFd w
    r' <- fdToHandle r
    hSetBinaryMode r' True
    B.hGetContents r'
