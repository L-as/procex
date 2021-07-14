module Procex.Process (makeCmd, run, pipeArgFd, pipeStrIn, pipeFd, pipeIn, pipeOut, capture) where

import Control.Concurrent.Async
import Control.Exception.Base
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Char (ord)
import Data.Function
import Data.Tuple
import Procex.Core
import System.Exit (ExitCode (..))
import System.IO (hClose, hSetBinaryMode)
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
  pure $ makeCmd' fullpath & arg path & passFd (0, 0) & passFd (1, 1) & passFd (2, 2)

run :: Cmd -> IO ()
run cmd =
  run' cmd >>= wait >>= \case
    Exited ExitSuccess -> pure ()
    e -> throwIO . userError $ "Cmd failed " <> show e

--argpipe :: Cmd -> Cmd -> Cmd
--argpipe = undefined

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

pipeIn :: Cmd -> Cmd -> Cmd
pipeIn = pipeFd True 1 0

pipeOut :: Cmd -> Cmd -> Cmd
pipeOut = pipeFd False 0 1

pipeStrIn :: ByteString -> Cmd -> Cmd
pipeStrIn str cmd = unIOCmd $ do
  bracketOnError createPipe (\(r, w) -> closeFd r >> closeFd w) $ \(r, w) -> do
    w' <- fdToHandle w
    _ <- async $ B.hPut w' str >> hClose w'
    pure . postCmd (const $ closeFd r) $ cmd & passFd (0, r)

capture :: Cmd -> IO ByteString
capture cmd =
  bracketOnError createPipe (\(r, w) -> closeFd r >> closeFd w) $ \(r, w) -> do
    _ <- run' $ cmd & passFd (1, w) -- TODO terminate eventually?
    closeFd w
    r' <- fdToHandle r
    hSetBinaryMode r' True
    B.hGetContents r'
