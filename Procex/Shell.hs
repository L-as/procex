module Procex.Shell (promptFunction, initInteractive) where

import System.IO
import System.Posix.Directory
import System.Posix.Env

promptFunction :: [String] -> Int -> IO String
promptFunction _modules _line = do
  d <- getEnvDefault "PWD" ""
  -- For some ungodly reason, cd-ing inside ghci won't change the cwd of ghci itself,
  -- so completion, etc. will always happen from the directory you started ghci in.
  -- This is a quick hack to work around this by also running changeWorkingDirectory
  -- "above" the shell.
  changeWorkingDirectory d
  -- This is just what the prompt will show inside ghci.
  pure $ d <> ": "

-- | You need to run this if you want stdin to work properly inside ghci.
initInteractive :: IO ()
initInteractive = hSetBuffering stdin LineBuffering
