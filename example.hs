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

main :: IO ()
main = do
  mq "diff" (pipeArgStrIn "ab\ncd") (pipeArgStrIn "ab\ncd")
  _ <- wait <=< run' $ mq "diff" (pipeArgStrIn "ab\ncd") (pipeArgStrIn "ab\nce")
  try (mq "false") >>= \case
    Left (CmdException (Exited (ExitFailure 1))) -> pure ()
    x -> fail $ "unreachable: " <> show x
