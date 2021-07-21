-- You will need this
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- You might not need this
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

import "procex" Procex.Prelude
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel)
import Control.Exception

main :: IO ()
main = do
  mq "echo" "teststring" >>> \out -> (putStrLn . show $ out) >> threadDelay (1000 * 1000 * 3) >> fail "test error" --putStrLn . show $ out == "teststrinssg\n"
  --mq "echo" "teststring" (postCmd $ \_ -> fail "hello error")
  --mq "sleep" "10"
  --status <- run' $ mq "sh" "-c" "while sleep 1; do echo hello; done"
  threadDelay (1000 * 1000 * 5)
  --cancel status
  --threadDelay (1000 * 1000 * 10)
  --mq "diff" (pipeArgStrIn "ab\ncd") (pipeArgStrIn "ab\ncd")
  --mq "diff" (pipeArgStrIn "ab\ncd") (pipeArgStrIn "ab\nce")
