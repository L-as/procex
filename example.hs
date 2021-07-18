-- You will likely need this
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

-- End-users don't need this
{-# LANGUAGE PackageImports #-}

import "procex" Procex.Prelude
import Control.Exception

main :: IO ()
main = do
  mq "echo" "teststring" >>> \out -> assert (out == "teststring") $ pure ()
