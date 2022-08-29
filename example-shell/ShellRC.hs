{-# LANGUAGE NoMonomorphismRestriction #-}
-- Only needed if you're using this for a shell
{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures #-}

module ShellRC where

-- All the imports are also made available to the shell.
-- They're copied verbatim into the init script.
import Procex.Prelude
import Procex.Shell hiding (promptFunction)

-- Useful imports

-- from `replace-megaparsec` package
import Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BU
import Replace.Megaparsec
import System.Directory
import System.Environment

-- Our prompt
promptFunction :: [String] -> Int -> IO String
promptFunction _modules _line = do
  d <- getEnv "PWD"
  -- If you don't do this cd-ing from within the shell won't work...
  setCurrentDirectory d
  pure $ d <> ": "

-- Run when your shell starts
_init :: IO ()
_init = do
  initInteractive
  getEnv "REALHOME" >>= setEnv "HOME" -- Set by the script that launches GHCi

-- `nixos-rebuild switch --flake flake` alternative
rebuildSystem :: String -> IO ()
rebuildSystem flake = do
  out <- ((<> "/result") . B.init) <$> (capture $ mq "mktemp" "-d")
  hostname <- B.init <$> (capture $ mq "hostname")
  mq "nix" "build" "-L" "--no-write-lock-file" (BU.fromString flake <> "#nixosConfigurations." <> hostname <> ".config.system.build.toplevel") "-o" out
  mq "sudo" "nix-env" "-p" "/nix/var/nix/profiles/system" "--set" out
  mq "sudo" (out <> "/bin/switch-to-configuration") "switch"

diff :: ByteString -> ByteString -> IO ByteString
diff x y = captureLazyNoThrow $ mq "diff" (pipeArgStrIn x) (pipeArgStrIn y)
