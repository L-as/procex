module Procex.Execve (exec, forkexec) where

import Control.Monad
import Prelude
import Control.Exception

import System.Posix.ByteString

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS

import Foreign
import Foreign.C.Types

data AdhocError where
  AdhocError :: Show a => a -> AdhocError

instance Show AdhocError where
  showsPrec i (AdhocError x) = showsPrec i x

instance Exception AdhocError where

type CloseExecveRaw a =
  Ptr CChar ->
  Ptr (Ptr CChar) ->
  Ptr (Ptr CChar) ->
  Ptr Fd ->
  CSize ->
  IO a
type CloseExecve a =
  ByteString ->
  [ByteString] ->
  Maybe [ByteString] ->
  [Fd] ->
  IO a

foreign import ccall "vfork_close_execve" c_vfork_close_execve :: CloseExecveRaw CPid
foreign import ccall "close_execve" c_close_execve :: CloseExecveRaw Int
--foreign import ccall "execve" c_execve :: Ptr CChar -> Ptr (Ptr CChar) -> Ptr (Ptr CChar) -> IO ()
--foreign import ccall "&environ" c_environ :: Ptr (Ptr CChar)

exec' :: CloseExecveRaw a -> CloseExecve a
exec' f path args env fds = do
  let
    go :: [BS.ByteString] -> ([Ptr CChar] -> IO a) -> IO a
    go [] f = f []
    go (x:xs) f = go xs (\ys -> BS.useAsCString x $ \y -> f (y:ys))
  BS.useAsCString (B.toStrict path) $ \path ->
    go (B.toStrict <$> args) $ \args ->
    withArray0 nullPtr args $ \args ->
    withArrayLen fds $ \fd_count fds ->
    case env of
      Just env ->
        go (B.toStrict <$> env) $ \env ->
        withArray0 nullPtr env $ \env ->
        f path args env fds (CSize . fromIntegral $ fd_count)
      Nothing -> f path args nullPtr fds (CSize . fromIntegral $ fd_count)

exec :: CloseExecve ()
exec path args env fds = exec' c_close_execve path args env fds >>= \i -> if i == -1 then throwIO $ AdhocError ("failed" :: String) else pure ()

forkexec :: CloseExecve (Maybe CPid)
forkexec path args env fds = h <$> exec' c_vfork_close_execve path args env fds where
  h :: CPid -> Maybe CPid
  h (CPid (-1)) = Nothing
  h x = Just x

