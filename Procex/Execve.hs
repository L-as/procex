-- | Contains FFI bindings to the C bits
module Procex.Execve (Execve, execve, forkexecve) where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Foreign
import Foreign.C.Types
import System.Posix.ByteString
import Prelude

type ExecveRaw =
  Ptr CChar ->
  Ptr (Ptr CChar) ->
  Ptr (Ptr CChar) ->
  Ptr Fd ->
  CSize ->
  IO CPid

-- | The signature for 'execve' and 'forkexecve'.
type Execve =
  -- | The full path to the executable.
  ByteString ->
  -- | The args to pass, including argv[0].
  [ByteString] ->
  -- | The environment to pass. Will default to the current environment if 'Nothing' is passed.
  Maybe [ByteString] ->
  -- | The fds to pass. All other fds will be closed. In the new process, the integral id for each fd will be
  -- set to the position the fd has in this list, e.g. the first element in this list will be stdin, and so on.
  [Fd] ->
  -- | The process id for the new process.
  IO (Maybe CPid)

foreign import ccall "vfork_close_execve" c_vfork_close_execve :: ExecveRaw

foreign import ccall "close_execve" c_close_execve :: ExecveRaw

--foreign import ccall "execve" c_execve :: Ptr CChar -> Ptr (Ptr CChar) -> Ptr (Ptr CChar) -> IO ()
--foreign import ccall "&environ" c_environ :: Ptr (Ptr CChar)

exec' :: ExecveRaw -> ByteString -> [ByteString] -> Maybe [ByteString] -> [Fd] -> IO CPid
exec' f path args env fds = do
  let go :: [BS.ByteString] -> ([Ptr CChar] -> IO a) -> IO a
      go [] f = f []
      go (x : xs) f = go xs (\ys -> BS.useAsCString x $ \y -> f (y : ys))
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

-- | Replace the current process with a new process.
execve :: Execve
execve path args env fds = const Nothing <$> exec' c_close_execve path args env fds

-- | Fork and execute a new process.
forkexecve :: Execve
forkexecve path args env fds = h <$> exec' c_vfork_close_execve path args env fds
  where
    h :: CPid -> Maybe CPid
    h (CPid (-1)) = Nothing
    h x = Just x
