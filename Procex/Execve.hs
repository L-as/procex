module Procex.Execve (Execve, execve, forkexecve) where

import Control.Exception
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Foreign
import Foreign.C.Types
import System.Posix.ByteString
import Prelude

data AdhocError where
  AdhocError :: Show a => a -> AdhocError

instance Show AdhocError where
  showsPrec i (AdhocError x) = showsPrec i x

instance Exception AdhocError

type ExecveRaw =
  Ptr CChar ->
  Ptr (Ptr CChar) ->
  Ptr (Ptr CChar) ->
  Ptr Fd ->
  CSize ->
  IO CPid

type Execve =
  ByteString ->
  [ByteString] ->
  Maybe [ByteString] ->
  [Fd] ->
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

execve :: Execve
execve path args env fds = const Nothing <$> exec' c_close_execve path args env fds

forkexecve :: Execve
forkexecve path args env fds = h <$> exec' c_vfork_close_execve path args env fds
  where
    h :: CPid -> Maybe CPid
    h (CPid (-1)) = Nothing
    h x = Just x
