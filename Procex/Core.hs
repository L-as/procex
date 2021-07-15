module Procex.Core (Cmd, makeCmd', passArg, unIOCmd, postCmd, run', passFd, passArgFd) where

import Control.Concurrent.Async
import Control.Exception.Base
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Foreign.C.Error (throwErrno)
import Procex.Execve
import System.Posix.ByteString

data Arg = ArgStr ByteString | ArgFd Fd deriving (Show)

data Args = Args
  { args :: [Arg],
    fds :: [(Fd, Fd)]
  }
  deriving (Show)

emptyArgs :: Args
emptyArgs = Args {args = [], fds = []}

fdPrepend :: (Fd, Fd) -> Args -> Args
fdPrepend (x, y) args = args {fds = (x, y) : fds args}

argPrepend :: ByteString -> Args -> Args
argPrepend arg Args {..} = Args {args = ArgStr arg : args, ..}

argFdPrepend :: Fd -> Args -> Args
argFdPrepend arg Args {..} = Args {args = ArgFd arg : args, ..}

newtype Cmd = Cmd {unCmd :: Args -> IO (Async ProcessStatus)}

makeCmd' :: ByteString -> Cmd
makeCmd' path = Cmd $ \Args {args, fds} -> do
  let sequentialize_fds :: [(Fd, Fd)] -> S.Seq Fd -> S.Seq Fd
      sequentialize_fds [] out = out
      sequentialize_fds ((new, old) : fds) out =
        let out' = S.update (fromIntegral new) old $ out <> S.replicate (max 0 $ fromIntegral $ fromIntegral new - S.length out + 1) (-1)
         in sequentialize_fds fds out'
  let fds_seq = sequentialize_fds fds []
  let (all_fds, args') =
        foldr
          ( flip $ \(all_fds, args') -> \case
              ArgStr str -> (all_fds, str : args')
              ArgFd old_fd -> let new_fd = S.length all_fds in (all_fds S.|> old_fd, ("/proc/self/fd/" <> B.fromString (show new_fd)) : args')
          )
          (fds_seq, [] :: [ByteString])
          args
  pid <- forkexec path args' Nothing (toList all_fds) -- FIXME there could be an asynchronous exception here
  pid <- case pid of
    Just x -> pure x
    Nothing -> throwErrno $ "Couldn't execute " <> show path <> " with args " <> show args' <> " with the following fds: " <> show all_fds
  async $ do
    status <- getProcessStatus True True pid `onException` signalProcess sigTERM pid
    case status of
      Just status -> pure status
      Nothing -> throwErrno "getProcessStatus returned Nothing"

wrapCmdIO :: (Args -> IO (Async ProcessStatus)) -> Cmd
wrapCmdIO = Cmd

unIOCmd :: IO Cmd -> Cmd
unIOCmd cmd = wrapCmdIO $ \args -> do
  cmd <- cmd
  unCmd cmd args

postCmd :: (Either SomeException (Async ProcessStatus) -> IO ()) -> Cmd -> Cmd
postCmd f cmd = Cmd $ \args -> do
  r <- try (unCmd cmd args)
  f r
  case r of
    Left e -> throwIO e
    Right p -> pure p

run' :: Cmd -> IO (Async ProcessStatus)
run' cmd = unCmd cmd emptyArgs

-- TODO: Execve without fork
-- runReplace :: Cmd -> IO ()

passArg :: ByteString -> Cmd -> Cmd
passArg str cmd = Cmd $ \args -> unCmd cmd $ argPrepend str args

passFd :: (Fd, Fd) -> Cmd -> Cmd
passFd fdpair cmd = Cmd $ \args -> unCmd cmd $ fdPrepend fdpair args

passArgFd :: Fd -> Cmd -> Cmd
passArgFd fd cmd = Cmd $ \args -> unCmd cmd $ argFdPrepend fd args
