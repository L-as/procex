module Procex.Core (Cmd, makeCmd', passArg, unIOCmd, postCmd, run', runReplace, passFd, passArgFd) where

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
    fds :: [(Fd, Fd)],
    executor :: Execve
  }

emptyArgs :: Args
emptyArgs = Args {args = [], fds = [], executor = forkexecve}

fdPrepend :: (Fd, Fd) -> Args -> Args
fdPrepend (x, y) args = args {fds = (x, y) : fds args}

argPrepend :: ByteString -> Args -> Args
argPrepend arg Args {..} = Args {args = ArgStr arg : args, ..}

argFdPrepend :: Fd -> Args -> Args
argFdPrepend arg Args {..} = Args {args = ArgFd arg : args, ..}

-- | A command. You can execute this with 'run'' or 'Procex.Process.run'.
newtype Cmd = Cmd {unCmd :: Args -> IO (Async ProcessStatus)}

-- | Make a 'Cmd' from the path to an executable. Does not take PATH into account.
-- See 'Procex.Process.makeCmd' for a version that provides
-- some sensible defaults, like forwarding stdin, stdout, stderr.
makeCmd' :: ByteString -> Cmd
makeCmd' path = Cmd $ \Args {args, fds, executor} -> do
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
  pid <- executor path args' Nothing (toList all_fds) -- FIXME there could be an asynchronous exception here
  pid <- case pid of
    Just x -> pure x
    Nothing -> throwErrno $ "Couldn't execute " <> show path <> " with args " <> show args' <> " with the following fds: " <> show all_fds
  async $ do
    status <- getProcessStatus True True pid `onException` signalProcess sigTERM pid
    case status of
      Just status -> pure status
      Nothing -> throwErrno "getProcessStatus returned Nothing"

-- | Embeds the IO action inside the command, such that the IO action
-- is executed when the command is executed.
unIOCmd :: IO Cmd -> Cmd
unIOCmd cmd = Cmd $ \args -> do
  cmd <- cmd
  unCmd cmd args

-- | Executes some code after launching the process. If launching the process
-- fails, it will be provided with the exception it failed with.
postCmd :: (Either SomeException (Async ProcessStatus) -> IO ()) -> Cmd -> Cmd
postCmd f cmd = Cmd $ \args -> do
  r <- try (unCmd cmd args)
  f r
  case r of
    Left e -> throwIO e
    Right p -> pure p

-- | Runs the specified command asynchronously and returns
-- the process status.
run' :: Cmd -> IO (Async ProcessStatus)
run' cmd = unCmd cmd emptyArgs

-- | Runs the specified commands and replaces the current process with it.
-- This will not return unless an error occurs while executing the process.
runReplace :: Cmd -> IO ()
runReplace cmd = const () <$> unCmd cmd emptyArgs {executor = execve}

-- | Pass an argument to the command.
passArg :: ByteString -> Cmd -> Cmd
passArg str cmd = Cmd $ \args -> unCmd cmd $ argPrepend str args

-- | Bind a fd in the new process to a fd available now.
-- If you try to bind an fd already bound, it will simply replace the older binding.
passFd ::
  -- | (new, old)
  (Fd, Fd) ->
  Cmd ->
  Cmd
passFd fdpair cmd = Cmd $ \args -> unCmd cmd $ fdPrepend fdpair args

-- | Pass an argument of the form @\/proc\/self\/fd\/\<n\>@ to the process,
-- where `n` is an fd which is a duplicate of the fd provided here.
passArgFd :: Fd -> Cmd -> Cmd
passArgFd fd cmd = Cmd $ \args -> unCmd cmd $ argFdPrepend fd args
