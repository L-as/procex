[![Hackage](https://img.shields.io/hackage/v/procex.svg?style=flat)](https://hackage.haskell.org/package/procex)

[Hackage documentation](https://hackage.haskell.org/package/procex-0.2.2/docs/) ([unbuilt](https://hackage.haskell.org/package/procex-0.2.2/reports/) as of 2021-07-24...)

[My blog post about using it as a shell](https://las.rs/blog/haskell-as-shell.html) (see `example-shell` directory).

# About

`procex` is a library for launching unix processes, that DOES NOT wrap `createProcess`.
It interfaces directly with `vfork` and `execve`, and closes fds efficiently using the
new `close_range` Linux syscall (or `close` if not available).
The syntax for launching processes is clean, concise, and flexible, mimicking `sh`.

# Differences from shh, turtle, etc.

`procex` can:

- `execve` without `fork`, i.e. replace the current process
- Set fds besides 0, 1, and 2
- Launch a process without taking >0.5s when max fds is high
- Launch processes with an invalid UTF-8 name
- Be extended easily due to the flexible and simple API
- Be used as a shell
- not wrap Haskell functions in a `Cmd` like `shh`, because that is generally not a good solution

# Example

```hs
import Procex.Prelude

main :: IO ()
main = captureLazy (mq "cat" <<< "some stdin") >>= \stdout -> putStrLn (show stdout)
```


# TODO

- Setting the environment (workaround: use `env` from coreutils)
- Better README
- Support for non-Linux kernels
