[![Hackage](https://img.shields.io/hackage/v/procex.svg?style=flat)](https://hackage.haskell.org/package/procex)

# About

`procex` is a library for launching unix processes, that DOES NOT wrap `createProcess`.
It interfaces directly with `vfork` and `execve`, and closes fds efficiently using the new `close_range` Linux syscall.
The syntax for launching processes is clean, concise, and flexible, mimicking `sh`.

# Differences from shh, turtle, etc.

`procex` can:

- `execve` without `fork`, i.e. replace the current process
- Set fds besides 0, 1, and 2
- Launch a process without taking >0.5s when max fds is high
- Launch processes with an invalid UTF-8 name
- Be extended easily due to the simple API
- Be used as a shell

# Example

```hs
import Procex.Prelude

main :: IO ()
main = mq "someexe" <<< "some stdin" >>> \stdout -> putStrLn (show stdout)
```


# TODO

- Setting the environment (workaround: use `env` from coreutils)
- Better README
- Support for Linuxes older than 5.9
- Support for non-Linux kernels
