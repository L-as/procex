# About

This is a Haskell library for launching Unix processes.

# Differences from shh, turtle, etc. etc.

`procex` can:

- `execve` without `fork`, i.e. replace the current process
- Set fds besides 0, 1, and 2
- Launch a process without taking >0.5s when max fds is high
- Launch processes with an invalid UTF-8 name
- Simple and extensible API

# TODO

- Setting the environment (workaround: use `env` from coreutils)
- Better README
