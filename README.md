# About

This is a Haskell library for launching Unix processes.

# Differences from shh, turtle, etc. etc.

`procex` does not use `createProcess` from `process`, and thus does not suffer from horribly slow process launching,
inability to replace the current process (i.e. `exec` in bash), inability to use fds other than 0, 1, 2.
`procex` interfaces with the syscalls directly (through libc), and uses the new close_range syscall from
Linux 5.9 to efficiently close all fds without looping through 0 to the highest possible fd, which
can take a lot of time, if you can have more than 1024 fds on your system.

The API is also (IMO) simpler and more flexible.

# TODO

- Setting the environment (workaround: use `env` from coreutils)
- `execve` without `vfork` (workaround: use `exec` directly from Procex.Core)
- Better README
