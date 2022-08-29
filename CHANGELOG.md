# Revision history for procex

## 0.3.3 -- 2022-08-29

- Fix associativities of piping operators

## 0.3.2 -- 2022-04-24

- Fix support for newer glibcs that already define `close_range`
- Fall back to primitive `close_range` if syscall fails

## 0.3.1 -- 2021-08-24

- Added `passNoFd` and some `QuickCmdArg` instances
