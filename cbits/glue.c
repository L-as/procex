#define _GNU_SOURCE

#include <sys/syscall.h>
#include <linux/version.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <sched.h>

static int close_range_fallback(unsigned int first) {
	// There could be fds above FD_SETSIZE, but this might not be a problem
	// because we reset the soft fd limit to FD_SETSIZE (1024) later.
	for (unsigned int i = first; i < FD_SETSIZE; i++) close(i);
	return 0;
}
#if LINUX_VERSION_CODE >= KERNEL_VERSION(5,9,0)
#include <linux/close_range.h>
// glibc does not wrap close_range so we need to do it ourselves.
static int close_range(unsigned int first) {
	int r = syscall(__NR_close_range, first, ~0U, 0);
	// unsupported syscall?
	if (r) {
		return close_range_fallback(first);
	} else {
		return 0;
	}
}
#else
static int close_range(unsigned int first) {
	return close_range_fallback(first);
}
#endif

// This contains the current environment.
extern char **environ;

// This is like vfork_close_execve but replaces the current process.
int close_execve(
	const char *path,
	char *const argv[],
	char *const envp[],
	int fds[],
	size_t fd_count
) {
	// We make sure the file desciptors in the array point to what they're
	// supposed to point to, since if e.g. one pointed to stdin (fd 0),
	// we want it to mean the old stdin, not the new stdin.
	for (size_t i = 0; i < fd_count; i++) {
		if (fds[i] != -1) {
			int fd = dup(fds[i]);
			if (fd == -1) return -1;
			fds[i] = fd;
		}
	}

	// Rename the file descriptors as specified,
	// closing the ones we don't want.
	for (int i = 0; i < fd_count; i++) {
		if (fds[i] != -1) {
			if (dup2(fds[i], i) == -1) return -1;
		} else {
			if (close(i) == -1) return -1;
		}
	}

	// We close all file descriptors that are larger than or equal to fd_count.
	if (close_range(fd_count) == -1) return -1;

	// Reset fd limit for compatibility with select(), see http://0pointer.net/blog/file-descriptor-limits.html.
	struct rlimit rl;
	if (getrlimit(RLIMIT_NOFILE, &rl) < 0) return -1;
	rl.rlim_cur = FD_SETSIZE;
	if (setrlimit(RLIMIT_NOFILE, &rl) < 0) return -1;

	return execve(path, argv, envp != NULL ? envp : environ);
}

// Fork, close file descriptors, then execute.
pid_t vfork_close_execve(
	const char *path, // The path to executable, does not look through PATH
	char *const argv[], // Will be passed verbatim to execve
	// Will be passed verbatim to execve if not NULL, otherwise it will be set to the current environment
	char *const envp[],
	// This is an array that is fd_count long of all file descriptorswe want to share.
	// In the new process, the descriptors will be renamed, fd[i] will be renamed to i using dup2.
	// -1 means it will be closed.
	int fds[],
	size_t fd_count
) {
	// We mark this volatile so it isn't contained in a register.
	// In the child, we can not write to the parent's registers, only its memory.
	// This is used to pass the result code to the parent.
	volatile int result = 0;
	// We fork. If you change this to a standard fork it would break since we
	// wouldn't be able to write to the parent's memory.
	pid_t pid = vfork();
	// vfork had an error.
	if (pid == -1) {
		return -1;
	// We are in the child.
	} else if (pid == 0) {
		result = close_execve(path, argv, envp, fds, fd_count);
		_exit(1);
	// We are in the parent, the child must have finished by now.
	} else {
		// The child had an error.
		if (result != 0) {
			return result;
		// Success! Return the new PID.
		} else {
			return pid;
		}
	}
}
