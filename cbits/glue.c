#define _GNU_SOURCE

#include <sys/syscall.h>
#include <linux/close_range.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <sched.h>

extern char **environ;

static int close_range(unsigned int first, unsigned int last, unsigned int flags) {
	return syscall(__NR_close_range, first, last, flags);
}

#define l(msg) write(1, msg, sizeof(msg))

int close_execve(const char *path, char *const argv[], char *const envp[], int fds[], size_t fd_count) {
	if (unshare(CLONE_FILES) == -1) return -1;

	// prevent dependencies within the array
	for (size_t i = 0; i < fd_count; i++) {
		if (fds[i] != -1) {
			int fd = dup(fds[i]);
			if (fd == -1) return -1;
			fds[i] = fd;
		}
	}

	for (int i = 0; i < fd_count; i++) {
		if (fds[i] != -1) {
			if (dup2(fds[i], i) == -1) return -1;
		} else {
			if (close(i) == -1) return -1;
		}
	}

	if (close_range(fd_count, ~0U, 0) == -1) return -1;

	// reset fd limit
	struct rlimit rl;
	if (getrlimit(RLIMIT_NOFILE, &rl) < 0) return -1;
	rl.rlim_cur = FD_SETSIZE;
	if (setrlimit(RLIMIT_NOFILE, &rl) < 0) return -1;

	return execve(path, argv, envp != NULL ? envp : environ);
}

pid_t vfork_close_execve(const char *path, char *const argv[], char *const envp[], int fds[], size_t fd_count) {
	volatile int result = 0;
	// NB: This can not be a normal fork, memory mappings need to be shared.
	pid_t pid = vfork();
	if (pid == -1) {
		return -1;
	} else if (pid == 0) {
		result = close_execve(path, argv, envp, fds, fd_count);
		_exit(1);
	} else {
		if (result != 0) {
			return result;
		} else {
			return pid;
		}
	}
}
