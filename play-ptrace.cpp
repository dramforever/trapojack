extern "C"
{
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/user.h>
#include <sys/reg.h>
#include <sys/syscall.h>
#include <asm/unistd.h>
#include <errno.h>
}

#include <cstdio>
#include <cstdlib>

using namespace std;

inline bool is_safe_call(const user_regs_struct *regs)
{
  int id = regs->orig_rax;
  
  return
    // read stdin
    (id == __NR_read && regs->rdi == 0)

    // write stdout
    || (id == __NR_write && regs->rdi == 1)

    // brk, memory allocation
    || id == __NR_brk

    // set FS and GS, x64 segmentation
    || id == __NR_arch_prctl

    // anon mmap for allocation, probably for allocation
    || (id == __NR_mmap && (regs->r8 == 0xffffffffULL))

    // stat-ing stdin or stdout
    || (id == __NR_fstat && (regs->rdi == 0 || regs->rdi == 1))

    // Exiting
    || id == __NR_exit
    || id == __NR_exit_group;
}

inline bool should_mask_call(const user_regs_struct *regs)
{
  return regs->orig_rax == __NR_uname || regs->orig_rax == __NR_readlink;
}

inline void filter_syscall(pid_t child)
{
  user_regs_struct regs;
  ptrace(PTRACE_GETREGS, child, NULL, &regs);

  fflush(stderr);
  if(should_mask_call(&regs)) {
    // TODO: Implement masks
    ptrace(PTRACE_SYSCALL, child, 0, 0);
    wait(NULL);
    ptrace(PTRACE_SYSCALL, child, 0, 0);
  } else if(is_safe_call(&regs)) {
    ptrace(PTRACE_SYSCALL, child, 0, 0);
    wait(NULL);
    ptrace(PTRACE_SYSCALL, child, 0, 0);
  } else {
    fprintf(stderr, "[exe]: Unsafe call %llu, killing guest\n",
	    regs.orig_rax);
    kill(child, SIGKILL);
    exit(1);
  }
}

int main(int argc, char *argv[], char *envp[])
{
  if(argc <= 1)
    {
      fputs("Usage: play-ptrace <program> [<args>...]\n", stderr);
      return 1;
    }
  
  pid_t child = vfork();
  
  if(child == 0) {
    ptrace(PTRACE_TRACEME, 0, NULL, NULL);
    execve(argv[1], argv+1, envp);
    _exit(127);
  } else {
    int status;
    wait(&status);
    bool exec_failed = WIFEXITED(status);
    ptrace(PTRACE_SETOPTIONS, child, NULL, PTRACE_O_TRACESYSGOOD);
    ptrace(PTRACE_SYSCALL, child, 0, 0); // Skip over the exec
    wait(&status);
    while(WIFSTOPPED(status)) {
      if(WSTOPSIG(status) == (SIGTRAP | 0x80)) {
	filter_syscall(child);
      } else {
	ptrace(PTRACE_CONT, child, NULL, WSTOPSIG(status));
      }
      wait(&status);
    }

    if(exec_failed) {
      fputs("[exe]: Exec failed in guest\n", stderr);
      return 127;
    } else if (WIFEXITED(status)) {
      if(WEXITSTATUS(status) == 0) {
	fputs("[exe]: Exited successfully\n", stderr);
	return 0;
      } else {
	fprintf(stderr, "[exe]: Exited with failure status %d\n",
		WEXITSTATUS(status));
	return 1;
      }
    } else if (WIFSIGNALED(status)) {
      fprintf(stderr, "[exe]: Killed by signal %d\n",
	      WTERMSIG(status));
      return 1;
    }
  } 
}
