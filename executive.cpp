extern "C"
{
#include <asm/unistd.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/resource.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/user.h>
#include <sys/wait.h>
#include <unistd.h>
}

#include <cstdio>
#include <cstdlib>
#include <cstring>

using namespace std;

#if defined(__x86_64__)

typedef long long REG_TYPE;
#define REG_FMT "%lld"

#define SYS_ID orig_rax
#define SYS_ARG_1 rdi
#define SYS_ARG_3 rdx
#define SYS_ARG_4 r10
#define SYS_ARG_5 r8

#elif defined(__i386__)

typedef int REG_TYPE;
#define REG_FMT "%ld"

#define SYS_ID orig_eax
#define SYS_ARG_1 ebx
#define SYS_ARG_3 edx
#define SYS_ARG_4 esi
#define SYS_ARG_5 edi

#else
# error "Unknown architecture"
#endif

inline bool is_safe_call(const user_regs_struct *regs)
{
  int id = regs->SYS_ID;

  return
    // read stdin
    (id == __NR_read && regs->SYS_ARG_1 == 0)

    // write stdout
    || (id == __NR_write && regs->SYS_ARG_1 == 1)

    // brk, memory allocation
    || id == __NR_brk

#if defined(__x86_64__)
    // set FS and GS, x64 segmentation
    || id == __NR_arch_prctl
#endif

    // anon mmap for allocation
    || (id == __NR_mmap
        && regs->SYS_ARG_5 == 0xffffffffU
        && (regs->SYS_ARG_4 & MAP_ANONYMOUS))

    // stat-ing stdin or stdout
    || (id == __NR_fstat && (regs->SYS_ARG_1 == 0 || regs->SYS_ARG_1 == 1))

    // getting the uname
    || id == __NR_uname;
}

inline bool is_exit_call(const user_regs_struct *regs)
{
  return regs->SYS_ID == __NR_exit || regs->SYS_ID == __NR_exit_group;
}

#define TRY(x)                                                               \
  do {                                                                       \
    if((x) == -1) {                                                          \
      fprintf(stderr, "[exe]: Internal error %s\n", strerror(errno));        \
      kill(child, SIGKILL);                                                  \
      asm("int $3"); exit(-1);                                               \
    }                                                                        \
  } while(0)

inline void filter_syscall(pid_t child, REG_TYPE *out_lim)
{
  user_regs_struct regs;
  TRY(ptrace(PTRACE_GETREGS, child, NULL, &regs));

  TRY(fflush(stderr));

  if(regs.SYS_ID == __NR_write && regs.SYS_ARG_1 == 1
     && (*out_lim -= (REG_TYPE)regs.SYS_ARG_3) <= 0) {
    // Write to stdout, OLE
    fputs("[exe]: Output limit exceeded\n", stderr);
    kill(child, SIGKILL);
    exit(1);
  } else if(is_exit_call(&regs)) {
    TRY(ptrace(PTRACE_SYSCALL, child, 0, 0));
  } else if(is_safe_call(&regs)) {
    TRY(ptrace(PTRACE_SYSCALL, child, 0, 0));
    TRY(wait(NULL));
    TRY(ptrace(PTRACE_SYSCALL, child, 0, 0));
  } else {
    fprintf(stderr, "[exe]: Denied call " REG_FMT "\n", regs.SYS_ID);

#if defined(__x86_64__)
    TRY(ptrace(PTRACE_POKEUSER, child, 8 * ORIG_RAX, __NR_getpid));
#elif defined(__i386__)
    TRY(ptrace(PTRACE_POKEUSER, child, 4 * ORIG_EAX, __NR_getpid));
#else
# error "Unknown architecture"
#endif

    TRY(ptrace(PTRACE_SYSCALL, child, 0, 0));
    TRY(wait(NULL));

#if defined(__x86_64__)
    TRY(ptrace(PTRACE_POKEUSER, child, 8 * RAX, -EPERM));
#elif defined(__i386__)
    TRY(ptrace(PTRACE_POKEUSER, child, 4 * EAX, -EPERM));
#else
# error "Unknown architecture"
#endif

    TRY(ptrace(PTRACE_SYSCALL, child, 0, 0));
  }
}

int main(int argc, char *argv[], char *envp[])
{
  if(argc <= 6) {
    fputs("Usage: executive <input> <output> "
          "<time limit> <mem limit> <out limit>"
          "<program> [<args>...]\n", stderr);
    return -1;
  }

  REG_TYPE time_lim = strtol(argv[3], &argv[3], 10);

  if(*argv[3] != '\0') {
    fputs("Time limit must be a number\n", stderr);
    return -1;
  }

  long mem_lim = strtol(argv[4], &argv[4], 10);

  if(*argv[4] != '\0') {
    fputs("Mem limit must be a number\n", stderr);
    return -1;
  }

  REG_TYPE out_lim = strtol(argv[5], &argv[5], 10);

  if(*argv[5] != '\0') {
    fputs("Output limit must be a number\n", stderr);
    return -1;
  }

  pid_t child = vfork();

  if(child == 0) {

#define GTRY(x, ec)                                                          \
    do {                                                                     \
      if((x) == ec) {                                                        \
        fprintf(stderr, "[gst]: Internal error %s\n", strerror(errno));      \
        fflush(stderr);                                                      \
        _exit(-1);                                                           \
      }                                                                      \
    } while(0)

    fprintf(stderr, "[gst]: Guest pid = %d\n", getpid());

    GTRY(freopen(argv[1], "r", stdin), NULL);
    GTRY(freopen(argv[2], "w", stdout), NULL);
    GTRY(ptrace(PTRACE_TRACEME, 0, NULL, NULL), -1);

    rlimit mlm;
    GTRY(getrlimit(RLIMIT_AS, &mlm), -1);
    mlm.rlim_cur = mlm.rlim_max = mem_lim;
    GTRY(setrlimit(RLIMIT_AS, &mlm), -1);

    itimerval tmr;

    GTRY(getitimer(ITIMER_REAL, &tmr), -1);

    tmr.it_value.tv_sec = time_lim / 1000000;
    tmr.it_value.tv_usec = time_lim % 1000000;

    GTRY(setitimer(ITIMER_REAL, &tmr, NULL), -1);

    execve(argv[6], argv+6, envp);

    // execve failed
    _exit(1);
  } else {
    int status;
    wait(&status);
    if(WIFEXITED(status)) {
      fputs("[exe]: Failed to start guest\n", stderr);
      return 127;
    } else {
      TRY(ptrace(PTRACE_SETOPTIONS, child, NULL, PTRACE_O_TRACESYSGOOD));
      TRY(ptrace(PTRACE_SYSCALL, child, 0, 0)); // Skip over the exec
      TRY(wait(&status));
      while(WIFSTOPPED(status)) {
        if(WSTOPSIG(status) == (SIGTRAP | 0x80)) {
          filter_syscall(child, &out_lim);
        } else {
          ptrace(PTRACE_CONT, child, NULL, WSTOPSIG(status));
        }
        wait(&status);
      }

      if (WIFEXITED(status)) {
        if(WEXITSTATUS(status) == 0) {
          fputs("[exe]: Exited successfully\n", stderr);
          return 0;
        } else {
          fprintf(stderr, "[exe]: Exited with failure status %d\n",
                  WEXITSTATUS(status));
          return 1;
        }
      } else if (WIFSIGNALED(status)) {
        if(WTERMSIG(status) == SIGALRM) {
          fputs("[exe]: Time limit exceeded", stderr);
          return 1;
        } else {
          fprintf(stderr, "[exe]: Killed by signal %d\n",
                  WTERMSIG(status));
          return 1;
        }
      }
    }
  }
}
