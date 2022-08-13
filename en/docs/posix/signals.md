---
title: "Signals"
slug: "signals"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
- unsigned alarm(unsigned seconds);
- int kill(pid_t pid, int sig);

## Parameters
| Function, Parameter(s), Return Value | Description |
| --- | --- | 
| `alarm()` | function name |
| `unsigned seconds`| Seconds to raise an alarm or 0 to cancel any pending alarm |
| \>= 0| 0 if no other alarm was pending, else the number of seconds the pending alarm still had open. This function won't fail. |
| - | - |
| `kill()`  | function name |
| `pid_t pid`  | . |
| ` int sig`  | 0 or signal ID |
| 0, -1 | On success 0 is returned, -1 on failure with setting `errno` to `EINVAL`, `EPERM`or `ESRCH`.

## Raising SIGALARM with the default action
Using `alarm`, user can schedule `SIGALARM` signal to be raised after specified interval. In case user did not blocked, ignored or specified explicit signal handler for this signal, the default action for this signal will be performed on arrival. Per [specification][1] default action for `SIGALARM ` is to terminate the process:

<!-- language: lang-c -->
    #include <unistd.h>
    #include <stdio.h>
    #include <stdlib.h>
    
    int main (int argc, char** argv)
    {
        printf("Hello!\n");
    
        // Set the alarm for five second
        alarm(5); // Per POSIX, this cannot fail
    
        // Now sleep for 15 seconds
        for (int i = 1; i <= 15; i++)
        {
            printf("%d\n", i);
            sleep(1);
        }
    
        // And print the message before successful exit
        printf("Goodbye!\n");
    
        return EXIT_SUCCESS;
    }


This outputs:

```
Hello!
1
2
3
4
5
[2]    35086 alarm      ./a.out
```


  [1]: http://man7.org/linux/man-pages/man7/signal.7.html "Specification"

## Setting signal handler using sigaction and raising signals using raise
In order for a program to react to a certain signal, other than using default action, custom signal handler can be installed using `sigaction`. `sigaction` receives three arguments - signal to act on, pointer to `sigaction_t` structure which, if not `NULL`, is describing new behaviour and pointer to `sigaction_t` which, if not `NULL` will be filled with the old behaviour (so one can restore it). Raising signals in same process can be done with `raise` method. If more control is needed (to send the signal to some other process, `kill` or `pthread_kill` can be used, which accept the destination process id or thread id).

<!-- language: lang-c -->
    #include <unistd.h>
    #include <signal.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    
    // Signals are numbered from 1, signal 0 doesn't exist
    volatile sig_atomic_t last_received_signal = 0;
    
    // Signal handler, will set the global variable
    // to indicate what is the last signal received.
    // There should be as less work as possible inside
    // signal handler routine, and one must take care only
    // to call reentrant functions (in case of signal arriving
    // while program is already executing same function)
    void signal_catcher(int signo, siginfo_t *info, void *context)
    {
        last_received_signal = info->si_signo;
    }
    
    int main (int argc, char** argv)
    {
        // Setup a signal handler for SIGUSR1 and SIGUSR2
        struct sigaction act;
        memset(&act, 0, sizeof act);
    
        // sigact structure holding old configuration
        // (will be filled by sigaction):
        struct sigaction old_1;
        memset(&old_1, 0, sizeof old_1);
        struct sigaction old_2;
        memset(&old_2, 0, sizeof old_2);
    
        act.sa_sigaction = signal_catcher;
        // When passing sa_sigaction, SA_SIGINFO flag
        // must be specified. Otherwise, function pointed
        // by act.sa_handler will be invoked
        act.sa_flags = SA_SIGINFO;
    
        if (0 != sigaction(SIGUSR1, &act, &old_1))
        {
            perror("sigaction () failed installing SIGUSR1 handler");
            return EXIT_FAILURE;
        }
    
        if (0 != sigaction(SIGUSR2, &act, &old_2))
        {
            perror("sigaction() failed installing SIGUSR2 handler");
            return EXIT_FAILURE;
        }
    
        // Main body of "work" during which two signals
        // will be raised, after 5 and 10 seconds, and which
        // will print last received signal
        for (int i = 1; i <= 15; i++)
        {
            if (i == 5)
            {
                if (0 != raise(SIGUSR1))
                {
                    perror("Can't raise SIGUSR1");
                    return EXIT_FAILURE;
                }
            }
    
            if (i == 10)
            {
                if (0 != raise(SIGUSR2))
                {
                    perror("Can't raise SIGUSR2");
                    return EXIT_FAILURE;
                }
            }
    
            printf("Tick #%d, last caught signal: %d\n",
                i, last_received_signal);
    
            sleep(1);
        }
    
        // Restore old signal handlers
        if (0 != sigaction(SIGUSR1, &old_1, NULL))
        {
            perror("sigaction() failed restoring SIGUSR1 handler");
            return EXIT_FAILURE;
        }
    
        if (0 != sigaction(SIGUSR2, &old_2, NULL))
        {
            perror("sigaction() failed restoring SIGUSR2 handler");
            return EXIT_FAILURE;
        }
    
        return EXIT_SUCCESS;
    }

This outputs:

```
Tick #1, last caught signal: 0
Tick #2, last caught signal: 0
Tick #3, last caught signal: 0
Tick #4, last caught signal: 0
Tick #5, last caught signal: 30
Tick #6, last caught signal: 30
Tick #7, last caught signal: 30
Tick #8, last caught signal: 30
Tick #9, last caught signal: 30
Tick #10, last caught signal: 31
Tick #11, last caught signal: 31
Tick #12, last caught signal: 31
Tick #13, last caught signal: 31
Tick #14, last caught signal: 31
Tick #15, last caught signal: 31

```

## A process committing suicide using kill()
A process can (try to) send a signal to any other process using the `kill()` function.

To do so, the sending process needs to known the receiving process' PID. As, without introducing a race, a process can only be sure of its own PID (and the PIDs of its children) the most simple example to demonstrate the usage of `kill()` is to have a process send a signal to itself.

Below an example of a process initiating its own termination by sending itself a kill-signal (`SIGKILL`):

<!-- language: lang-c -->
    #define _POSIX_C_SOURCE 1
 
    #include <sys/types.h>
    #include <unistd.h>
    #include <signal.h>
    #include <stdio.h>


    int main (void)
    {
      pid_t pid = getpid(); /* Get my iown process ID. */
  
      kill(pid, SIGKILL); /* Send myself a KILL signal. */

      puts("Signal delivery initiated.");  /* Although not guaranteed, 
                                      practically the program never gets here. */

      pause(); /* Wait to die. */

      puts("This never gets printed.");
    }

Output:

    Killed

(... or alike, depending on the implementation)

## Handle SIGPIPE generated by write() in a thread-safe manner
When [`write()`][1] is called for a named or unnamed pipe or stream socket whose reading end is closed, two things happen:

<!-- if version [lte POSIX.1-2001] -->
1. `SIGPIPE` signal is sent to the *process* that called `write()`
<!-- end version if -->

<!-- if version [gte POSIX.1-2004] -->
1. `SIGPIPE` signal is sent to the *thread* that called `write()`
<!-- end version if -->

2. `EPIPE` error is returned by `write()`

----

There are several ways to deal with `SIGPIPE`:

* For sockets, `SIGPIPE` may be disabled by setting platform-specific options like `MSG_NOSIGNAL` in Linux and `SO_NOSIGPIPE` in BSD (works only for `send`, but not for `write`). This is not portable.


* For FIFOs (named pipes), `SIGPIPE` will not be generated if writer uses `O_RDWR` instead of `O_WRONLY`, so that reading end is always opened. However, this disables `EPIPE` too.


* We can ignore `SIGPIPE` or set global handler. This is a good solution, but it's not acceptable if you don't control the whole application (e.g. you're writing a library).


* With recent POSIX versions, we can use the fact that `SIGPIPE` is send to the thread that called `write()` and handle it using synchronous signal handling technique.

----

Code below demonstrates thread-safe `SIGPIPE` handling for POSIX.1-2004 and later. 

It's inspired by [this post][2]:

* First, add `SIGPIPE` to signal mask of current thread using [`pthread_sigmask()`][3].
* Check if there is already pending `SIGPIPE` using [`sigpending()`][4].
* Call `write()`. If reading end is closed, `SIGPIPE` will be added to pending signals mask and `EPIPE` will be returned.
* If `write()` returned `EPIPE`, and `SIGPIPE` was not already pending before `write()`, remove it from pending signals mask using [`sigtimedwait()`][5].
* Restore original signal mask using `pthread_sigmask()`.

Source code:

<!-- language: c -->
```
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <sys/signal.h>

ssize_t safe_write(int fd, const void* buf, size_t bufsz)
{
    sigset_t sig_block, sig_restore, sig_pending;

    sigemptyset(&sig_block);
    sigaddset(&sig_block, SIGPIPE);

    /* Block SIGPIPE for this thread.
     *
     * This works since kernel sends SIGPIPE to the thread that called write(),
     * not to the whole process.
     */
    if (pthread_sigmask(SIG_BLOCK, &sig_block, &sig_restore) != 0) {
        return -1;
    }

    /* Check if SIGPIPE is already pending.
     */
    int sigpipe_pending = -1;
    if (sigpending(&sig_pending) != -1) {
        sigpipe_pending = sigismember(&sig_pending, SIGPIPE);
    }

    if (sigpipe_pending == -1) {
        pthread_sigmask(SIG_SETMASK, &sig_restore, NULL);
        return -1;
    }

    ssize_t ret;
    while ((ret = write(fd, buf, bufsz)) == -1) {
        if (errno != EINTR)
            break;
    }

    /* Fetch generated SIGPIPE if write() failed with EPIPE.
     *
     * However, if SIGPIPE was already pending before calling write(), it was
     * also generated and blocked by caller, and caller may expect that it can
     * fetch it later. Since signals are not queued, we don't fetch it in this
     * case.
     */
    if (ret == -1 && errno == EPIPE && sigpipe_pending == 0) {
        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 0;

        int sig;
        while ((sig = sigtimedwait(&sig_block, 0, &ts)) == -1) {
            if (errno != EINTR)
                break;
        }
    }

    pthread_sigmask(SIG_SETMASK, &sig_restore, NULL);
    return ret;
}
```


  [1]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/write.html
  [2]: http://www.microhowto.info/howto/ignore_sigpipe_without_affecting_other_threads_in_a_process.html
  [3]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/pthread_sigmask.html
  [4]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/sigpending.html
  [5]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/sigtimedwait.html

