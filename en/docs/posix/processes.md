---
title: "Processes"
slug: "processes"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
- pid_t getpid(void);
- pid_t getppid(void);
- pid_t fork(void);
- pid_t waitpid(pid_t pid, int *wstatus, int options);
- int execv(const char *path, char *const argv[]);

## Parameters
| Function, Parameter(s), Return Value | Description |
| --- | --- | 
| `fork()` | function name |
| none | n/a |
| Returns PID, 0, or -1| The calling process receives the PID of the newly create process or -1 on failure. The child (the newly created process) receive 0. In case of failure set `errno` to either `EAGAIN` or `ENOMEM`|
| - | - |
| `execv()` | function name |
| `const char *path`  | String containing name of to executable (might inlcude path) |
| `char *const argv[]`  | Array of string pointer as arguments |
| Returns -1 on failure | On success  this function does not return. |
| - | - |



## Create child process and wait until it exits
This program demonstrates how to run another process using [`fork()`][1] and wait its termination using [`waitpid()`][2]:

* `fork()` creates an identical copy of the current process. The original process is the parent process, while the newly created one is the child process. Both processes continue exactly after the `fork()`.

* `waitpid()` blocks until child process exits or terminates and returns its exit code and termination reason.

<!-- language: c -->

```
#include <unistd.h>     /* for fork(), getpid() */
#include <sys/types.h>  /* for waitpid() */
#include <sys/wait.h>   /* for waitpid() */
#include <stdlib.h>     /* for exit() */
#include <stdio.h>      /* for printf(), perror() */

int
main(int argc, char *argv[])
{
    /* Create child process.
     *
     * On success, fork() returns the process ID of the child (> 0) to the
     * parent and 0 to the child. On error, -1 is returned.
     */
    pid_t child_pid = fork();

    if (child_pid < 0) {
        perror("fork() failed");
        exit(EXIT_FAILURE);
    } else if (child_pid == 0) {
        /* Print message from child process.
         *
         * getpid() returns the PID (process identifier) of current process,
         * which is typically int but doesn't have to be, so we cast it.
         *
         * getppid() returns the PID of the parent process.
         */
        printf("from child: pid=%d, parent_pid=%d\n",
                 (int)getpid(), (int)getppid());

        /* We can do something here, e.g. load another program using exec().
         */
        exit(33);
    } else if (child_pid > 0) {
        /* Print message from parent process.
         */
        printf("from parent: pid=%d child_pid=%d\n",
                  (int)getpid(), (int)child_pid); 
        
        /* Wait until child process exits or terminates.
         *
         * The return value of waitpid() is PID of the child process, while
         * its argument is filled with exit code and termination reason.
         */
        int status;
        pid_t waited_pid = waitpid(child_pid, &status, 0);

        if (waited_pid < 0) {
            perror("waitpid() failed");
            exit(EXIT_FAILURE);
        } else if (waited_pid == child_pid) {
            if (WIFEXITED(status)) {
                /* WIFEXITED(status) returns true if the child has terminated 
                 * normally. In this case WEXITSTATUS(status) returns child's
                 * exit code.
                 */
                printf("from parent: child exited with code %d\n",
                          WEXITSTATUS(status));
            }
        }
    }

    exit(EXIT_SUCCESS);
}
```

----

Example output:

```
from parent: pid=2486 child_pid=2487
from child: pid=2487, parent_pid=2486
from parent: child exited with code 33
```

-----

Copied from [here][3], originally created by M.Geiger.


  [1]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/fork.html
  [2]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/waitpid.html
  [3]: https://www.wikiod.com/c

