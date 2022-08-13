---
title: "Fork System call"
slug: "fork-system-call"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## fork() system call
`fork()` is a system call. fork is used to create a child process from the running process, which is a replica of the parent process(Process which executed `fork()` ). Child process is derived from the parent process. Both the parent and child have different address space, each is independent of the changes made to the variables. 

The child process has its own PID(process identification). PPID(Parent Process ID) of child process is same as PID of parent process.

> Format:
> 
> Header file          : `#include <unistd.h>`  
> Function Declaration : `pid_t fork(void);`

fork() doesn't need any input arguments.

On successful creation of child process the pid of the child process is returned to the parent process and 0 is returned in the child process. On Failure return `-1` with no process created.

Usage example:

    #include <stdio.h>
    #include <unistd.h>

    void child_process();
    void parent_process();

    int main()
    {
        pid_t pid;
        pid=fork();
        if(pid==0)
            child_process();
        else
            parent_process();
        return 0;
    }
    
    /*getpid() will return the Pid of the 
      current process executing the function */

    void child_process()
    {
        printf("Child process with PID : %d  and PPID : %d ", getpid(),getppid());    
    }
    
    void parent_process()
    { 
        printf("Parent process with PID : %d", getpid());    
    }

The sequence of the `printf` statements from the child and parent depend on the scheduling mechanism which purely depends on the system.

