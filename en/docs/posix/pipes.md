---
title: "Pipes"
slug: "pipes"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Pipes are a mechanism for unidirectional interprocess or interthread communication within the scope of a single machine.  Logically, a pipe consists of two connected termini, one to which data can be written, and another from which that data can subsequently be read, with a data buffer between such that writes and reads are not required to be synchronous.  Pipes should be distinguished from shell *pipelines*, which are an application of pipes.

## Basic creation and usage
Anonymous pipes, or simply pipes, are kernel-managed objects exposed to processes as a pair of file descriptors, one for the read terminus and one for the write terminus.  They are created via the `pipe`(2) function:

    int pipefds[2];
    int result;

    result = pipe(pipefds);

On success, `pipe()` records the descriptor for the read end of the pipe at index 0 of the provided array, and the descriptor for the write end at index 1; these indices are analogous to the conventional file descriptor numbers for the standard streams.

Having created a pipe, one uses POSIX I/O functions to write to the write end or read from the read end:

*Process 1:*

    ssize_t bytes_written = write(pipefds[1], "Hello, World!", 14);

*Process 2:*

    char buffer[256];
    ssize_t bytes_read = read(pipefds[0], buffer, sizeof(buffer));


Alternatively, one can instead use `fdopen()` to wrap one of both pipe ends in a `FILE` structure for use with C stdio functions:

    FILE *write_end = fdopen(pipefds[1]);

    if (write_end) {
        fputs("Hello, World!");
    }

Pipes have finite I/O buffers, and ordinary writes to pipes with full buffers will block.  Pipes are therefore not a safe mechanism for a thread to communicate with itself, as if the thread writing to a pipe is also the only one that reads from it, then as soon as a write blocks that thread is deadlocked.

A pipe persists as long as any process has an open file description for either of the pipe ends.  The `close`(2) function can be used to close a pipe end represented by a file descriptor, and `fclose`(3) can be used to close a pipe end via a `FILE` wrapped around it.


## Establishing a pipe to a child process
File descriptors and `FILE` objects are per-process resources that cannot themselves be exchanged between processes via ordinary I/O.  Therefore, in order for two distinct processes to communicate via an anonymous pipe, one or both participating processes must inherit an open pipe end from the process that created the pipe, which must therefore be the parent process or a more distant ancestor.  The simplest case is the one in which a parent process wants to communicate with a child process.

Because the child process must inherit the needed open file description from its parent, the pipe must be created first.  The parent then forks.  Ordinarily, each process will be either strictly a reader or strictly a writer; in that case, each one should close the pipe end that it does not intend to use.

    void demo() {
        int pipefds[2];
        pid_t pid;
    
        // Create the pipe
        if (pipe(pipefds)) {
            // error - abort ...
        }
    
        switch (pid = fork()) {
            case -1:
                // error - abort ...
                break;
            case 0:   /* child */
                close(pipefds[0]);
                write(pipefds[1], "Goodbye, and thanks for all the fish!", 37);
                exit(0);
            default:  /* parent */
                close(pipefds[1]);

                char buffer[256];
                ssize_t nread = read(pipefds[0], sizeof(buffer) - 1);

                if (nread >= 0) {
                    buffer[nread] = '\0';
                    printf("My child said '%s'\n", buffer);
                }

                // collect the child
                wait(NULL);

                break;
        }
    }

## Connecting two child processes via a pipe
Connecting two child processes via a pipe is performed by connecting each of two children to the parent via different ends of the same pipe.  Usually, the parent will not be party to the conversation between the children, so it closes its copies of both pipe ends.

    int demo() {
        int pipefds[2];
        pid_t child1, child2;

        if (pipe(pipefds)) {
            // error - abort ...
        }

        switch (child1 = fork()) {
            case -1:
                // error - abort
                break;
            case 0:   /* child 1 */
                close(pipefds[0]);
                write(pipefds[1], "Hello, brother!", 15);
                exit(0);
            default:  /* parent */
                // nothing
        }

        switch (child1 = fork()) {
            case -1:
                // error - abort
                break;
            case 0:   /* child 2 */
                char buffer[256];
                ssize_t nread;

                close(pipefds[1]);
                nread = read(pipefds[0], buffer, sizeof(buffer) - 1);
                if (nread < 0) {
                    // handle error
                } else {
                    buffer[nread] = '\0';
                    printf("My brother told me '%s'\n", buffer);
                }
                exit(0);
            default:  /* parent */
                // nothing
        }

        // Only the parent reaches this point
        close(pipefds[0]);
        close(pipefds[1]);
        if (child1 >= 0) {
            wait(NULL);
            if (child2 >= 0) {
                wait(NULL);
            }
        }
    }



## Creating a shell-style pipeline
A shell-style pipeline consists of two or more processes, each one with its standard output connected to the standard input of the next.  The output-to-input connections are built on pipes.  To establish a pipeline-like set of processes, one creates pipe-connected child processes as described in another example, and furthermore uses the `dup2`(2) function to duplicate each pipe end onto the appropriate standard file descriptor of its process.  It is generally a good idea to then close the original pipe-end file descriptor, especially if, as is often the case, one intends for the child to afterward exec a different command.

    // Most error handling omitted for brevity, including ensuring that pipe ends are
    // always closed and child processes are always collected as needed; see other
    // examples for more detail.
    int demo() {
        int pipefds[2];
        pid_t child1 = -1, child2 = -1;
    
        pipe(pipefds);
    
        switch (child1 = fork()) {
            case -1:
                // handle error ...
                break;
            case 0:   /* child 1 */
                close(pipefds[0]);
                dup2(pipefds[1], STDOUT_FILENO);
                close(pipefds[1]);
                execl("/bin/cat", "cat", "/etc/motd", NULL);
                exit(1);  // execl() returns only on error
            default:  /* parent */
                // nothing
        }
    
        switch (child2 = fork()) {
            case -1:
                // handle error ...
                break;
            case 0:   /* child 2 */
                close(pipefds[1]);
                dup2(pipefds[0], STDIN_FILENO);
                close(pipefds[0]);
                execl("/bin/grep", "grep", "[Ww]ombat", NULL);
                exit(1);  // execl() returns only on error
            default:  /* parent */
                // nothing
        }
    
        // Only the parent reaches this point
        close(pipefds[0]);
        close(pipefds[1]);
        wait(NULL);
        wait(NULL);
    }

One of the more common errors in setting up a pipeline is to forget to have one or more of the processes involved close the pipe ends that it is not using.  As a rule of thumb, the end result should be that each pipe end is owned by exactly one process, and is open only in that process.

In a case such as the demo function above, failure the second child or of the parent to close `pipefds[1]` will result in the second child hanging, for `grep` will continue to wait for input until it sees EOF, and that will not be observed as long as the write end of the pipe remains open in any process, such as the parent process or the second child itself.


