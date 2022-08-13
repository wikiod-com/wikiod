---
title: "InputOutput multiplexing"
slug: "inputoutput-multiplexing"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

IO may be blocking/non-blocking and synchronous/asynchronous. POSIX API provides synchronous blocking API (e.g. classic read, write, send, recv calls), synchronous non-blocking API (same functions, file descriptors opened with `O_NONBLOCK` flag and IO-multiplexing calls) and asynchonous API (functions starting with `aio_`).

Synchronous API is usually used with "one thread/process per fd" style. This is dreadful for resources. Non-blocking API allows to operate with a set of fds in one thread.

## Poll
In this example we create a pair of connected sockets and send 4 strings from one to another and print received strings to console. Note, that the number of times we will call send may not be equal to number of times we call recv
<!-- language: c -->

    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <sys/socket.h>
    #include <sys/types.h>
    #include <unistd.h>
    #include <fcntl.h>
    #include <poll.h>
    
    #define BUFSIZE 512
    
    int main()
    {    
        #define CKERR(msg) {if(ret < 0) { perror(msg); \
            close(sockp[0]); close(sockp[1]); exit(EXIT_FAILURE); } }
        const char* strs_to_write[] = {"hello ", "from ", "other ", "side "};
        
        int sockp[2] = {-1, -1};
        ssize_t ret = socketpair (AF_UNIX, SOCK_STREAM, 0, sockp);
        CKERR("Socket pair creation error")
        
        struct pollfd pfds[2];
        for(int i=0; i<2; ++i) {
            pfds[i] = (struct pollfd){sockp[i], POLLIN|POLLOUT, 0};
            fcntl(sockp[i], F_SETFL|O_NONBLOCK); // nonblocking fds are
                    // literally mandatory for IO multiplexing; non-portable
        }
        char buf[BUFSIZE];
        
        size_t snt = 0, msgs = sizeof(strs_to_write)/sizeof(char*);
        while(1) {
            int ret = poll(pfds,
                2 /*length of pollfd array*/,
                5 /*milliseconds to wait*/);
            CKERR("Poll error")
    
            if (pfds[0].revents & POLLOUT && snt < msgs) {
                // Checking POLLOUT before writing to ensure there is space
                // available in socket's kernel buffer to write, otherwise we
                // may face EWOULDBLOCK / EAGAIN error
                ssize_t ret = send(sockp[0], strs_to_write[snt], strlen(strs_to_write[snt]), 0);
                if(++snt >= msgs)
                    close(sockp[0]);
                CKERR("send error")
                if (ret == 0) {
                    puts("Connection closed");
                    break;
                }
                if (ret > 0) {
                    // assuming that all bytes were written
                    // if ret != %sent bytes number%, send other bytes later
                }
            }
            if (pfds[1].revents & POLLIN) {
                // There is something to read
                ssize_t ret = recv(sockp[1], buf, BUFSIZE, 0);
                CKERR("receive error")
                if (ret == 0) {
                    puts("Connection closed");
                    break;
                }
                if (ret > 0) {
                    printf("received str: %.*s\n", (int)ret, buf);
                }
            }
    
        }
        close(sockp[1]);
        return EXIT_SUCCESS;
    }



## Select
Select is another way to do I/O multiplexing. One of it's advantages is an existance in winsock API. Moreover, on Linux, select() modifies timeout to reflect the amount of time not slept; most other implementations do not do this. (POSIX.1 permits either behavior.)

Both poll and select have ppoll and pselect alternatives, which allow handling incoming signals during waiting for event. And both of them become slow with huge amount of file descriptors (one hundred and more), so it would be wise to choose platform specific call, e.g. `epoll` on Linux and `kqueue` on FreeBSD. Or switch to asynchronous API (POSIX `aio` e.g. or something specific like IO Completion Ports).

Select call has the following prototype:

<!-- language: c -->
    int select(int nfds, fd_set *readfds, fd_set *writefds,
           fd_set *exceptfds, struct timeval *timeout);

`fd_set` is a bitmask array of file descriptors,

`nfds` is the maximum number of all file descriptors in set + 1.

Snippet of working with select:

<!-- language: c -->

    fd_set active_fd_set, read_fd_set;
    FD_ZERO (&active_fd_set); // set fd_set to zeros
    FD_SET (sock, &active_fd_set); // add sock to the set
    // # define FD_SETSIZE sock + 1
    while (1) {
        /* Block until input arrives on one or more active sockets. */
        read_fd_set = active_fd_set; // read_fd_set gets overriden each time
        if (select (FD_SETSIZE, &read_fd_set, NULL, NULL, NULL) < 0) {
            // handle error
        }
        // Service all file descriptors with input pending.
        for (i = 0; i < FD_SETSIZE; ++i) {
            if (FD_ISSET (i, &read_fd_set)) {
                // there is data for i
        }
    }

Note, that on most POSIX implemetations file descriptors associated with files on disk are blocking. So writing to file, even if this file was set in `writefds`, would block until all bytes won't be dumped to disk

