---
title: "File locks"
slug: "file-locks"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
- int fcntl(int fd, int cmd, struct flock*);
- int lockf(int fd, int cmd, off_t len);

## POSIX record locks (fcntl)
This example demonstrates usage of POSIX record locks (a.k.a. process-associated locks), provided by [`fcntl`][1] function (POSIX base standard).

Notes:

* Exclusive and shared locks are supported.
* Can be applied to a byte range, optionally automatically expanding when data is appended in future (controlled by `struct flock`).
* Locks are released on first close by the locking process of *any* file descriptor for the file, or when process terminates.

<!-- language: c -->

    #include <stdlib.h>  /* for exit() */
    #include <stdio.h>   /* for perror() */
    #include <string.h>  /* for memset() */
    #include <unistd.h>  /* for close() */
    #include <fcntl.h>   /* for open(), fcntl() */

    int main(int argc, char **argv) {
        /* open file
         * we need O_RDWR for F_SETLK */
        int fd = open(argv[1], O_RDWR);
        if (fd == -1) {
            perror("open");
            exit(EXIT_FAILURE);
        }

        struct flock fl;
        memset(&fl, 0, sizeof(fl));

        /* lock entire file */
        fl.l_type = F_RDLCK;    /* F_RDLCK is shared lock */
        fl.l_whence = SEEK_SET; /* offset base is start of the file */
        fl.l_start = 0;         /* starting offset is zero */
        fl.l_len = 0;           /* len is zero, which is a special value
                                   representing end of file (no matter
                                   how large the file grows in future) */

        /* F_SETLKW specifies blocking mode */
        if (fcntl(fd, F_SETLKW, &fl) == -1) {
            perror("fcntl(F_SETLKW)");
            exit(EXIT_FAILURE);
        }

        /* atomically upgrade shared lock to exclusive lock, but only
         * for bytes in range [10; 15)
         *
         * after this call, the process will hold three lock regions:
         *   [0; 10)        - shared lock
         *   [10; 15)       - exclusive lock
         *   [15; SEEK_END) - shared lock
         */
        fl.l_type = F_WRLCK;    /* F_WRLCK is exclusive lock */
        fl.l_whence = SEEK_SET;
        fl.l_start = 10;
        fl.l_len= 5;

        /* F_SETLKW specifies non-blocking mode */
        if (fcntl(fd, F_SETLK, &fl) == -1) {
            perror("fcntl(F_SETLK)");
            exit(EXIT_FAILURE);
        }

        /* release lock for bytes in range [10; 15) */
        fl.l_type = F_UNLCK;

        if (fcntl(fd, F_SETLK, &fl) == -1) {
            perror("fcntl(F_SETLK)");
            exit(EXIT_FAILURE);
        }

        /* close file and release locks for all regions
         * note that locks are released when process calls close() on any
         * descriptor for a lock file */
        close(fd);

        return EXIT_SUCCESS;
    }


  [1]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/fcntl.html

## lockf function
This example demonstrates usage of [`lockf`][1] function (POSIX XSI).

Notes:

* Only exclusive locks are supported.
* Can be applied to a byte range, optionally automatically expanding when data is appended in future (controlled by `len` argument and position set with `lseek` function).
* Locks are released on first close by the locking process of *any* file descriptor for the file, or when process terminates.
* The interaction between `fcntl` and `lockf` locks is unspecified. On Linux, `lockf` is a wrapper for POSIX record locks.

<!-- language: c -->

    #include <stdlib.h>  /* for exit() */
    #include <stdio.h>   /* for perror() */
    #include <unistd.h>  /* for lockf(), lseek() */
    #include <fcntl.h>   /* for open() */

    int main(int argc, char **argv) {
        /* open file
         * we need O_RDWR for lockf */
        int fd = open(argv[1], O_RDWR);
        if (fd == -1) {
            perror("open");
            exit(EXIT_FAILURE);
        }

        /* set current position to byte 10 */
        if (lseek(fd, 10, SEEK_SET) == -1) {
            perror("lseek");
            exit(EXIT_FAILURE);
        }

        /* acquire exclusive lock for bytes in range [10; 15)
         * F_LOCK specifies blocking mode */
        if (lockf(fd, F_LOCK, 5) == -1) {
            perror("lockf(LOCK)");
            exit(EXIT_FAILURE);
        }

        /* release lock for bytes in range [10; 15) */
        if (lockf(fd, F_ULOCK, 5) == -1) {
            perror("lockf(ULOCK)");
            exit(EXIT_FAILURE);
        }

        return EXIT_SUCCESS;
    }


  [1]: http://pubs.opengroup.org/onlinepubs/9699919799/functions/lockf.html

