---
title: "Filesystem"
slug: "filesystem"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Remove files recursively (nftw, not thread-safe)
<!-- language: lang-c -->

    #define _XOPEN_SOURCE 500
    #include <stdlib.h>  /* for exit() */
    #include <stdio.h>   /* for remove() */
    #include <ftw.h>     /* for nftw() */
    
    int unlink_cb(
        const char *fpath, const struct stat *sb, int typeflag, struct FTW *ftwbuf)
    {
        return remove(fpath);
    }
    
    int rm_rf(const char *path)
    {
        return nftw(path,
                    unlink_cb,
                    64 /* number of simultaneously opened fds, up to OPEN_MAX */,
                    FTW_DEPTH | FTW_PHYS);
    }

`FTW_PHYS` flag means do not follow symbolic links

`FTW_DEPTH` flag does a post-order traversal, that is, call `unlink_cb()` for the directory itself after handling the contents of the directory and its subdirectories.

`nftw` is interrupted if callback function returns non-zero value.

*Note: this method is not thread-safe , because `nftw` uses `chdir`.*

## Count number of text files in the directory
<!-- language: lang-c -->

    #include <string.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <sys/types.h>
    #include <unistd.h>
    #include <dirent.h>
    #include <ctype.h>
    
    int main(int argc, char **argv)
    {
        const char *dname = (argc > 1 ? argv[1] : ".");
        DIR* pdir = opendir(dname);
        if(pdir == NULL) {
            perror("Can't open directory");
            return EXIT_FAILURE;
        }
        int textfiles=0;
        struct dirent* dent;
        while ((dent=readdir(pdir))!=NULL) {
            int d_name_len = strlen(dent->d_name);
            if (d_name_len > 4 &&
                    strcmp(dent->d_name + (d_name_len - 4), ".txt") == 0) {
                textfiles++;
            }
        }
        printf("Number of text files: %i\n", textfiles);
        closedir(pdir);
        return 0;
    }

## Remove files recursively (openat and unlinkat, thread-safe)
<!-- language: lang-c -->

    #include <stddef.h>   /* for offsetof() */
    #include <stdlib.h>   /* for exit() */
    #include <stdio.h>    /* for perror() */
    #include <string.h>   /* for strcmp() */
    #include <unistd.h>   /* for close(), unlink() */
    #include <fcntl.h>    /* for open() */
    #include <dirent.h>   /* for DIR */
    #include <sys/stat.h> /* for stat */
    #include <limits.h>   /* for NAME_MAX */

    int rm_rf(const char* path)
    {
        if (unlink(path) == 0) {
            return 0;
        } else {
            int dirfd = open(path, O_RDONLY | O_DIRECTORY);
            if (dirfd == -1) {
                perror("open");
                return -1;
            }
            if (rm_children(dirfd) == -1) {
                return -1;
            }
            if (rmdir(path) == -1) {
                perror("rmdir");
                return -1;
            }
            return 0;
        }
    }

    int rm_children(int dirfd)
    {
        DIR* dir = fdopendir(dirfd);
        if (dir == NULL) {
            perror("fdopendir");
            if (close(dirfd) == -1) {
                perror("close");
            }
            return -1;
        }

        char buf[offsetof(struct dirent, d_name) + NAME_MAX + 1];

        struct dirent* dbuf = (struct dirent*)buf;
        struct dirent* dent = NULL;

        int ret = 0;

        for (;;) {
            if ((ret = readdir_r(dir, dbuf, &dent)) == -1) {
                perror("readdir_r");
                break;
            }
            if (dent == NULL) {
                break;
            }
            if (strcmp(dent->d_name, ".") == 0 || strcmp(dent->d_name, "..") == 0) {
                continue;
            }
            if ((ret = rm_at(dirfd, dent->d_name)) == -1) {
                break;
            }
        }

        if (closedir(dir) == -1) {
            perror("closedir");
            ret = -1;
        }

        return ret;
    }

    int rm_at(int dirfd, const char* name)
    {
        int fd = openat(dirfd, name, O_RDONLY);
        if (fd == -1) {
            perror("openat");
            return -1;
        }

        int ret = 0;

        struct stat st;
        if ((ret = fstat(fd, &st)) == -1) {
            perror("fstat");
            goto out;
        }

        if (S_ISDIR(st.st_mode)) {
            ret = rm_children(fd);
            fd = -1;
            if (ret == -1) {
                goto out;
            }
        }

        ret = unlinkat(dirfd, name, S_ISDIR(st.st_mode) ? AT_REMOVEDIR : 0);
        if (ret == -1) {
            perror("unlinkat");
            goto out;
        }

    out:
        if (fd != -1) {
            if (close(fd) == -1) {
                perror("close");
                ret = -1;
            }
        }

        return ret;
    }

*Note: this method is thread-safe, but uses stack for uncontrolled recursion. Each tree level adds at least NAME_MAX bytes + one frame size.*

