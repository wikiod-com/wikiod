---
title: "Git Clean"
slug: "git-clean"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Syntax
- `git clean [-d] [-f] [-i] [-n] [-q] [-e <pattern>] [-x | -X] [--] <path>` 







## Parameters
| Parameter | Details |
| --------- | ------- |
| -d |Remove untracked directories in addition to untracked files. If an untracked directory is managed by a different Git repository, it is not removed by default. Use -f option twice if you really want to remove such a directory. |
| -f, --force | If the Git configuration variable clean. `requireForce` is not set to false, git clean will refuse to delete files or directories unless given -f, -n or -i. Git will refuse to delete directories with .git sub directory or file unless a second -f is given. |
| -i, --interactive| Interactively prompts the removal of each file. |
|-n, --dry-run| Only displays a list of files to be removed, without actually removing them.|
|-q,--quiet|Only display errors, not the list of successfully removed files.|



## Clean Ignored Files
    git clean -fX
Will remove all [ignored][1] files from the current directory and all subdirectories.

    git clean -Xn
Will preview all files that will be cleaned.


  [1]: https://www.wikiod.com/git/ignoring-files-and-folders

## Clean All Untracked Directories
    git clean -fd
Will remove all untracked directories and the files within them. It will start at the current working directory and will iterate through all subdirectories.

    git clean -dn
Will preview all directories that will be cleaned.

## Forcefully remove untracked files
    git clean -f

Will remove all untracked files.

## Clean Interactively
```
git clean -i
```
Will print out items to be removed and ask for a confirmation via commands like the follow:
```
Would remove the following items:
  folder/file1.py
  folder/file2.py
*** Commands ***
    1: clean        2: filter by pattern        3: select by numbers        4: ask each
    5: quit        6: help
What now>
```
Interactive option `i` can be added along with other options like `X`, `d`, etc.

