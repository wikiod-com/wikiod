---
title: "strace"
slug: "strace"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Syntax
- strace  -c[df]  [-In]  [-bexecve]  [-eexpr]...  [-Ooverhead] [-Ssortby]
       -ppid... / [-D] [-Evar[=val]]... [-uusername] command [args]


## How to observe the system calls of a program
For an *executable file *or* command* exec, running this will list all system calls:

    $ ptrace exec

To display specific system calls use -e option:

    $ strace -e open exec

To save the output to a file use the -o option:

    $ strace -o output exec

To find the system calls an active program uses, use the -p option while specifying the pid [[how to get pid]][1] :

    $ sudo strace -p 1115

To generate a statistics report of all the system calls used, use option -c:

    $ strace -c exec 



[1]:https://stackoverflow.com/questions/31676071/how-to-get-process-id-of-specific-process

