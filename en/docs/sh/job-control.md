---
title: "Job Control"
slug: "job-control"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Pause, run in background, run in foreground
Let's create a process which is rather long to complete :

    $ sleep 1000
To pause the process, type Ctrl + Z :

    ^Z
    [1]+  Stopped                 sleep 1000

You can use `jobs` to see the list of processes running or stopped in the current terminal :

    $ jobs
    [1]+  Stopped                 sleep 1000

To bring back a job on the foreground, use `fg` with the id written between brackets in the list provided by `jobs` :

    $ fg 1                                                     
    sleep 1000

When a job is stopped, you can run it in background with the command `bg` with the same id :

    $ bg 1                                                     
    [1]+ sleep 1000 &
And then see it in the list of jobs in the current terminal :

    $ jobs                                                     
    [1]+  Running                 sleep 1000 &
To directly run a job in background, finish the command with `&` :

    $ jobs                                                     
    [1]+  Running                 sleep 1000 &                                      
    $ sleep 5000 &
    [2] 6743                                                                        
    $ jobs                                                     
    [1]-  Running                 sleep 1000 &                                      
    [2]+  Running                 sleep 5000 &

## List, wait and stop processes
To get a list of the processes running in the current terminal, you can use `ps` :

    $ sleep 1000 &
    $ ps -opid,comm
      PID COMMAND
     1000 sh
     1001 sleep
     1002 ps

To kill a running process, use `kill` with the process ID (PID) indicated by `ps`:

    $ kill 1001
    $ ps -opid,comm
     PID COMMAND
    1000 sh
    1004 ps

To wait for a process to terminate, use the `wait` command :

    $ sleep 10 && echo End &
    $ ps -opid,comm
     PID COMMAND
    1000 sh
    1005 sh
    1006 sleep
    1007 ps
    $ wait 1005 && echo Stop waiting
    End
    Stop waiting

First, we run a process with PID 1005 in background which will print "End" before ending. Then, we wait for this process to finish, and print "Stop waiting".
The output shows "End", meaning the process with PID 1005 is complete, then "Stop waiting", showing the wait command is complete.

