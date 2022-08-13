---
title: "Debugging a container"
slug: "debugging-a-container"
draft: false
images: []
weight: 9817
type: docs
toc: true
---

## Syntax
- docker stats [OPTIONS] [CONTAINER...]
- docker logs [OPTIONS] CONTAINER
- docker top [OPTIONS] CONTAINER [ps OPTIONS]

## Printing the logs
Following the logs is the less intrusive way to debug a live running application. This example reproduces the behavior of the traditional ``tail -f some-application.log`` on container ``7786807d8084``.

    docker logs --follow --tail 10 7786807d8084

This command basically shows the standard output of the container process (the process with pid 1).

If your logs do not natively include timestamping, you may add the ``--timestamps`` flag.

It is possible to look at the logs of a stopped container, either
- start the failing container with 
`docker run ... ; docker logs $(docker ps -lq)`

- find the container id or name with 

`docker ps -a`

and then

`docker logs container-id`
or

`docker logs containername`

as it is possible to look at the logs of a stopped container 

## Monitoring resource usage
Inspecting system resource usage is an efficient way to find misbehaving applications. This example is an equivalent of the traditional ``top`` command for containers:

    docker stats

To follow the stats of specific containers, list them on the command line:

    docker stats 7786807d8084 7786807d8085

Docker stats displays the following information:

    CONTAINER     CPU %   MEM USAGE / LIMIT   MEM %    NET I/O               BLOCK I/O
    7786807d8084  0.65%   1.33 GB / 3.95 GB   33.67%   142.2 MB / 57.79 MB   46.32 MB / 0 B

By default `docker stats` displays the id of the containers, and this is not very helpful, if your prefer to display the names of the container, just do

`docker stats $(docker ps --format '{{.Names}}')`



## Attach to a running container
'Attaching to a container' is the act of starting a terminal session within the context that the container (and any programs therein) is running. This is primarily used for debugging purposes, but may also be needed if specific data needs to be passed to programs running within the container.

The `attach` command is utilized to do this. It has this syntax:

    docker attach <container>

`<container>` can be either the container id or the container name. For instance:

    docker attach c8a9cf1a1fa8

Or:

    docker attach graceful_hopper

You may need to `sudo` the above commands, depending on your user and how docker is set up.

> Note: Attach only allows a single shell session to be attached to a container at a time.

> Warning: *all* keyboard input will be forwarded to the container. Hitting <kbd>Ctrl-c</kbd> will *kill* your container.

To detach from an attached container, successively hit <kbd>Ctrl-p</kbd> then <kbd>Ctrl-q</kbd> 

To attach multiple shell sessions to a container, or simply as an alternative, you can use `exec`. Using the container id:

    docker exec -i -t c8a9cf1a1fa8 /bin/bash

Using the container's name:

    docker exec -i -t graceful_hopper /bin/bash

`exec` will run a program within a container, in this case `/bin/bash` (a shell, presumably one the container has). `-i` indicates an interactive session, while `-t` allocates a pseudo-TTY. 

> Note: Unlike *attach*, hitting <kbd>Ctrl-c</kbd> will only terminate the *exec*'d command when running interactively.

## Entering in a running container
To execute operations in a container, use the `docker exec` command. Sometimes this is called "entering the container" as all commands are executed inside the container.

    docker exec -it container_id bash

or 

    docker exec -it container_id /bin/sh

And now you have a shell in your running container. For example, list files in a directory and then leave the container:

    docker exec container_id ls -la

You can use the `-u flag` to enter the container with a specific user, e.g. `uid=1013`, `gid=1023`.

    docker exec -it -u 1013:1023 container_id ls -la

The uid and gid does not have to exist in the container but the command can result in errors.If you want to launch a container and immediately enter inside in order to check something, you can do

`docker run...; docker exec -it $(docker ps -lq) bash`

the command `docker ps -lq` outputs only the id of the last (the l in `-lq`) container started.
(this supposes you have bash as interpreter available in your container, you may have sh or zsh or any other)





## Monitoring processes in a container
Inspecting system resource usage is an efficient way to narrow down a problem on a live running application. This example is an equivalent of the traditional ``ps`` command for containers.

    docker top 7786807d8084

To filter of format the output, add ``ps`` options on the command line:

    docker top 7786807d8084 faux

Or, to get the list of processes running as root, which is a potentially harmful practice:

    docker top 7786807d8084 -u root

The ``docker top`` command proves especially useful when troubleshooting minimalistic containers without a shell or the ``ps`` command.

## Docker container process debugging
Docker is just a fancy way to run a process, not a virtual machine. Therefore, debugging a process "in a container" is also possible "on the host" by simply examining the running container process as a user with the appropriate permissions to inspect those processes on the host (e.g. root). For example, it's possible to list every "container process" on the host by running a simple `ps` as root:

    sudo ps aux

Any currently running Docker containers will be listed in the output. 

This can be useful during application development for debugging a process running in a container. As a user with appropriate permissions, typical debugging utilities can be used on the container process, such as strace, ltrace, gdb, etc.

