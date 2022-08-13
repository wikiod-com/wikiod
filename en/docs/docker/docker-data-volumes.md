---
title: "Docker Data Volumes"
slug: "docker-data-volumes"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

Docker data volumes provide a way to persist data independent of a container's life cycle. Volumes present a number of helpful features such as:

Mounting a host directory within the container, sharing data in-between containers using the filesystem and preserving data if a container gets deleted

## Syntax
- docker volume [OPTIONS] [COMMAND]

## Mounting a directory from the local host into a container
It is possible to mount a host directory to a specific path in your container using the `-v` or `--volume` command line option. The following example will mount `/etc` on the host to `/mnt/etc` in the container:

    (on linux) docker run -v "/etc:/mnt/etc" alpine cat /mnt/etc/passwd
    (on windows)  docker run -v "/c/etc:/mnt/etc" alpine cat /mnt/etc/passwd

The default access to the volume inside the container is read-write. To mount a volume read-only inside of a container, use the suffix `:ro`:

    docker run -v "/etc:/mnt/etc:ro" alpine touch /mnt/etc/passwd

## Creating a named volume
    docker volume create --name="myAwesomeApp"

Using a named volume makes managing volumes much more human-readable. It is possible to create a named volume using the command specified above, but it's also possible to create a named volume inside of a `docker run` command using the `-v` or `--volume` command line option:

    docker run -d --name="myApp-1" -v="myAwesomeApp:/data/app" myApp:1.5.3

Note that creating a named volume in this form is similar to mounting a host file/directory as a volume, except that instead of a valid path, the volume name is specified. Once created, named volumes can be shared with other containers:

    docker run -d --name="myApp-2" --volumes-from "myApp-1" myApp:1.5.3

After running the above command, a new container has been created with the name `myApp-2` from the `myApp:1.5.3` image, which is sharing the `myAwesomeApp` named volume with `myApp-1`. The `myAwesomeApp` named volume is mounted at `/data/app` in the `myApp-2` container, just as it is mounted at `/data/app` in the `myApp-1` container.

