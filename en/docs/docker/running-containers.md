---
title: "Running containers"
slug: "running-containers"
draft: false
images: []
weight: 9567
type: docs
toc: true
---

## Syntax
- docker run [OPTIONS] IMAGE [COMMAND] [ARG...]


## Automatically delete a container after running it
Normally, a Docker container persists after it has exited. This allows you to run the container again, inspect its filesystem, and so on. However, sometimes you want to run a container and delete it immediately after it exits. For example to execute a command or show a file from the filesystem. Docker provides the `--rm` command line option for this purpose:

```
docker run --rm ubuntu cat /etc/hosts
```

This will create a container from the "ubuntu" image, show the content of **/etc/hosts** file and then delete the container immediately after it exits. This helps to prevent having to clean up containers after you're done experimenting.

> **Note:** The `--rm` flag doesn't work in conjunction with the `-d` (`--detach`) flag in docker < 1.13.0.

When `--rm` flag is set, Docker also removes the volumes associated with the container when the container is removed. This is similar to running ``docker rm -v my-container``. ***Only volumes that are specified without a name are removed***. 

For example, with ``docker run -it --rm -v /etc -v logs:/var/log centos /bin/produce_some_logs``, the volume of ``/etc`` will be removed, but the volume of ``/var/log`` will not. Volumes inherited via --volumes-from will be removed with the same logic -- if the original volume was specified with a name it will not be removed.

## Container restart policy (starting a container at boot)
    docker run --restart=always -d <container>


By default, Docker will not restart containers when the Docker daemon restarts, for example after a host system reboot. Docker provides a restart policy for your containers by supplying the `--restart` command line option. Supplying `--restart=always` will always cause a container to be restarted after the Docker daemon is restarted. **However** when that container is manually stopped (e.g. with `docker stop <container>`), the restart policy will not be applied to the container.


Multiple options can be specified for `--restart` option, based on the requirement (``--restart=[policy]``). These options effect how the container starts at boot as well.

| Policy| Result|
| ------ | ------ |
| **no**| The **default** value. Will not restart container automatically, when container is stopped.     |
|**on-failure[:max-retries]** |Restart only if the container exits with a failure (``non-zero exit status``). To avoid restarting it indefinitely (in case of some problem), one can limit the number of restart retries the Docker daemon attempts.|
|**always**| Always restart the container regardless of the exit status. When you specify ``always``, the Docker daemon will try to restart the container indefinitely. The container will also always start on daemon startup, regardless of the current state of the container.  |
|**unless-stopped**|Always restart the container regardless of its exit status, but do not start it on daemon startup if the container has been put to a stopped state before. |

## Specifying a name
By default, containers created with `docker run` are given a random name like `small_roentgen` or `modest_dubinsky`. These names aren't particularly helpful in identifying the purpose of a container. It is possible to supply a name for the container by passing the `--name` command line option:

    docker run --name my-ubuntu ubuntu:14.04

Names must be unique; if you pass a name that another container is already using, Docker will print an error and no new container will be created.

Specifying a name will be useful when referencing the container within a Docker network. This works for both background and foreground Docker containers. 

Containers on the default bridge network **must** be linked to communicate by name.

## Run a container in background
To keep a container running in the background, supply the `-d` command line option during container startup:

    docker run -d busybox top

The option `-d` runs the container in detached mode. It is also equivalent to `-d=true`.

A container in detached mode cannot be removed automatically when it stops, this means one cannot use the --rm option in combination with -d option.

## Assign a volume to a container
A Docker volume is a file or directory which persists beyond the lifetime of the container. It is possible to mount a host file or directory into a container as a volume (bypassing the UnionFS).

Add a volume with the `-v` command line option:

    docker run -d -v "/data" awesome/app bootstrap.sh

This will create a volume and mount it to the path `/data` inside the container. 

* Note: You can use the flag `--rm` to automatically remove the volume when the container is removed. 

**Mounting host directories**

To mount a host file or directory into a container:

    docker run -d -v "/home/foo/data:/data" awesome/app bootstrap.sh

* **When specifying a host directory, an absolute path must be supplied.**

This will mount the host directory `/home/foo/data` onto `/data` inside the container. This "bind-mounted host directory" volume is the same thing as a Linux `mount --bind` and therefore temporarily mounts the host directory over the specified container path for the duration of the container's lifetime. Changes in the volume from either the host or the container are reflected immediately in the other, because they are the same destination on disk.

UNIX example mounting a relative folder

    docker run -d -v $(pwd)/data:/data awesome/app bootstrap.sh

**Naming volumes**

A volume can be named by supplying a string instead of a host directory path, docker will create a volume using that name.

    docker run -d -v "my-volume:/data" awesome/app bootstrap.sh

After creating a named volume, the volume can then be shared with other containers using that name.



## Getting a shell into a running (detached) container
# Log into a running container
A user can enter a running container in a new interactive bash shell with __[`exec`][1]__ command.

Say a container is called `jovial_morse` then you can get an interactive, pseudo-TTY bash shell by running:

```
docker exec -it jovial_morse bash
```

# Log into a running container with a specific user
If you want to enter a container as a specific user, you can set it with `-u` or `--user` parameter. The username must exists in the container.

>   `-u, --user`           Username or UID (format: `<name|uid>[:<group|gid>]`)

This command will log into `jovial_morse` with the `dockeruser` user

```
docker exec -it -u dockeruser jovial_morse bash
```
# Log into a running container as root
If you want to log in as root, just simply use the __`-u root`__ parameter. Root user always exists.

```
docker exec -it -u root jovial_morse bash
```


# Log into a image
You can also log into a image with the [`run`][2] command, but this requires an image name instead of a container name.

```
docker run -it dockerimage bash
```

# Log into a intermediate image (debug)
You can log into an intermediate image as well, which is created during a Dockerfile build.

Output of `docker build .`

```
$ docker build .
Uploading context 10240 bytes
Step 1 : FROM busybox
Pulling repository busybox
 ---> e9aa60c60128MB/2.284 MB (100%) endpoint: https://cdn-registry-1.docker.io/v1/
Step 2 : RUN ls -lh /
 ---> Running in 9c9e81692ae9
total 24
drwxr-xr-x    2 root     root        4.0K Mar 12  2013 bin
drwxr-xr-x    5 root     root        4.0K Oct 19 00:19 dev
drwxr-xr-x    2 root     root        4.0K Oct 19 00:19 etc
drwxr-xr-x    2 root     root        4.0K Nov 15 23:34 lib
lrwxrwxrwx    1 root     root           3 Mar 12  2013 lib64 -> lib
dr-xr-xr-x  116 root     root           0 Nov 15 23:34 proc
lrwxrwxrwx    1 root     root           3 Mar 12  2013 sbin -> bin
dr-xr-xr-x   13 root     root           0 Nov 15 23:34 sys
drwxr-xr-x    2 root     root        4.0K Mar 12  2013 tmp
drwxr-xr-x    2 root     root        4.0K Nov 15 23:34 usr
 ---> b35f4035db3f
Step 3 : CMD echo Hello world
 ---> Running in 02071fceb21b
 ---> f52f38b7823e
```

Notice the `---> Running in 02071fceb21b` output, you can log into these images:

```
docker run -it 02071fceb21b bash
```


  [1]: https://docs.docker.com/engine/reference/commandline/exec/
  [2]: https://docs.docker.com/engine/reference/commandline/run/

## Run a container interactively
To run a container interactively, pass in the `-it` options:

```
$ docker run -it ubuntu:14.04 bash
root@8ef2356d919a:/# echo hi
hi
root@8ef2356d919a:/#
```

`-i` keeps STDIN open, while `-t` allocates a pseudo-TTY.

## Detaching from a container
While attached to a running container with a pty assigned (`docker run -it ...`), you can press <kbd>Control</kbd><kbd>P</kbd> - <kbd>Control</kbd><kbd>Q</kbd> to detach.

## Binding a container port to the host
    docker run -p "8080:8080" myApp
    docker run -p "192.168.1.12:80:80" nginx
    docker run -P myApp

In order to use ports on the host have been exposed in an image (via the `EXPOSE` Dockerfile directive, or `--expose` command line option for `docker run`), those ports need to be bound to the host using the `-p` or `-P` command line options. Using `-p` requires that the particular port (and optional host interface) to be specified. Using the uppercase `-P` command line option will force Docker to bind *all* exposed ports in a container's image to the host.

## Setting environment variables
    $ docker run -e "ENV_VAR=foo" ubuntu /bin/bash

Both `-e` and `--env` can be used to define environment variables inside of a container. It is possible to supply many environment variables using a text file:

    $ docker run --env-file ./env.list ubuntu /bin/bash

Example environment variable file:

    # This is a comment
    TEST_HOST=10.10.0.127

The `--env-file` flag takes a filename as an argument and expects each line to be in the `VARIABLE=VALUE` format, mimicking the argument passed to `--env`. Comment lines need only be prefixed with `#`.

Regardless of the order of these three flags, the `--env-file` are processed first, and then `-e`/`--env` flags. This way, any environment variables supplied individually with `-e` or `--env` will override variables supplied in the `--env-var` text file.

## Passing stdin to the container
In cases such as restoring a database dump, or otherwise wishing to push some information through a pipe from the host, you can use the `-i` flag as an argument to `docker run` or `docker exec`.

E.g., assuming you want to put to a containerized mariadb client a database dump that you have on the host, in a local `dump.sql` file, you can perform the following command:

    docker exec -i mariadb bash -c 'mariadb "-p$MARIADB_PASSWORD" ' < dump.sql

In general,

    docker exec -i container command < file.stdin
Or

    docker exec -i container command <<EOF
    inline-document-from-host-shell-HEREDOC-syntax
    EOF

## Overriding image entrypoint directive
    docker run --name="test-app" --entrypoint="/bin/bash" example-app

This command will override the `ENTRYPOINT` directive of the `example-app` image when the container `test-app` is created. The `CMD` directive of the image will remain unchanged unless otherwise specified:

    docker run --name="test-app" --entrypoint="/bin/bash" example-app /app/test.sh

In the above example, both the `ENTRYPOINT` and the `CMD` of the image have been overridden. This container process becomes `/bin/bash /app/test.sh`.

## Specifying a hostname
By default, containers created with docker run are given a random hostname. You can give the container a different hostname by passing the --hostname flag:

    docker run --hostname redbox -d ubuntu:14.04

## Running a container
```
docker run hello-world
```

This will fetch the latest [hello-world] image from the Docker Hub (if you don't already have it), create a new container, and run it. You should see a message stating that your installation appears to be working correctly.

[hello-world]: https://hub.docker.com/r/_/hello-world/

## Running a different command in the container
<!-- language: lang-none -->
```
docker run docker/whalesay cowsay 'Hello, StackExchange!'
```

This command tells Docker to create a container from the `docker/whalesay` image and run the command `cowsay 'Hello, StackExchange!'` in it.
It should print a picture of a whale saying `Hello, StackExchange!` to your terminal.

If the entrypoint in the image is the default you can run any command that's available in the image:

```
docker run docker/whalesay ls /
```

If it has been changed during image build you need to reverse it back to the default

```
docker run --entrypoint=/bin/bash docker/whalesay -c ls /
```

## Prevent container from stopping when no commands are running
A container will stop if no command is running on the foreground. Using the `-t` option will keep the container from stopping, even when detached with the `-d` option.

    docker run -t -d debian bash

## Stopping a container

    docker stop mynginx

Additionally, the container id can also be used to stop the container instead of its name.

This will stop a running container by sending the SIGTERM signal and then the SIGKILL signal if necessary.

Further, the kill command can be used to immediately send a SIGKILL or any other specified signal using the `-s` option.

    docker kill mynginx
Specified signal:

    docker kill -s SIGINT mynginx

Stopping a container doesn't delete it. Use `docker ps -a` to see your stopped container.

## Running container with memory/swap limits
Set memory limit and disable swap limit

    docker run -it -m 300M --memory-swap -1 ubuntu:14.04 /bin/bash
    

Set both memory and swap limit. In this case, container can use 300M memory and 700M swap.

    docker run -it -m 300M --memory-swap 1G ubuntu:14.04 /bin/bash

## Add host entry to container
    docker run --add-host="app-backend:10.15.1.24" awesome-app

This command adds an entry to the container's `/etc/hosts` file, which follows the format `--add-host <name>:<address>`. In this example, the name `app-backend` will resolve to `10.15.1.24`. This is particularly useful for tying disparate app components together programmatically.

## Execute another command on a running container
When required you can tell Docker to execute additional commands on an already running container using the `exec` command. You need the container's ID which you can get with `docker ps`.

`docker exec 294fbc4c24b3 echo "Hello World"`

You can attach an interactive shell if you use the `-it` option.

`docker exec -it 294fbc4c24b3 bash`

## Running GUI apps in a Linux container
By default, a Docker container won't be able to *run* a GUI application.

Before that, the X11 socket must be forwarded first to the container, so it can be used directly. The *DISPLAY* environment variable must be forwarded as well:

    docker run -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=unix$DISPLAY <image-name>

This will fail at first, since we didn't set the permissions to the X server host:

    cannot connect to X server unix:0

The quickest (but not safest) way is to allow access directly with:

    xhost +local:root

After finishing with the container, we can go back to the original state with:

    xhost -local:root


----------

Another (safer) way is to prepare a Dockerfile that will build a new image that will use the our user credentials to access the X server:


    FROM <iamge-name>
    MAINTAINER <you>
    
    # Arguments picked from the command line!
    ARG user
    ARG uid
    ARG gid
    
    #Add new user with our credentials
    ENV USERNAME ${user}
    RUN useradd -m $USERNAME && \
            echo "$USERNAME:$USERNAME" | chpasswd && \
            usermod --shell /bin/bash $USERNAME && \
            usermod  --uid ${uid} $USERNAME && \
            groupmod --gid ${gid} $USERNAME
    
    USER ${user}
    
    WORKDIR /home/${user}

When invoking `docker build` from the command line, we have to pass the *ARG* variables that appear in the Dockerfile:

    docker build --build-arg user=$USER --build-arg uid=$(id -u) --build-arg gid=$(id -g) -t <new-image-with-X11-enabled-name> -f <Dockerfile-for-X11> .

Now, before spawning a new container, we have to create a xauth file with access permission:

    xauth nlist $DISPLAY | sed -e 's/^..../ffff/' | xauth -f /tmp/.docker.xauth nmerge -

This file has to be mounted into the container when creating/running it:

    docker run -e DISPLAY=unix$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix -v /tmp/.docker.xauth:/tmp/.docker.xauth:rw -e XAUTHORITY=/tmp/.docker.xauth

