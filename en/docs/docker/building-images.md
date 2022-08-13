---
title: "Building images"
slug: "building-images"
draft: false
images: []
weight: 9918
type: docs
toc: true
---

## Parameters
| Parameter            | Details                                              |
|----------------------|------------------------------------------------------|
| --pull               | Ensures that the base image (`FROM`) is up-to-date before building the rest of the Dockerfile.|



## Building an image from a Dockerfile
Once you have a Dockerfile, you can build an image from it using `docker build`.
The basic form of this command is:

<pre>docker build -t <em>image-name</em> <em>path</em></pre>

If your Dockerfile isn't named `Dockerfile`, you can use the `-f` flag to give the name of the Dockerfile to build.

    docker build -t image-name -f Dockerfile2 .

For example, to build an image named `dockerbuild-example:1.0.0` from a `Dockerfile` in the current working directory:

    $ ls
    Dockerfile Dockerfile2

    $ docker build -t dockerbuild-example:1.0.0 .

    $ docker build -t dockerbuild-example-2:1.0.0 -f Dockerfile2 .

See the [`docker build` usage documentation] for more options and settings.

[`docker build` usage documentation]: https://docs.docker.com/engine/reference/builder/#/usage

A common mistake is creating a Dockerfile in the user home directory (`~`). This is a bad idea because during `docker build -t mytag .` this message will appear for a long time:

> Uploading context

The cause is the docker daemon trying to copy all the user's files (both the home directory and it's subdirectories). Avoid this by always specifying a directory for the Dockerfile.

Adding a `.dockerignore` file to the build directory [is a good practice][1]. Its syntax is similar to `.gitignore` files and will make sure only wanted files and directories are uploaded as the context of the build.


  [1]: https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/#/use-a-dockerignore-file

## Difference between ENTRYPOINT and CMD
There are two `Dockerfile` directives to specify what command to run by default in built images. If you only specify `CMD` then docker will run that command using the default `ENTRYPOINT`, which is `/bin/sh -c`. You can override either or both the entrypoint and/or the command when you start up the built image. If you specify both, then the `ENTRYPOINT` specifies the executable of your container process, and `CMD` will be supplied as the parameters of that executable.

For example if your `Dockerfile` contains

    FROM ubuntu:16.04
    CMD ["/bin/date"]

Then you are using the default `ENTRYPOINT` directive of `/bin/sh -c`, and running `/bin/date` with that default entrypoint. The command of your container process will be `/bin/sh -c /bin/date`. Once you run this image then it will by default print out the current date

    $ docker build -t test .
    $ docker run test
    Tue Jul 19 10:37:43 UTC 2016

You can override `CMD` on the command line, in which case it will run the command you have specified.

    $ docker run test /bin/hostname
    bf0274ec8820

If you specify an `ENTRYPOINT` directive, Docker will use that executable, and the `CMD` directive specifies the default parameter(s) of the command. So if your `Dockerfile` contains:

    FROM ubuntu:16.04
    ENTRYPOINT ["/bin/echo"]
    CMD ["Hello"]

Then running it will produce

    $ docker build -t test .
    $ docker run test
    Hello

You can provide different parameters if you want to, but they will all run `/bin/echo`

    $ docker run test Hi
    Hi

If you want to override the entrypoint listed in your Dockerfile (i.e. if you wish to run a different command than `echo` in this container), then you need to specify the `--entrypoint` parameter on the command line:

    $ docker run --entrypoint=/bin/hostname test
    b2c70e74df18

Generally you use the `ENTRYPOINT` directive to point to your main application you want to run, and `CMD` to the default parameters.

## A simple Dockerfile
    FROM node:5

The `FROM` directive specifies an image to start from. Any valid [image reference] may be used.

    WORKDIR /usr/src/app

The `WORKDIR` directive sets the current working directory inside the container, equivalent to running `cd` inside the container. (Note: `RUN cd` will *not* change the current working directory.)

    RUN npm install cowsay knock-knock-jokes

`RUN` executes the given command inside the container.

    COPY cowsay-knockknock.js ./

`COPY` copies the file or directory specified in the first argument from the build context (the <code><em>path</em></code> passed to <code>docker build <em>path</em></code>) to the location in the container specified by the second argument.

    CMD node cowsay-knockknock.js

`CMD` specifies a command to execute when the image is [run] and no command is given. It can be overridden by [passing a command to `docker run`].

There are many other instructions and options; see the [Dockerfile reference] for a complete list.

[image reference]: https://www.wikiod.com/docker/managing-images#Referencing images
[run]: https://www.wikiod.com/docker/running-containers#Running a container
[passing a command to `docker run`]: https://www.wikiod.com/docker/running-containers#Running a different command in the container
[Dockerfile reference]: https://docs.docker.com/engine/reference/builder/

## Pushing and Pulling an Image to Docker Hub or another Registry
Locally created images can be pushed to [Docker Hub][1] or any other docker repo host, known as a registry. Use `docker login` to sign in to an existing docker hub account.
 

    docker login

    Login with your Docker ID to push and pull images from Docker Hub.
    If you don't have a Docker ID, head over to https://hub.docker.com to create one.

    Username: cjsimon
    Password:
    Login Succeeded

A different docker registry can be used by specifying a server name. This also works for private or self-hosted registries. Further, using an [external credentials store][2] for safety is possible.

    docker login quay.io

You can then tag and push images to the registry that you are logged in to. Your repository must be specified as `server/username/reponame:tag`. Omitting the server currently defaults to Docker Hub. (The default registry cannot be changed to another provider, and there are [no plans][3] to implement this feature.) 

    docker tag mynginx quay.io/cjsimon/mynginx:latest

Different tags can be used to represent different versions, or branches, of the same image. An image with multiple different tags will display each tag in the same repo.

Use `docker images` to see a list of installed images installed on your local machine, including your newly tagged image. Then use push to upload it to the registry and pull to download the image.

    docker push quay.io/cjsimon/mynginx:latest

All tags of an images can be pulled by specifying the `-a` option 

    docker pull quay.io/cjsimon/mynginx:latest


  [1]: http://hub.docker.com
  [2]: https://docs.docker.com/engine/reference/commandline/login/#/credentials-store
  [3]: https://github.com/docker/docker/issues/7203

## Exposing a Port  in the Dockerfile
    EXPOSE <port> [<port>...]

[From Docker's documentation:](https://docs.docker.com/engine/reference/builder/#/expose)

> The `EXPOSE` instruction informs Docker that the container listens on the specified network ports at runtime. `EXPOSE` does not make the ports of the container accessible to the host. To do that, you must use either the `-p` flag to publish a range of ports or the `-P` flag to publish all of the exposed ports. You can expose one port number and publish it externally under another number.

# Example:

Inside your Dockerfile:

    EXPOSE 8765

To access this port from the host machine, include this argument in your `docker run` command:

    -p 8765:8765


## ENTRYPOINT and CMD seen as verb and parameter
Suppose you have a Dockerfile ending with

`ENTRYPOINT [ "nethogs"]
CMD ["wlan0"]`

if you build this image with a 

`docker built -t inspector .` 

launch the image built with such a Dockerfile with a command such as

`docker run -it --net=host --rm inspector`

,nethogs will monitor the interface named wlan0

Now if you want to monitor the interface eth0 (or wlan1, or ra1...), you will do something like 

`docker run -it --net=host --rm inspector eth0`

or

`docker run -it --net=host --rm inspector wlan1`

## Building using a proxy
Often when building a Docker image, the Dockerfile contains instructions that runs programs to fetch resources from the Internet (`wget` for example to pull a program binary build on GitHub for example). 

It is possible to instruct Docker to pass set set environment variables so that such programs perform those fetches through a proxy:

```
$ docker build --build-arg http_proxy=http://myproxy.example.com:3128 \
               --build-arg https_proxy=http://myproxy.example.com:3128 \
               --build-arg no_proxy=internal.example.com \
               -t test .
```

`build-arg` are environment variables which are available at build time only.

