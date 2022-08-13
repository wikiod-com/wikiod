---
title: "Managing images"
slug: "managing-images"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Syntax
- docker images [OPTIONS] [REPOSITORY[:TAG]]
- docker inspect [OPTIONS] CONTAINER|IMAGE [CONTAINER|IMAGE...]
- docker pull [OPTIONS] NAME[:TAG|@DIGEST]
- docker rmi [OPTIONS] IMAGE [IMAGE...]
- docker tag [OPTIONS] IMAGE[:TAG] [REGISTRYHOST/][USERNAME/]NAME[:TAG]

## Listing locally downloaded images
```
$ docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
hello-world         latest              693bce725149        6 days ago          967 B
postgres            9.5                 0f3af79d8673        10 weeks ago        265.7 MB
postgres            latest              0f3af79d8673        10 weeks ago        265.7 MB
```

## Fetching an image from Docker Hub
Ordinarily, images are pulled automatically from [Docker Hub](http://hub.docker.com/). Docker will attempt to pull any image from Docker Hub that doesn't already exist on the Docker host. For example, using `docker run ubuntu` when the `ubuntu` image is not already on the Docker host will cause Docker to initiate a pull of the latest `ubuntu` image. It is possible to pull an image separately by using `docker pull` to manually fetch or update an image from Docker Hub.

    docker pull ubuntu
    docker pull ubuntu:14.04

Additional options for pulling from a different image registry or pulling a specific version of an image exist. Indicating an alternate registry is done using the full image name and optional version. For example, the following command will attempt to pull the `ubuntu:14.04` image from the `registry.example.com` registry:

    docker pull registry.example.com/username/ubuntu:14.04

## Referencing images
Docker commands which take the name of an image accept four different forms:

| Type       | Example |
| ---------- | ------- |
| Short ID | `693bce725149` |
| Name     | `hello-world` *(defaults to `:latest` tag)* |
| Name+tag | `hello-world:latest` |
| Digest   | `hello-world@sha256:e52be8ffeeb1f374f440893189cd32f44cb166650e7ab185fa7735b7dc48d619` |

**Note:** You can only refer to an image by its digest if that image was originally pulled using that digest. To see the digest for an image (if one is available) run `docker images --digests`.

## Removing Images
The `docker rmi` command is used to remove images:

    docker rmi <image name>

The full image name must be used to remove an image. Unless the image has been tagged to remove the registry name, it needs to be specified. For example:

    docker rmi registry.example.com/username/myAppImage:1.3.5

It is also possible to remove images by their ID instead:

    docker rmi 693bce725149

As a convenience, it is possible to remove images by their image ID by specifying only the first few characters of the image ID, as long as the substring specified is unambiguous:

    docker rmi 693

> **Note:** Images can be removed even if there are existing containers that use that image; docker rmi simply "untags" the image. 

If no containers are using an image it is garbage-collected. If a container uses an image, the image will be garbage-collected once all the containers using it are removed. For example:

    $ docker ps -a
    CONTAINER ID        IMAGE               COMMAND             CREATED                  STATUS                     PORTS               NAMES
    5483657ee07b        hello-world         "/hello"            Less than a second ago   Exited (0) 2 seconds ago                       small_elion
    
    $ docker rmi hello-world
    Untagged: hello-world:latest
    
    $ docker ps -a
    CONTAINER ID        IMAGE               COMMAND             CREATED                  STATUS                      PORTS               NAMES
    5483657ee07b        693bce725149        "/hello"            Less than a second ago   Exited (0) 12 seconds ago                       small_elion

**Remove All Images With No Started Containers**

To remove all local images that have no started containers, you can provide a listing of the images as a parameter:

    docker rmi $(docker images -qa)

**Remove All Images**

If you want to remove images regardless of whether or not they have a started container use the force flag (`-f`):

    docker rmi -f $(docker images -qa)

**Remove Dangling Images**

If an image is not tagged and not being used by any container, it is 'dangling' and may be removed like this:

    docker images -q --no-trunc -f dangling=true | xargs -r docker rmi

## Search the Docker Hub for images
You can search [Docker Hub](https://hub.docker.com/) for images by using the [search](https://docs.docker.com/engine/reference/commandline/search/) command:

    docker search <term>

For example:

    $ docker search nginx
    NAME                      DESCRIPTION                                     STARS     OFFICIAL   AUTOMATED
    nginx                     Official build of Nginx.                        3565      [OK]
    jwilder/nginx-proxy       Automated Nginx reverse proxy for docker c...   717                  [OK]
    richarvey/nginx-php-fpm   Container running Nginx + PHP-FPM capable ...   232                  [OK]
    ...

## Inspecting images
```
docker inspect <image>
```

The output is in JSON format. You can use `jq` command line utility to parse and print only the desired keys.

```
docker inspect <image> | jq -r '.[0].Author'
```

The above command will shows author name of the images.

## Tagging images
Tagging an image is useful for keeping track of different image versions:

    docker tag ubuntu:latest registry.example.com/username/ubuntu:latest

Another example of tagging:

    docker tag myApp:1.4.2 myApp:latest
    docker tag myApp:1.4.2 registry.example.com/company/myApp:1.4.2

## Saving and loading Docker images
    docker save -o ubuntu.latest.tar ubuntu:latest

This command will save the `ubuntu:latest` image as a tarball archive in the current directory with the name `ubuntu.latest.tar`. This tarball archive can then be moved to another host, for example using `rsync`, or archived in storage.

Once the tarball has been moved, the following command will create an image from the file:

    docker load -i /tmp/ubuntu.latest.tar

Now it is possible to create containers from the `ubuntu:latest` image as usual.

