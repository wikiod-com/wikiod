---
title: "Data Volumes and Data Containers"
slug: "data-volumes-and-data-containers"
draft: false
images: []
weight: 9899
type: docs
toc: true
---

## Data-Only Containers
**Data-only containers are obsolete and are now considered an anti-pattern!**

In the days of yore, before Docker's `volume` subcommand, and before it was possible to create named volumes, Docker deleted volumes when there were no more references to them in any containers. Data-only containers are obsolete because Docker now provides the ability to create named volumes, as well as much more utility via the various `docker volume` subcommand. Data-only containers are now considered an anti-pattern for this reason.

Many resources on the web from the last couple of years mention using a pattern called a "data-only container", which is simply a Docker container that exists only to keep a reference to a data volume around.

Remember that in this context, a "data volume" is a Docker volume which is not mounted from the host. To clarify, a "data volume" is a volume which is created either with the `VOLUME` Dockerfile directive, or using the `-v` switch on the command line in a `docker run` command, specifically with the format `-v /path/on/container`. Therefore a "data-only container" is a container whose only purpose is to have a data volume attached, which is used by the `--volumes-from` flag in a `docker run` command. For example:

    docker run -d --name "mysql-data" -v "/var/lib/mysql" alpine /bin/true

When the above command is run, a "data-only container" is created. It is simply an empty container which has a data volume attached. It was then possible to use this volume in another container like so:

    docker run -d --name="mysql" --volumes-from="mysql-data" mysql

The `mysql` container now has the same volume in it that is also in `mysql-data`. 

Because Docker now provides the `volume` subcommand and named volumes, this pattern is now obsolete and not recommended.

To get started with the `volume` subcommand and named volumes see  https://www.wikiod.com/docker/docker-data-volumes#Creating a named volume

## Creating a data volume
    docker run -d --name "mysql-1" -v "/var/lib/mysql" mysql

This command creates a new container from the `mysql` image. It also creates a new data volume, which it then mounts in the container at `/var/lib/mysql`. This volume helps any data inside of it persist beyond the lifetime of the container. That is to say, when a container is removed, its filesystem changes are also removed. If a database was storing data in the container, and the container is removed, all of that data is also removed. Volumes will persist a particular location even beyond when its container is removed.

It is possible to use the same volume in multiple containers with the `--volumes-from` command line option:

    docker run -d --name="mysql-2" --volumes-from="mysql-1" mysql

The `mysql-2` container now has the data volume from `mysql-1` attached to it, also using the path `/var/lib/mysql`.

