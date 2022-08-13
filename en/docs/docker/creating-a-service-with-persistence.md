---
title: "Creating a service with persistence"
slug: "creating-a-service-with-persistence"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
 - docker volume create --name <volume_name> # Creates a volume called <volume_name>
 - docker run -v <volume_name>:<mount_point> -d crramirez/limesurvey:latest # Mount the <volume_name> volume in <mount_point> directory in the container


## Parameters
| Parameter | Details |
| --------- | ------- |
| --name <volume_name> | Specify the volume name to be created |
| -v <volume_name>:<mount_point> | Specify where the named volume will be mounted in the container | 


Persistence is created in docker containers using volumes. Docker have many ways to deal with volumes.
Named volumes are very convenient by:
 - They persist even when the container is removed using the -v option.
 - The only way to delete a named volume is doing an explicit call to docker volume rm
 - The named volumes can be shared among container without linking or --volumes-from option.
 - They don't have permission issues that host mounted volumes have.
 - They can be manipulated using docker volume command.


## Backup a named volume content
We need to create a container to mount the volume. Then archive it and download the archive to our host.

Let's create first a data volume with some data:

    docker volume create --name=data
    echo "Hello World" |  docker run -i --rm=true -v data:/data ubuntu:trusty tee /data/hello.txt

Let's backup the data:

    docker run -d --name backup -v data:/data ubuntu:trusty tar -czvf /tmp/data.tgz /data
    docker cp backup:/tmp/data.tgz data.tgz
    docker rm -fv backup

Let's test:

    tar -xzvf data.tgz
    cat data/hello.txt



## Persistence with named volumes
Persistence is created in docker containers using volumes. Let's create a Limesurvey container and persist the database, uploaded content and configuration in a named volume:

<!-- language: lang-bash -->

    docker volume create --name mysql
    docker volume create --name upload
    
    docker run -d --name limesurvey -v mysql:/var/lib/mysql -v upload:/app/upload -p 80:80 crramirez/limesurvey:latest



