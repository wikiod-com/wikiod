---
title: "Concept of Docker Volumes"
slug: "concept-of-docker-volumes"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

People new to Docker often don't realize that Docker filesystems are temporary by default. If you start up a Docker image you'll get a container that on the surface behaves much like a virtual machine. You can create, modify, and delete files. However, unlike a virtual machine, if you stop the container and start it up again, all your changes will be lost -- any files you previously deleted will now be back, and any new files or edits you made won't be present.

Volumes in docker containers allow for persistent data, and for sharing host-machine data inside a container.

## A) Launch a container with a volume
    [root@localhost ~]# docker run -it -v  /data  --name=vol3   8251da35e7a7 /bin/bash
    root@d87bf9607836:/# cd /data/
    root@d87bf9607836:/data# touch abc{1..10}
    root@d87bf9607836:/data# ls

abc1  abc10  abc2  abc3  abc4  abc5  abc6  abc7  abc8  abc9

## B) Now press [cont +P+Q] to move out from container without terminating the container  checking for container that is running
    [root@localhost ~]# docker ps
CONTAINER ID        IMAGE               COMMAND             CREATED              STATUS              PORTS               NAMES
d87bf9607836        8251da35e7a7        "/bin/bash"         About a minute ago   Up 31 seconds                           vol3
[root@localhost ~]#

## C) Run 'docker inspect' to check out more info about the volume
    [root@localhost ~]# docker inspect  d87bf9607836

"Mounts": [
        {
            "Name": "cdf78fbf79a7c9363948e133abe4c572734cd788c95d36edea0448094ec9121c",
            "Source": "/var/lib/docker/volumes/cdf78fbf79a7c9363948e133abe4c572734cd788c95d36edea0448094ec9121c/_data",
            "Destination": "/data",
            "Driver": "local",
            "Mode": "",
            "RW": true

## D) You can attach a running containers volume to another containers
    [root@localhost ~]# docker run -it  --volumes-from  vol3  8251da35e7a7  /bin/bash

    root@ef2f5cc545be:/# ls

bin  boot  data  dev  etc  home  lib  lib64  media  mnt  opt  proc  root  run  sbin  srv sys  tmp  usr  var

`root@ef2f5cc545be:/# ls`  /data
abc1  abc10  abc2  abc3  abc4  abc5  abc6  abc7  abc8  abc9


## E) You can also mount you base directory inside container
    [root@localhost ~]# docker run -it  -v  /etc:/etc1 8251da35e7a7 /bin/bash

Here:  /etc is host machine directory  and  /etc1 is the target inside container

