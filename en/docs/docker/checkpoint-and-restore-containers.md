---
title: "Checkpoint and Restore Containers"
slug: "checkpoint-and-restore-containers"
draft: false
images: []
weight: 9896
type: docs
toc: true
---

## Compile docker with checkpoint and restore enabled (ubuntu)
In order to compile docker its recommended you have at least **2 GB RAM**. Even with that it fails sometimes so its better to go for **4GB** instead.

1) make sure git and make is installed

       sudo apt-get install make git-core -y

2) install a new kernel (at least 4.2)

       sudo apt-get install linux-generic-lts-xenial

3) reboot machine to have the new kernel active

       sudo reboot

4) compile `criu` which is needed in order to run `docker checkpoint`
      
       sudo apt-get install libprotobuf-dev libprotobuf-c0-dev protobuf-c-compiler protobuf-compiler python-protobuf libnl-3-dev libcap-dev -y
       wget http://download.openvz.org/criu/criu-2.4.tar.bz2 -O - | tar -xj
       cd criu-2.4
       make
       make install-lib
       make install-criu

5) check if every requirement is fulfilled to run criu
      
       sudo criu check
      
6) compile experimental docker ( we need docker to compile docker)

       cd ~
       wget -qO- https://get.docker.com/ | sh
       sudo usermod -aG docker $(whoami)

- **At this point we have to logoff and login again to have a docker daemon. After relog continue with compile step**

      git clone https://github.com/boucher/docker
      cd docker
      git checkout docker-checkpoint-restore
      make #that will take some time - drink a coffee
      DOCKER_EXPERIMENTAL=1 make binary

7) We now have a compiled docker. Lets move the binaries. Make sure to replace `<version>` with the version installed

       sudo service docker stop
       sudo cp $(which docker) $(which docker)_ ; sudo cp ./bundles/latest/binary-client/docker-<version>-dev $(which docker)
       sudo cp $(which docker-containerd) $(which docker-containerd)_ ; sudo cp ./bundles/latest/binary-daemon/docker-containerd $(which docker-containerd)
       sudo cp $(which docker-containerd-ctr) $(which docker-containerd-ctr)_ ; sudo cp ./bundles/latest/binary-daemon/docker-containerd-ctr $(which docker-containerd-ctr)
       sudo cp $(which docker-containerd-shim) $(which docker-containerd-shim)_ ; sudo cp ./bundles/latest/binary-daemon/docker-containerd-shim $(which docker-containerd-shim)
       sudo cp $(which dockerd) $(which dockerd)_ ; sudo cp ./bundles/latest/binary-daemon/dockerd $(which dockerd)
       sudo cp $(which docker-runc) $(which docker-runc)_ ; sudo cp ./bundles/latest/binary-daemon/docker-runc $(which docker-runc)
       sudo service docker start

Dont worry - we backed up the old binaries. They are still there but with an underscore added to its names (`docker_`).

Congratulation you now have an experimental docker with the ability to checkpoint a container and restore it.

**Please note that experimental features are NOT ready for production**

## Checkpoint and Restore a Container
    # create docker container
    export cid=$(docker run -d --security-opt seccomp:unconfined busybox /bin/sh -c 'i=0; while true; do echo $i; i=$(expr $i + 1); sleep 1; done')

    # container is started and prints a number every second
    # display the output with 
    docker logs $cid

    # checkpoint the container
    docker checkpoint create $cid checkpointname

    # container is not running anymore
    docker np

    # lets pass some time to make sure

    # resume container
    docker start $cid --checkpoint=checkpointname

    # print logs again
    docker logs $cid

