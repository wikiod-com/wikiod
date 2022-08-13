---
title: "Docker in Docker"
slug: "docker-in-docker"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Jenkins CI Container using Docker
This chapter describes how to set up a Docker Container with Jenkins inside, which is capable of sending Docker commands to the Docker installation (the Docker Daemon) of the Host. Effectively using Docker in Docker. To achieve this, we have to build a custom Docker Image which is based on an arbitrary version of the official Jenkins Docker Image. The Dockerfile (The Instruction how to build the Image) looks like this:
```
FROM jenkins

USER root

RUN cd /usr/local/bin && \
curl https://master.dockerproject.org/linux/amd64/docker > docker  && \
chmod +x docker  && \
groupadd -g 999 docker && \
usermod -a -G docker jenkins
    
USER Jenkins
```
This Dockerfile builds an Image which contains the Docker client binaries this client is used to communicate with a Docker Daemon. In this case the Docker Daemon of the Host. The `RUN` statement in this file also creates an UNIX usergroup with the UID 999 and adds the user Jenkins to it. Why exactly this is necessary is described in the further chapter.
With this Image we can run a Jenkins server which can use Docker commands, but if we just run this Image the Docker client we installed inside the image cannot communicate with the Docker Daemon of the Host. These two components do communicate via a UNIX Socket `/var/run/docker.sock`. On Unix this is a file like everything else, so we can easily mount it inside the Jenkins Container. This is done with the command `docker run -v /var/run/docker.sock:/var/run/docker.sock --name jenkins MY_CUSTOM_IMAGE_NAME`. But this mounted file is owned by `docker:root`and because of this does the Dockerfile create this group with a well know UID and adds the Jenkins user to it. Now is the Jenkins Container really capable of running and using Docker.
In production the run command should also contain `-v jenkins_home:/var/jenkins_home` to backup the Jenkins_home directory and of course a port-mapping to access the server over network.

