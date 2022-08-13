---
title: "Running services"
slug: "running-services"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Creating a more advanced service
In the following example we will create a service with the name *visualizer*. We will specify a custom label and remap the internal port of the service from 8080 to 9090. 
In addition we will bind mount an external directory of the host into the service.

```
docker service create \
        --name=visualizer \
        --label com.my.custom.label=visualizer \
        --publish=9090:8080 \
        --mount type=bind,source=/var/run/docker.sock,target=/var/run/docker.sock \
        manomarks/visualizer:latest
```



## Creating a simple service
This simple exampe will create a hello world web service that will listen on the port 80.
```
docker service create \
    --publish 80:80 \
    tutum/hello-world
```

## Removing a service
This simple example will remove the service with name "visualizer":

    docker service rm visualizer



## Scaling a service
This example will scale the service to 4 instances:

    docker service scale visualizer=4

In Docker Swarm Mode we do not stop a service. We scale it down to zero:

    docker service scale visualizer=0

