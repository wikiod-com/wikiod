---
title: "Docker network"
slug: "docker-network"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## How to find the Container's host ip
You need to find out the IP address of the container running in the host so you can, for example, connect to the web server running in it.

`docker-machine` is what is used on MacOSX and Windows.

Firstly, list your machines:

    $ docker-machine ls
    
    NAME      ACTIVE   DRIVER       STATE     URL                         SWARM
    default   *        virtualbox   Running   tcp://192.168.99.100:2376

Then select one of the machines (the default one is called default) and:

    $ docker-machine ip default
    
    192.168.99.100

## Creating a Docker network
    docker network create app-backend

This command will create a simple bridged network called `appBackend`. No containers are attached to this network by default.

## Listing Networks
    docker network ls

This command lists all networks that have been created on the local Docker host. It includes the default bridge `bridge` network, the host `host` network, and the null `null` network. All containers by default are attached to the default bridge `bridge` network.

## Add container to network
    docker network connect app-backend myAwesomeApp-1

This command attaches the `myAwesomeApp-1` container to the `app-backend` network. When you add a container to a user-defined network, the embedded DNS resolver (which is not a full-featured DNS server, and is not exportable) allows each container on the network to resolve each other container on the same network. This simple DNS resolver is not available on the default bridge `bridge` network.

## Detach container from network
    docker network disconnect app-backend myAwesomeApp-1

This command detaches the `myAwesomeApp-1` container from the `app-backend` network. The container will no longer be able to communicate with other containers on the network it has been disconnected from, nor use the embedded DNS resolver to look up other containers on the network it has been detached from.

## Remove a Docker network
    docker network rm app-backend

This command removes the user-defined `app-backend` network from the Docker host. All containers on the network not otherwise connected via another network will lose communication with other containers. It is not possible to remove the default bridge `bridge` network, the `host` host network, or the `null` null network.

## Inspect a Docker network
    docker network inspect app-backend

This command will output details about the `app-backend` network.

The of the output of this command should look similar to:

    [
        {
            "Name": "foo",
            "Id": "a0349d78c8fd7c16f5940bdbaf1adec8d8399b8309b2e8a969bd4e3226a6fc58",
            "Scope": "local",
            "Driver": "bridge",
            "EnableIPv6": false,
            "IPAM": {
                "Driver": "default",
                "Options": {},
                "Config": [
                    {
                        "Subnet": "172.18.0.0/16",
                        "Gateway": "172.18.0.1/16"
                    }
                ]
            },
            "Internal": false,
            "Containers": {},
            "Options": {},
            "Labels": {}
        }
    ]


