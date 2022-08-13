---
title: "Connecting Containers"
slug: "connecting-containers"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| --------- | ------- |
| `tty:true`| In docker-compose.yml, the `tty: true` flag keeps the container's `sh` command running waiting for input. |


The `host` and `bridge` network drivers are able to connect containers on a single docker host.  To allow containers to communicate beyond one machine, create an overlay network. Steps to create the network depend on how your docker hosts are managed.

* Swarm Mode: [`docker network create --driver overlay`][1]
* [docker/swarm][2]: requires an [external key-value store][3]


  [1]: https://docs.docker.com/engine/userguide/networking/#/an-overlay-network-with-docker-engine-swarm-mode
  [2]: https://github.com/docker/swarm
  [3]: https://docs.docker.com/engine/userguide/networking/#/an-overlay-network-with-an-external-key-value-store

## Docker network
Containers in the same docker network have access to exposed ports.

    docker network create sample
    docker run --net sample --name keys consul agent -server -client=0.0.0.0 -bootstrap

[Consul's Dockerfile][1] exposes `8500`, `8600`, and several more ports. To demonstrate, run another container in the same network:

    docker run --net sample -ti alpine sh
    / # wget -qO- keys:8500/v1/catalog/nodes

Here the consul container is resolved from `keys`, the name given in the first command.  Docker [provides dns resolution][2] on this network, to find containers by their `--name`.


  [1]: https://github.com/hashicorp/docker-consul/blob/9a59dc1a87adc164b72ac67bc9e4364a3fc4138d/0.6/Dockerfile#L60
  [2]: https://docs.docker.com/engine/userguide/networking/configure-dns/

## Docker-compose
Networks can be specified in a compose file (v2). By default all the containers are in a shared network. 

Start with this file: `example/docker-compose.yml`:

    version: '2'
    services:
      keys:
        image: consul
        command: agent -server -client=0.0.0.0 -bootstrap
      test:
        image: alpine
        tty: true
        command: sh

Starting this stack with `docker-compose up -d` will create a network named after the parent directory, in this case `example_default`.  Check with `docker network ls`

     > docker network ls
    NETWORK ID          NAME                    DRIVER              SCOPE
    719eafa8690b        example_default         bridge              local

Connect to the alpine container to verify the containers can resolve and communicate:

     > docker exec -ti example_test_1 sh
    / # nslookup keys
    ...
    / # wget -qO- keys:8500/v1/kv/?recurse
    ...

A compose file can have a `networks:` top level section to specify the network name, driver, and other options from the [docker network][1] command.


  [1]: https://www.wikiod.com/docker/docker-network

## Container Linking
The docker `--link` argument, and `link:` sections docker-compose make *aliases* to other containers.

    docker network create sample
    docker run -d --net sample --name redis redis

With link either the original name or the mapping will resolve the redis container.

    > docker run --net sample --link redis:cache -ti python:alpine sh -c "pip install redis && python"
    >>> import redis
    >>> r = redis.StrictRedis(host='cache')
    >>> r.set('key', 'value')
    True

----------


Before docker `1.10.0` container linking also setup network connectivity - behavior now provided by docker network. Links in later versions only provide [`legacy`][1] effect on the default bridge network. 


  [1]: https://docs.docker.com/engine/userguide/networking/default_network/dockerlinks/

