---
title: "run consul in docker 1.12 swarm"
slug: "run-consul-in-docker-112-swarm"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Run consul in a docker 1.12 swarm
This relies on the official consul docker image to run consul in clustered mode in a docker swarm with new swarm mode in Docker 1.12. This example is based on http://qnib.org/2016/08/11/consul-service/. Briefly the idea is to use two docker swarm services that talk to each other. This solves the problem that you cannot know the ips of individual consul containers up front and allows you to rely on docker swarm's dns.

This assumes you already have a running docker 1.12 swarm cluster with at least three nodes.

You may want to configure a log driver on your docker daemons so that you can see what is happening. I used the syslog driver for this: set the  `--log-driver=syslog` option on dockerd.


First create an overlay network for consul:
    
    docker network create consul-net -d overlay
    
Now bootstrap the cluster with just 1 node (default --replicas is 1):
    
    docker service create --name consul-seed \
      -p 8301:8300 \
      --network consul-net \
      -e 'CONSUL_BIND_INTERFACE=eth0' \
      consul agent -server -bootstrap-expect=3  -retry-join=consul-seed:8301 -retry-join=consul-cluster:8300
    
You should now have a 1 node cluster. Now bring up the second service:

    docker service create --name consul-cluster \
      -p 8300:8300 \
      --network consul-net \
      --replicas 3 \
      -e 'CONSUL_BIND_INTERFACE=eth0' \
      consul agent -server -retry-join=consul-seed:8301 -retry-join=consul-cluster:8300 

You should now have a four node consul cluster. You can verify this by running on any of the docker containers:

    docker exec <containerid> consul members



