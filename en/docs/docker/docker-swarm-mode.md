---
title: "Docker swarm mode"
slug: "docker-swarm-mode"
draft: false
images: []
weight: 9853
type: docs
toc: true
---

A swarm is a number of Docker Engines (or *nodes*) that deploy *services* collectively. Swarm is used to distribute processing across many physical, virtual or cloud machines.

## Syntax
- [Initialize a swarm](https://docs.docker.com/engine/reference/commandline/swarm_init/): docker swarm init [OPTIONS]

- [Join a swarm as a node and/or manager](https://docs.docker.com/engine/reference/commandline/swarm_join/):
 docker swarm join [OPTIONS] HOST:PORT 
- [Create a new service](https://docs.docker.com/engine/reference/commandline/service_create/): docker service create [OPTIONS] IMAGE [COMMAND] [ARG...]
- [Display detailed information on one or more services](https://docs.docker.com/engine/reference/commandline/service_inspect/): docker service inspect [OPTIONS] SERVICE [SERVICE...]
- [List services](https://docs.docker.com/engine/reference/commandline/service_ls/): docker service ls [OPTIONS]
- [Remove one or more services](https://docs.docker.com/engine/reference/commandline/service_rm/): docker service rm SERVICE [SERVICE...]
- [Scale one or multiple replicated services](https://docs.docker.com/engine/reference/commandline/service_scale/): docker service scale SERVICE=REPLICAS [SERVICE=REPLICAS...]
- [List the tasks of one or more services](https://docs.docker.com/engine/reference/commandline/service_ps/): docker service ps [OPTIONS] SERVICE [SERVICE...]
- [Update a service](https://docs.docker.com/engine/reference/commandline/service_update/): docker service update [OPTIONS] SERVICE

Swarm mode implements the following features:
- Cluster management integrated with Docker Engine
- Decentralized design
- Declarative service model
- Scaling
- Desired state reconciliation
- Multi-host networking
- Service discovery
- Load balancing
- Secure design by default
- Rolling updates

For more official Docker documentation regarding Swarm visit: [Swarm mode overview](https://docs.docker.com/engine/swarm/)

<br/>

# Swarm Mode CLI Commands
*Click on commands description for documentation*
<br/><br/>

<a href="https://docs.docker.com/engine/reference/commandline/swarm_init/">Initialize a swarm</a>

    docker swarm init [OPTIONS]

<a href="https://docs.docker.com/engine/reference/commandline/swarm_join/">Join a swarm as a node and/or manager</a>

    docker swarm join [OPTIONS] HOST:PORT

<a href="https://docs.docker.com/engine/reference/commandline/service_create/">Create a new service</a>

    docker service create [OPTIONS] IMAGE [COMMAND] [ARG...]

<a href="https://docs.docker.com/engine/reference/commandline/service_inspect/">Display detailed information on one or more services</a>

    docker service inspect [OPTIONS] SERVICE [SERVICE...]

<a href="https://docs.docker.com/engine/reference/commandline/service_ls/">List services</a>

    docker service ls [OPTIONS]

<a href="https://docs.docker.com/engine/reference/commandline/service_rm/">Remove one or more services</a>

    docker service rm SERVICE [SERVICE...]

<a href="https://docs.docker.com/engine/reference/commandline/service_scale/">Scale one or multiple replicated services</a>

    docker service scale SERVICE=REPLICAS [SERVICE=REPLICAS...]

<a href="https://docs.docker.com/engine/reference/commandline/service_ps/">List the tasks of one or more services</a>

    docker service ps [OPTIONS] SERVICE [SERVICE...]

<a href="https://docs.docker.com/engine/reference/commandline/service_update/">Update a service</a>

    docker service update [OPTIONS] SERVICE



## Create a swarm on Linux using docker-machine and VirtualBox
```
# Create the nodes
# In a real world scenario we would use at least 3 managers to cover the fail of one manager.
docker-machine create -d virtualbox manager
docker-machine create -d virtualbox worker1

# Create the swarm
# It is possible to define a port for the *advertise-addr* and *listen-addr*, if none is defined the default port 2377 will be used.
docker-machine ssh manager \
    docker swarm init \
    --advertise-addr $(docker-machine ip manager)
    --listen-addr $(docker-machine ip manager)

# Extract the Tokens for joining the Swarm
# There are 2 different Tokens for joining the swarm.
MANAGER_TOKEN=$(docker-machine ssh manager docker swarm join-token manager --quiet)
WORKER_TOKEN=$(docker-machine ssh manager docker swarm join-token worker --quiet)


# Join a worker node with the worker token
docker-machine ssh worker1 \
    docker swarm join \
    --token $WORKER_TOKEN \
    --listen-addr $(docker-machine ip worker1) \
    $(docker-machine ip manager):2377



```

## Node Availablility
Swarm Mode Node Availability:

 - Active means that the scheduler can assign tasks to a node.
 - Pause means the scheduler doesn’t assign new tasks to the node, but
   existing tasks remain running.
 - Drain means the scheduler doesn’t assign new tasks to the node. The
   scheduler shuts down any existing tasks and schedules them on an
   available node.

To change Mode Availability:

    #Following commands can be used on swarm manager(s)
    docker node update --availability drain node-1
    #to verify:
    docker node ls

## Promote or Demote Swarm Nodes
To promote a node or set of nodes, run `docker node promote` from a manager node:

    docker node promote node-3 node-2
    
    Node node-3 promoted to a manager in the swarm.
    Node node-2 promoted to a manager in the swarm.

To demote a node or set of nodes, run `docker node demote` from a manager node:

    docker node demote node-3 node-2
    
    Manager node-3 demoted in the swarm.
    Manager node-2 demoted in the swarm.



## Find out worker and manager join token
When automating the provisioning of new nodes to a swarm, you need to know what the right join token is for the swarm as well as the advertised address of the manager. You can find this out by running the following commands on any of the existing manager nodes:

```
# grab the ipaddress:port of the manager (second last line minus the whitespace)
export MANAGER_ADDRESS=$(docker swarm join-token worker | tail -n 2 |  tr -d '[[:space:]]')

# grab the manager and worker token
export MANAGER_TOKEN=$(docker swarm join-token manager -q)
export WORKER_TOKEN=$(docker swarm join-token worker -q)
```

The -q option outputs only the token. Without this option you get the full command for registering to a swarm.

Then on newly provisioned nodes, you can join the swarm using.

```
docker swarm join --token $WORKER_TOKEN $MANAGER_ADDRESS
```

## Hello world application
Usually you'd want to create a stack of services to form a replicated and orchestrated application.

A typical modern web application consists of a database, api, frontend and reverse proxy.

**Persistence**

Database needs persistence, so we need some filesystem which is shared across all the nodes in a swarm. It can be NAS, NFS server, GFS2 or anything else. Setting it up is out of scope here. Currently Docker doesn't contain and doesn't manage persistence in a swarm. This example assumes that there's `/nfs/` shared location mounted across all nodes.

**Network**

To be able to communicate with each other, services in a swarm need to be on the same network.

Choose an IP range (here `10.0.9.0/24`) and network name (`hello-network`) and run a command:

```
docker network create \
  --driver overlay \
  --subnet 10.0.9.0/24 \
  --opt encrypted \
  hello-network
```

**Database**

The first service we need is a database. Let's use postgresql as an example. Create a folder for a database in `nfs/postgres` and run this:

```
docker service create --replicas 1 --name hello-db \
       --network hello-network -e PGDATA=/var/lib/postgresql/data \
       --mount type=bind,src=/nfs/postgres,dst=/var/lib/postgresql/data \
       kiasaki/alpine-postgres:9.5
```

Notice that we've used `--network hello-network` and `--mount` options.

**API**

Creating API is out of scope of this example, so let's pretend you have an API image under `username/hello-api`.

```
docker service create --replicas 1 --name hello-api \
       --network hello-network \
       -e NODE_ENV=production -e PORT=80 -e POSTGRESQL_HOST=hello-db \
       username/hello-api
```

Notice that we passed a name of our database service. Docker swarm has an embedded round-robin DNS server, so API will be able to connect to database by using its DNS name.

**Reverse proxy**

Let's create nginx service to serve our API to an outer world. Create nginx config files in a shared location and run this:

```
docker service create --replicas 1 --name hello-load-balancer \
       --network hello-network \
       --mount type=bind,src=/nfs/nginx/nginx.conf,dst=/etc/nginx/nginx.conf \
       -p 80:80 \
       nginx:1.10-alpine
```

Notice that we've used `-p` option to publish a port. This port would be available to any node in a swarm.

## Leaving the Swarm
Worker Node:

    #Run the following on the worker node to leave the swarm.

    docker swarm leave
    Node left the swarm.

If the node has the *Manager* role, you will get a warning about maintaining the quorum of Managers. You can use --force to leave on the manager node:

    #Manager Node

    docker swarm leave --force
    Node left the swarm.

Nodes that left the Swarm will still show up in `docker node ls` output.

To remove nodes from the list:

    docker node rm node-2
    
    node-2

