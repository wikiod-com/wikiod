---
title: "Docker Machine"
slug: "docker-machine"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

Remote management of multiple docker engine hosts.

``docker-machine`` manages remote hosts running Docker.

The ``docker-machine`` command line tool manages the full machine's life cycle using provider specific drivers. It can be used to select an "active" machine. Once selected, an active machine can be used as if it was the local Docker Engine.

## Get current Docker Machine environment info
_All these are shell commands._

`docker-machine env` to get the current default docker-machine configuration

`eval $(docker-machine env)` to get the current docker-machine configuration and set the current shell environment up to use this docker-machine with .

If your shell is set up to use a proxy, you can specify the --no-proxy option in order to bypass the proxy when connecting to your docker-machine:
`eval $(docker-machine env --no-proxy)`

If you have multiple docker-machines, you can specify the machine name as argument:
`eval $(docker-machine env --no-proxy machinename)`

## SSH into a docker machine
_All these are shell commands_

- If you need to log onto a running docker-machine directly, you can do that:

`docker-machine ssh` to ssh into the default docker-machine

`docker-machine ssh machinename` to ssh into a non-default docker-machine

- If you just want to run a single command, you can do so. To run `uptime` on the default docker-machine to see how long it's been running for, run `docker-machine ssh default uptime`


## Create a Docker machine
Using ``docker-machine`` is the best method to install Docker on a machine. It will automatically apply the best security settings available, including generating a unique pair of SSL certificates for mutual authentication and SSH keys.

To create a local machine using Virtualbox:

    docker-machine create --driver virtualbox docker-host-1

To install Docker on an existing machine, use the ``generic`` driver:

    docker-machine -D create -d generic --generic-ip-address 1.2.3.4 docker-host-2

The ``--driver`` option tells docker how to create the machine. For a list of supported drivers, see:

- [officially supported][1]
- [third party][2]

  [1]: https://docs.docker.com/v1.11/machine/drivers/
  [2]: https://github.com/docker/machine/blob/master/docs/AVAILABLE_DRIVER_PLUGINS.md

## List docker machines
Listing docker-machines will return the state, address and version of Docker of each docker machines.

     docker-machine ls

Will print something like:

    NAME             ACTIVE   DRIVER    STATE     URL                          SWARM   DOCKER    ERRORS
    docker-machine-1 -        ovh       Running   tcp://1.2.3.4:2376                   v1.11.2   
    docker-machine-2 -        generic   Running   tcp://1.2.3.5:2376                   v1.11.2   

To list running machines:

    docker-machine ls --filter state=running

To list error machines:

    docker-machine ls --filter state=

To list machines who's name starts with 'side-project-', use Golang filter:

    docker-machine ls --filter name="^side-project-"

To get only the list of machine's URLs:

    docker-machine ls --format '{{ .URL }}'

See https://docs.docker.com/machine/reference/ls/ for the full command reference.

## Upgrade a Docker Machine 
Upgrading a docker machine implies a downtime and may require planing. To upgrade a docker machine, run:

    docker-machine upgrade docker-machine-name

This command does not have options

## Get the IP address of a docker machine
 To get the IP address of a docker machine, you can do that with this command :

    docker-machine ip machine-name


  

