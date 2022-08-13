---
title: "Restricting container network access"
slug: "restricting-container-network-access"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

Example docker networks that blocks traffic. Use as the network when starting the container with `--net` or `docker network connect`.

## Block access to LAN and out
    docker network create -o "com.docker.network.bridge.enable_ip_masquerade"="false" lan-restricted

* Blocks
  * Local LAN
  * Internet
* Does not block
  * Host running docker daemon (example access to `10.0.1.10:22`)

## Block access to other containers
    docker network create -o "com.docker.network.bridge.enable_icc"="false" icc-restricted

* Blocks
  * Containers accessing other containers on the same `icc-restricted` network.
* Does not block
  * Access to host running docker daemon
  * Local LAN
  * Internet

## Block access from containers to the local host running docker daemon
    iptables -I INPUT -i docker0 -m addrtype --dst-type LOCAL -j DROP
* Blocks
  * Access to host running docker daemon
* Does not block
  * Container to container traffic
  * Local LAN
  * Internet
  * Custom docker networks that doesn't use `docker0`



## Block access from containers to the local host running docker daemon (custom network)
    docker network create --subnet=192.168.0.0/24 --gateway=192.168.0.1 --ip-range=192.168.0.0/25 local-host-restricted
    iptables -I INPUT -s 192.168.0.0/24 -m addrtype --dst-type LOCAL -j DROP

Creates a network called `local-host-restricted` which which:
* Blocks
  * Access to host running docker daemon
* Does not block
  * Container to container traffic
  * Local LAN
  * Internet
  * Access originating from other docker networks

Custom networks have bridge names like `br-15bbe9bb5bf5`, so we uses it's subnet instead.

