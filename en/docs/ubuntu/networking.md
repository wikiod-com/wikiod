---
title: "Networking"
slug: "networking"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Set proxy from CLI
If you need to add proxy of your network in a reboot-persistent way, edit:

    sudo vim /etc/environment

Press `i` and after the row with PATH variable insert:

    http_proxy=http://<proxy_server>:<port>/
    https_proxy=http://<proxy_server>:<port>/
    ftp_proxy=http://<proxy_server>:<port>/

Then press `ESC`, enter `:` and write `wq!` for save and exit from vim.

If you need to use wget, add the same three rows to (eventually uncommenting them inside the file):

    sudo vim /etc/wgetrc



## Show network interfaces
If you want to show active network interfaces, type:

    ifconfig

If you want to show also network interfaces that are down, type:

    ifconfig -a

## Configure network interface from CLI
You could use `ethtool`, but they are not going to be reboot persistent.
If you want to achieve this, edit the following file:

    sudo vim /etc/network/interfaces

And edit the file with needed informations:

    auto <interface_name>
    iface <interface_name> inet static
    address <ip_address>
    netmask <netmask>
    network <network>
    gateway <gateway>
    dns_nameservers <server_1> <server_2> <server_n>
    dns_naesearch <server_name>

Then restart interface to make changes running:

    sudo ifdown <interface_name> && sudo ifup <interface_name>

