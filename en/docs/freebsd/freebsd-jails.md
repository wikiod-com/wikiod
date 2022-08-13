---
title: "FreeBSD Jails"
slug: "freebsd-jails"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Deploying jail
A jail is simply a `chroot` with strong isolation. So, if you want to create jail, you simply need to create an alternative root and starting a new jail in it. 

# Simple jail deployment from binaries

<!-- language: lang-sh -->

    # create our alternative root path
    JAILROOT="/path/to/my/jail"
    mkdir -p "${JAILROOT}"
    cd "${JAILROOT}"

    # get distribution from freebsd repository
    fetch http://ftp.freebsd.org/pub/FreeBSD/releases/amd64/11.0-RELEASE/base.txz

    # extract it in our alternative root
    tar xJvf base.txz

    # now we can launch our jail
    jail -c name=simplejail path=${JAILROOT} 

    # to check if jail is up and running we use jls
    jls

    # now we can enter in our new jail
    jexec simplejail sh

# Simple jail deployment from source

<!-- language: lang-sh -->

    # create our alternative root path
    JAILROOT="/path/to/my/jail"
    mkdir -p "${JAILROOT}"

    # we need to build binaries from source...
    cd /usr/src
    make buildworld

    # ... and install it in our alternative path
    make installworld DESTDIR=${JAILROOT}

    # now we can launch our jail
    jail -c name=simplejail path=${JAILROOT} 

    # to check if jail is up and running we use jls
    jls

    # now we can enter in our new jail
    jexec simplejail sh

# Simple thin jail deployment

Thin jail is simply a jail with shared read-only alternative root mounted with [nullfs][1].

## Initializing our environment 

<!-- language: lang-sh -->

    # making our shared alternative root
    SHARED_ROOT=/path/to/your/shared/root
    mkdir -p "${SHARED_ROOT}"

    # making our jail root
    JAIL_ROOT=/path/to/your/jail/root
    mkdir -p "${JAIL_ROOT}"

## downloading sources

<!-- language: lang-sh -->

    # to initialize our shared root, we can use 
    # all method described above. Here, we will use
    # simple binary initialization from official
    # repository
    cd "${SHARED_ROOT}"

    # get distribution from freebsd repository
    fetch http://ftp.freebsd.org/pub/FreeBSD/releases/amd64/11.0-RELEASE/base.txz

    # extract it in our alternative root
    tar xJvf base.txz

## Initializing our thin jail

<!-- language: lang-sh -->

    # now we need to initialize our dedicated
    # jail root
    cd "${JAIL_ROOT}"
    mkdir base

    # we make symbolic link pointing to 
    # files stored in read-only directory
    for link in bin boot lib libexec rescue sbin
    do
      ln -s ${link} /base/${link}
    done

    # we do same thing with directory in /usr
    for link in bin include lib lib32 libdata libexec sbin share
    do
      ln -s usr/${link} /base/usr/${link}
    done

    # now we are ready to start our jail!
    jail -c name=thinjail path="${JAIL_ROOT}" \
            mount="${SHARED_ROOT} ${JAIL_ROOT} nullfs ro 0 0"

    # check if our thin jail is ok...
    jls

    # we can now grab in it!
    jexec thinjail sh

  [1]: https://www.freebsd.org/cgi/man.cgi?query=nullfs

## Networking and Jails
FreeBSD jails can have fine grained networking configuration. By default, every jails use the same network configuration than host.

# Removing network support

<!-- language: lang-sh -->

    jail -c name="nonetwork" path="/path/to/your/jail" ip4=disable ip6=disable

# Allowing only IPv4 networking

<!-- language: lang-sh -->

    jail -c name="onlyipv4" path="/path/to/your/jail" ip4=inherit ip6=disable

# Allowing only IPv6 networking

<!-- language: lang-sh -->

    jail -c name="onlyipv6" path="/path/to/your/jail" ip4=disable ip6=inherit

# Dedicated network stack (VNET)

`VNET` is recent feature allowing jail to have its own network stack. Doing this configuration need to add routing feature to the host. `VIMAGE` option is required in host kernel.

<!-- language: lang-sh -->

    # starting our own jail with vnet
    jail -c name="vnetjail" path="/path/to/your/jail" vnet=new

    # we need a bridge...
    ifconfig bridge0 create

    # a pair of ethernet interface...
    ifconfig epair0 create

    # and interconnecting epair, jail and bridge
    ifconfig epair0b vnet vnetjail
    ifconfig bridge0 add epair0a
    ifconfig bridge0 add ${your_external_interface}

