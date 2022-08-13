---
title: "Packages and Ports management"
slug: "packages-and-ports-management"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Tips:

- Remember to always check the `/usr/ports/UPDATING` file before upgrading. There might be some significant changes in programs you use or in their configuration which will break your current setup. 

## Building and installing software
If you have found your software in the ports tree, now its the time to build it.

# Simple build and install with manual configuration

    cd /usr/ports/www/apache24
    make
    make install

# Simple build and install with automatic configuration

    cd /usr/ports/www/apache24
    make BATCH=yes
    make install

## Getting Ports tree
# Portsnap

    portsnap fetch
    portsnap extract

## updating ports tree with portsnap

    portsnap update

## schedule cron job for daily updates

    0 3 * * * root /usr/sbin/portsnap cron

# SVN

## head

    cd /usr/ports
    svnlite checkout https://svnweb.freebsd.org/ports/head .

## quaterly 

FreeBSD Ports team freeze ports tree every 3 months. To get this ports tree you can use ports branches:

    cd /usr/ports
    svnlite checkout https://svnweb.freebsd.org/ports/branches/2016Q4 .

# Tarball (http or ftp)

    cd /usr/ports
    fetch http://ftp.freebsd.org/pub/FreeBSD/releases/amd64/11.0-RELEASE/ports.txz
    tar xJvf ports.txz

# Git

    git clone https://github.com/freebsd/freebsd-ports

## Searching software
# keyword search

    cd /usr/ports
    make search key=apache

# name search

    cd /usr/ports
    make search name=apache24

# Using fresports

Official FreeBSD ports website (http://freshports.org/) give you a nice way to find ports and all information concerning it.

## Configuring software sources
If you want custom configuration from ports, you can configure it before building it `make config`. All ports configuration are stored in `/var/db/ports/${CATEGORY_NAME}/options` as makefile.

# Configuring `www/apache24`

    cd /usr/ports/www/apache24
    make config
    make
    make install

This configuration will be saved in `/var/db/ports/www_apache24/options`.

## Packaging
# Manual packaging

You can make your own package based on ports. 

    cd /usr/ports/www/apache24
    make package BATCH=yes

This command will store your package in `/usr/ports/packages/All`.

# Using `poudriere`

[`poudriere`][1] is currently the official package builder for FreeBSD. 

## Installing poudriere

    pkg install poudriere
    # or
    cd /usr/ports/ports-mgmt/poudriere
    make
    make install

## Configuring poudriere

`poudriere` configuration is stored in `/usr/local/etc/poudriere.conf` and `/usr/local/etc/poudriere.d`

## Deploying poudriere jail

    poudriere jail -c -j myjail

## Updating poudriere jail

    poudriere jail -u -j myjail

## Deploying poudriere ports tree

    poudriere ports -c -p myports

## Updating poudriere ports tree

    poudriere ports -u -p myports

## Bulk build

    poudriere bulk -j myjail -p myports www/apache24

  [1]: https://www.freebsd.org/cgi/man.cgi?query=poudriere&apropos=0&sektion=0&manpath=FreeBSD%20Ports%2010.3-RELEASE&arch=default&format=html

