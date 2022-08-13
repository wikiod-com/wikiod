---
title: "Getting started with varnish"
slug: "getting-started-with-varnish"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
The following are instructions to setup latest version of Varnish on various Linux distros.

## CentOS 7 ##

    curl -s https://packagecloud.io/install/repositories/varnishcache/varnish5/script.rpm.sh | sudo bash

## Ubuntu ##

    apt-get install apt-transport-https
    curl https://repo.varnish-cache.org/GPG-key.txt | apt-key add -
    echo "deb https://repo.varnish-cache.org/ubuntu/ trusty varnish-4.1" \
      >> /etc/apt/sources.list.d/varnish-cache.list
    apt-get update
    apt-get install varnish

## Debian ##

    apt-get install apt-transport-https
    curl https://repo.varnish-cache.org/GPG-key.txt | apt-key add -
    echo "deb https://repo.varnish-cache.org/debian/ jessie varnish-4.1"\
      >> /etc/apt/sources.list.d/varnish-cache.list
    apt-get update
    apt-get install varnish

## Varnish VCL
Varnish controls and manipulates HTTP requests using Varnish Configuration Language (VCL). The following snippet of VCL removes cookie from incoming requests to /images subdirectory:

    sub vcl_recv {
        if (req.url ~ "^/images") {
            unset req.http.cookie;
        }
    }

