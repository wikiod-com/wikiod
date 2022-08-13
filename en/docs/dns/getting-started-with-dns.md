---
title: "Getting started with dns"
slug: "getting-started-with-dns"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Performing name lookups in C
The `getaddrinfo()` function is the recommended POSIX function for interfacing with the system resolver.  Depending on system configuration it will perform name lookups in the DNS, `/etc/hosts`, mDNS, etc.

It is preferred over the deprecated `gethostbyname()` family of functions because it supports both IPv4 and IPv6 addressing, and can also perform service name lookups at the same time (e.g. mapping `http` to port 80)

    #include <sys/types.h>
    #include <sys/socket.h>
    #include <netdb.h>

    ...

    struct addrinfo hints;
    struct addrinfo *result;
    int r;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;           // allow IPv4 or IPv6
    hints.ai_socktype = SOCK_STREAM;       // make a stream (TCP) connection

    r = getaddrinfo(hostname, "http", &hints, &result);
    if (r != 0) {
        fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(r));
    } else {
        // iterate over the linked list
        for (struct addrinfo *rp = result; rp != NULL; rp = rp->ai_next) {
            // use rp fields to create a socket and connect to it 
        }
        freeaddrinfo(result);
    }


