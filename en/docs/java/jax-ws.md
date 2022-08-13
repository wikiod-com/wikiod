---
title: "JAX-WS"
slug: "jax-ws"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Basic Authentication
The way to do a JAX-WS call with basic authentication is a little unobvious.

Here is an example where `Service` is the service class representation and `Port` is the service port you want to access.

    Service s = new Service();
    Port port = s.getPort();
    
    BindingProvider prov = (BindingProvider)port;
    prov.getRequestContext().put(BindingProvider.USERNAME_PROPERTY, "myusername");
    prov.getRequestContext().put(BindingProvider.PASSWORD_PROPERTY, "mypassword");
    
    port.call();



