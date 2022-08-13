---
title: "Service Container"
slug: "service-container"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

A Symfony application is typically composed of a lot of objects that perform different tasks, such as repositories, controllers, mailers, etc. In Symfony, these objects are called **services**, and are defined in `app/config/services.yml` or in one of the installed bundles.

The **Service Container** knows how to instantiate these services, and keeps a reference of them so they don't have to be instantiated twice. If a service has dependencies it will instantiate those too.

## Retrieve a service from the container
    $logger = $container->get('logger');

This will fetch the service with the service ID "logger" from the container, an object that implements `Psr\Log\LoggerInterface`.

