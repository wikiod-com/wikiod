---
title: "Getting started with spring-cloud"
slug: "getting-started-with-spring-cloud"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting started with Cloud Config: Server setup
To externalise a distributed systems configuration Spring Cloud Config provides server and client-side support needed for externalising and centralising your configuration.

To get started quickly you could use [Spring Initializr][1] to bootstrap your server. Add the Config Server dependency to automatically generate a project with the needed dependencies. 

Or you could add the dependency manually to an existing Spring Cloud application.

    <dependency>
        <groupId>org.springframework.cloud</groupId>
        <artifactId>spring-cloud-config-server</artifactId>
    </dependency>

By default you can use a Git repository to store you configuration.
Defined in :

    spring.cloud.config.server.git.uri: file://${user.home}/config-repo

The default port to run a config server on is 8888.

    server.port: 8888

To enable the config server the application starting class needs to be annotated with `@EnableConfigServer`.

  [1]: http://start.spring.io/

## Getting started with Cloud Config: Client setup



To get started quickly you could use Spring Initializr to bootstrap your client. Add the Config Client to automatically generate a project with the needed dependencies. 

Or you could add the dependency manually to an existing Spring Cloud application.

    <dependency>
        <groupId>org.springframework.cloud</groupId>
        <artifactId>spring-cloud-starter-config</artifactId>
    </dependency>


Once the dependency is on the classpath Spring Cloud will try to connect to a Config Server on `localhost` to retrieve the configuration.

