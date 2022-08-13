---
title: "Docker-compose multi-container example with default network"
slug: "docker-compose-multi-container-example-with-default-network"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

> By default Compose sets up a single network for your app. Each
> container for a service joins the default network and is both
> reachable by other containers on that network, and discoverable by
> them at a hostname identical to the container name.
> 
> Links allow you to define extra aliases by which a service is
> reachable from another service. They are not required to enable
> services to communicate – by default, any service can reach any other
> service at that service’s name.
> 
> > https://docs.docker.com/compose/networking/

## How to create a basic LAMP environment with default networking 
**docker-compose.yml**

    version: '2'
    services:
      php:
         image: phpmyadmin/phpmyadmin
         links:
           - mysql:db
         depends_on:
           - mysql
    
      mysql:
        image: k0st/alpine-mariadb
        volumes:
          - ./data/mysql:/var/lib/mysql
        environment:
          - MYSQL_DATABASE=mydb
          - MYSQL_USER=myuser
          - MYSQL_PASSWORD=mypass 
    
      nginx:
        image: nginx:stable-alpine
        ports:
          - "81:80"
        volumes:
          - ./nginx/log:/var/log/nginx
          - ./nginx/nginx.conf:/etc/nginx/nginx.conf:ro
        depends_on:
          - php

**nginx/nginx.conf**

    worker_processes  1;
    events {
      worker_connections  1024;
    }
    http {
      sendfile  off;
      server {
        listen 80;
     
        location / {
          proxy_pass  http://php;
          proxy_set_header Host $host;
          proxy_redirect     off;
        }
      }
    }

Note the nginx config is simplified but above should work for testing — basically all it’s doing is proxying the php app. Maps to port 81 to avoid conflicts on the host - adjust as needed.

Regarding linking, you can see that if you run: `docker-compose exec mysql ping -c2 nginx` to ping *from* the mysql container *to* the nginx container, you will succeed even though there are *no links specified between these containers*. Docker Compose will maintain those links in the default network for you.

