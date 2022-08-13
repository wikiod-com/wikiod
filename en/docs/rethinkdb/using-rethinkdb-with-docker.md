---
title: "Using RethinkDB with Docker"
slug: "using-rethinkdb-with-docker"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Basic usage
By default, RethinkDB binds all services to `127.0.0.1`. So this following example will persist data to the `host_data_path` on the container's host machine and available to `127.0.0.1` on the standard ports.

| Service | Flag | Default Port |
| ------- | ---- | ------------ |
| Driver  | `--driver-port` | 28015|
| Cluster | `--cluster-port` | 29015|
| HTTP WebUI | `--http-port` | 8080 |

    docker run -d -v host_data_path:/data rethinkdb 

To open up the driver and cluster port to the external traffic you must specify the address of local interfaces or provide `all`.

    docker run -d -v host_data_path:/data rethinkdb --bind all 



## Binding WebUI to localhost or disabling
When deploying RethinkDB in production, you want to either turn off or lock down the WebUI. This will only respond to `localhost` to access the WebUI allowing you to SSH Tunnel to the host machine and access it for diagnostics and troubleshooting. 

    docker run -d \
       -v host_data_path:/data \
       rethinkdb \
       rethinkdb --bind-cluster all --bind-driver all --bind-http 127.0.0.1 -d /data

If you'd like to completely turn off the WebUI:

    docker run -d \
      -v host_data_path:/data \
      rethinkdb \
      rethinkdb --bind-cluster all --bind-driver all --no-http-admin -d /data



