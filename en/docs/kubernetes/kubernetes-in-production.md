---
title: "Kubernetes in production"
slug: "kubernetes-in-production"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Introduce how to use kubernetes in production environment

## Deploy zookeeper cluster in production using kubernetes and ceph
#### Dockerize zookeeper-3.4.6
--------------------------------

Create a Dockerfile:

```dockerfile
#######################################################
# Image: img.reg.3g:15000/zookeeper:3.4.6
#######################################################

FROM img.reg.3g:15000/jdk:1.7.0_67 

MAINTAINER lth9739@gmail.com

USER root

ENV ZOOKEEPER_VERSION 3.4.6

ADD Dockerfile /

ADD zookeeper/ /opt/

COPY zoo.cfg /opt/zookeeper/conf/zoo.cfg

RUN mkdir -p /opt/zookeeper/{data,log}

WORKDIR /opt/zookeeper

VOLUME ["/opt/zookeeper/conf", "/opt/zookeeper/data", "/opt/zookeeper/log"]

COPY config-and-run.sh /opt/zookeeper/bin/

EXPOSE 2181 2888 3888

CMD ["/opt/zookeeper/bin/config-and-run.sh"]
```

[See more details](https://github.com/fabric8io/fabric8-zookeeper-docker)


#### Deploy zookeeper replica controller into kubernetes cluster
---------------------

You can use this command to deploy the replica-controller of zookeeper:

```bash
kubectl create -f zookeeper-rc-1.json
```

```json
{
  "apiVersion": "v1",
  "kind": "ReplicationController",
  "metadata": {
    "labels": {
      "component": "zookeeper"
    },
    "name": "zookeeper-1"
  },
  "spec": {
    "replicas": 1,
    "selector": {
      "server-id": "1",
      "role": "zookeeper-1"
    },
    "template": {
      "metadata": {
        "labels": {
          "server-id": "1",
          "role": "zookeeper-1"
        },
        "name": "zookeeper-1"
      },
      "spec": {
        "containers": [
          {
            "env": [
              {
                "value": "1",
                "name": "SERVER_ID"
              },
              {
                "value": "5",
                "name": "MAX_SERVERS"
              }
            ],
            "image": "img.reg.3g:15000/fabric8/zookeeper:latest",
            "name": "zookeeper-1",
            "ports": [
              {
                "containerPort": 2181,
                "name": "client",
                "protocol": "TCP"
              },
              {
                "containerPort": 2888,
                "name": "followers",
                "protocol": "TCP"
              },
              {
                "containerPort": 3888,
                "name": "election",
                "protocol": "TCP"
              }
            ],
            "volumeMounts": [
              {
                "mountPath": "/opt/zookeeper/data",
                "name": "zookeeper-1"
              }
            ]
          }
        ],
        "restartPolicy": "Always",
        "volumes": [
          {
            "name": "zookeeper-1",
            "rbd": {
              "monitors": [
                "10.151.32.27:6789",
                "10.151.32.29:6789",
                "10.151.32.32:6789"
              ],
              "pool": "rbd",
              "image": "log-zookeeper-1",
              "user": "admin",
              "secretRef": {
                "name": "ceph-secret-default"
              },
              "fsType": "ext4",
              "readOnly": false
            }
          }
        ]
      }
    }
  }
}
```

#### Deploy zookeeper service into kubernetes cluster
---------------------


You can use this command to deploy the service of zookeeper:

```bash
kubectl create -f zookeeper-svc-1.json
```

```json
{
  "kind": "Service",
  "apiVersion": "v1",
  "metadata": {
    "name": "zookeeper-1",
    "labels": {
      "name": "zookeeper-1"
    }
  },
  "spec": {
    "ports": [
      {
        "name": "client",
        "port": 2181,
        "targetPort": 2181
      },
      {
        "name": "followers",
        "port": 2888,
        "targetPort": 2888
      },
      {
        "name": "election",
        "port": 3888,
        "targetPort": 3888
      }
    ],
    "selector": {
      "server-id": "1"
    }
  }
}
```

#### Zookeeper cluster
------------------

If you want get a zookeeper cluster with 5 nodes, you can write zookeeper-rc-2/3/4/5.json and zookeeper-svc-2/3/4/5.json files as described above and use kubectl command to deploy them into kubernetes cluster.

