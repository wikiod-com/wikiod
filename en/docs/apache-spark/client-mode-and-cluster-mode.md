---
title: "Client mode  and Cluster Mode"
slug: "client-mode--and-cluster-mode"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Spark Client and Cluster mode explained
Let's try to look at the differences between client and cluster mode of Spark.

**Client**:
When running Spark in the client mode, the SparkContext and Driver program run external to the cluster; for example, from your laptop. Local mode is only for the case when you do not want to use a cluster and instead want to run everything on a single machine. So Driver Application and Spark Application are both on the same machine as the user.
Driver runs on a dedicated server (Master node) inside a dedicated process. This means it has all available resources at it's disposal to execute work.
Because the Master node has dedicated resources of it's own, you don't need to "spend" worker resources for the Driver program.
If the driver process dies, you need an external monitoring system to reset it's execution.

**Cluster:**
Driver runs on one of the cluster's Worker nodes.It runs as a dedicated, standalone process inside the Worker.
When working in Cluster mode, all JARs related to the execution of your application need to be publicly available to all the workers. This means you can either manually place them in a shared place or in a folder for each of the workers.
Each application gets its own executor processes, which stay up for the duration of the whole application and run tasks in multiple threads. This has the benefit of isolating applications from each other, on both the scheduling side (each driver schedules its own tasks) and executor side (tasks from different applications run in different JVMs

**Cluster Manager Types**

Apache Mesos – a general cluster manager that can also run Hadoop MapReduce and service applications.
Hadoop YARN – the resource manager in Hadoop.  
Kubernetes- container-centric infrastructure.it is experimental yet.

