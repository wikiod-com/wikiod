---
title: "YARN - MapReduce 2"
slug: "yarn---mapreduce-2"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

YARN stands for Yet Another Resource Negotiator.

Brief History:

YARN was developed by Yahoo in 2010 as a substitute of Map Reduce 1. It was able to overcome disadvantages which were present in Map Reduce 1.

Introduction
 
In Map Reduce 1 Job Tracker was used to both the Job scheduling and Task Monitoring. This lead to the bottleneck in scalability of the cluster. YARN separate this two roles into 2 separate daemons which are Resource Manager and Application Master.
  

 

## YARN Anatomy
**Components**

***Resource Manager***: It is the ultimate authority that allocates resource among all application in the system. It is also responsible for allocating container in which application master will start and initializing it after container allocation for application master. 

***Node Manager***: It is responsible for monitoring container and their resource usage on any given node on which it is running.

[![enter image description here][1]][1]

***Application Master***: It is responsible for negotiating with Resource Manager for allocation of required resources on data node for executing any job and also coordinating with node manager to execute the job and monitor the containers and their resource consumption.

***Container*** : It represent resources like CPU, Memory, Disk etc. required for processing of the any job. It is the result of successful resource allocation from the resource manager. Container grant the right to application master to use a specific amount of resource on a specific machine (node) in a cluster. There can be multiple containers on the same machine.

**Process of Job Execution**
[![enter image description here][2]][2] 

1. User/Client submit the application and specification for application master are generated. 

2. Resource manager takes responsibility to allocate specified container in which Application Master will start, then it initiates Application Master. Each job gets it's own separate Application Master. 

3. Application Master boot up and register with the Resource manager. This help client to get to know certain required details of jobs execution directly via Application Master. 

4. Application Master then makes a request to the resource manager for allocation of the containers with required resources in order to execute the job on nodes. This resource request is made via Resource Request Protocol (RRP). 

5. On successful container allocation Application Master introduce the containers to Node Manager It also provides details related to the allocation for launching containers on the node. Before starting container Node Manager verifies container allocation with Resource Manager to avoid fake allocation. 

6. When Application code executes within the container, the container provides appropriate information to its Application Master via Application Specific Protocol (ASP). 

7. During execution client directly connect with Application Master to get Status, Progress etc. via Application Specific Protocol. 

8. Once the process of the application submitted by the user is complete, Application Master deregister itself with Resource Manager and shut down, allowing the allocated container for it to release and reuse Along with it, all the containers acquired for the job are released again. 


  [1]: https://i.stack.imgur.com/N7VCT.png
  [2]: https://i.stack.imgur.com/lxddU.png

