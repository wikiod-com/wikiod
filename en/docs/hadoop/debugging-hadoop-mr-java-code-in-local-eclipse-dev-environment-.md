---
title: "Debugging Hadoop MR Java code in local eclipse dev environment."
slug: "debugging-hadoop-mr-java-code-in-local-eclipse-dev-environment"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

The basic thing to remember here is that debugging a Hadoop MR job is going to be similar to any remotely debugged application in Eclipse.

A debugger or debugging tool is a computer program that is used to test and debug other programs (the “target” program). It is greatly useful specially for a Hadoop environment wherein there is little room for error and one small error can cause a huge loss.

That is all you need to do.

## Steps for configuration
As you would know, Hadoop can be run in the local environment in 3 different modes :

1. Local Mode
2. Pseudo Distributed Mode
3. Fully Distributed Mode (Cluster)

Typically you will be running your local hadoop setup in Pseudo Distributed Mode to leverage HDFS and Map Reduce(MR). However you cannot debug MR programs in this mode as each Map/Reduce task will be running in a separate JVM process so you need to switch back to Local mode where you can run your MR programs in a single JVM process.

Here are the quick and simple steps to debug this in your local environment:

1. Run hadoop in local mode for debugging so mapper and reducer tasks run in a single JVM instead of separate JVMs. Below steps help you do it.

2. Configure HADOOP_OPTS to enable debugging so when you run your Hadoop job, it will be waiting for the debugger to connect. Below is the command to debug the same at port 8080.

(export HADOOP_OPTS=”-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=8008“)

3. Configure fs.default.name value in core-site.xml to file:/// from hdfs://. You won’t be using hdfs in local mode.

4. Configure mapred.job.tracker value in mapred-site.xml to local. This will instruct Hadoop to run MR tasks in a single JVM.

5. Create debug configuration for Eclipse and set the port to 8008 – typical stuff. For that go to the debugger configurations and create a new Remote Java Application type of configuration and set the port as 8080 in the settings.

7. Run your hadoop job (it will be waiting for the debugger to connect) and then launch Eclipse in debug mode with the above configuration. Do make sure to put a break-point first.


