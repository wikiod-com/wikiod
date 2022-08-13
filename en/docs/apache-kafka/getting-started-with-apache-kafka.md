---
title: "Getting started with apache-kafka"
slug: "getting-started-with-apache-kafka"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
**Step 1**. Install Java 7 or 8

**Step 2**. Download Apache Kafka at: http://kafka.apache.org/downloads.html

For example, we will try download [Apache Kafka 0.10.0.0][1]

**Step 3**. Extract the compressed file.

On Linux: 

    tar -xzf kafka_2.11-0.10.0.0.tgz

On Window: Right click --> Extract here

**Step 4**. Start Zookeeper

    cd kafka_2.11-0.10.0.0

Linux:

    bin/zookeeper-server-start.sh config/zookeeper.properties

Windows:

    bin/windows/zookeeper-server-start.bat config/zookeeper.properties

**Step 5**. Start Kafka server

Linux:

    bin/kafka-server-start.sh config/server.properties

Windows:

    bin/windows/kafka-server-start.bat config/server.properties



  [1]: https://www.apache.org/dyn/closer.cgi?path=/kafka/0.10.0.0/kafka_2.11-0.10.0.0.tgz

## Introduction
Apache Kafka™ is a distributed streaming platform.

Which means
-
1-It lets you publish and subscribe to streams of records. In this respect it is similar to a message queue or enterprise messaging system.

2-It lets you store streams of records in a fault-tolerant way.

3-It lets you process streams of records as they occur.

It gets used for two broad classes of application:
-
1-Building real-time streaming data pipelines that reliably get data between systems or applications

2-Building real-time streaming applications that transform or react to the streams of data


> Kafka console scripts are different for Unix-based and Windows platforms. In the examples, you might need to 
> add the extension according to your platform.
> Linux: scripts located in `bin/` with `.sh` extension.
> Windows: scripts located in `bin\windows\` and with `.bat` extension.

# Installation

__Step 1:__ [Download](https://www.apache.org/dyn/closer.cgi?path=/kafka/0.10.1.0/kafka_2.11-0.10.1.0.tgz) the code and untar it:

<!-- language: lang-bash -->
    tar -xzf kafka_2.11-0.10.1.0.tgz
    cd kafka_2.11-0.10.1.0

__Step 2:__ start the server.

> to be able to delete topics later, open `server.properties` and set `delete.topic.enable` to true.
 
Kafka relies heavily on zookeeper, so you need to start it first. If you don't have it installed, you can use the convenience script packaged with kafka to get a quick-and-dirty single-node ZooKeeper instance.

<!-- language: lang-bash -->
    zookeeper-server-start config/zookeeper.properties
    kafka-server-start config/server.properties

__Step 3:__ ensure everything is running

You should now have zookeeper listening to `localhost:2181` and a single kafka broker on `localhost:6667`. 

# Create a topic

We only have one broker, so we create a topic with no replication factor and just one partition:

<!-- language: lang-bash -->
    kafka-topics --zookeeper localhost:2181 \
        --create \
        --replication-factor 1 \
        --partitions 1 \
        --topic test-topic

Check your topic: 

<!-- language: lang-bash -->
    kafka-topics --zookeeper localhost:2181 --list
    test-topic

    kafka-topics --zookeeper localhost:2181 --describe --topic test-topic
    Topic:test-topic    PartitionCount:1    ReplicationFactor:1 Configs:
    Topic: test-topic   Partition: 0    Leader: 0   Replicas: 0 Isr: 0

# send and receive messages

Launch a consumer:

<!-- language: lang-bash -->
    kafka-console-consumer --bootstrap-server localhost:9092 --topic test-topic

On another terminal, launch a producer and send some messages. By default, the tool send each line as a separate message to the broker, without special encoding.  Write some lines and exit with CTRL+D or CTRL+C:

<!-- language: lang-bash -->
    kafka-console-producer --broker-list localhost:9092 --topic test-topic   
    a message
    another message
    ^D

The messages should appear in the consumer therminal.

# Stop kafka

<!-- language: lang-bash -->
    kafka-server-stop 

# start a multi-broker cluster

The above examples use only one broker. To setup a real cluster, we just need to start more than one kafka server. They will automatically coordinate themselves.

__Step 1:__ to avoid collision, we create a `server.properties` file for each broker and change the `id`, `port` and `logfile` configuration properties.

Copy:

    cp config/server.properties config/server-1.properties
    cp config/server.properties config/server-2.properties

Edit properties for each file, for example:

    vim config/server-1.properties
    broker.id=1
    listeners=PLAINTEXT://:9093
    log.dirs=/usr/local/var/lib/kafka-logs-1

    vim config/server-2.properties
    broker.id=2
    listeners=PLAINTEXT://:9094
    log.dirs=/usr/local/var/lib/kafka-logs-2

__Step 2:__ start the three brokers:

<!-- language: lang-bash -->
        kafka-server-start config/server.properties &
        kafka-server-start config/server-1.properties &
        kafka-server-start config/server-2.properties &

# Create a replicated topic

<!-- language: lang-bash -->
    kafka-topics --zookeeper localhost:2181 --create --replication-factor 3 --partitions 1 --topic replicated-topic

    kafka-topics --zookeeper localhost:2181 --describe --topic replicated-topic
    Topic:replicated-topic  PartitionCount:1    ReplicationFactor:3 Configs:
    Topic: replicated-topic Partition: 0    Leader: 1   Replicas: 1,2,0 Isr: 1,2,0

This time, there are more information:
 - "leader" is the node responsible for all reads and writes for the given partition. Each node will be the leader for a randomly selected portion of the partitions.
 - "replicas" is the list of nodes that replicate the log for this partition regardless of whether they are the leader or even if they are currently alive.
 - "isr" is the set of "in-sync" replicas. This is the subset of the replicas list that is currently alive and caught-up to the leader.

Note that the previously created topic is left unchanged.

# test fault tolerance

Publish some message to the new topic:

<!-- language: lang-bash -->
    kafka-console-producer --broker-list localhost:9092 --topic replicated-topic
    hello 1
    hello 2
    ^C

Kill the leader (1 in our example). On Linux:

<!-- language: lang-bash -->
    ps aux | grep server-1.properties
    kill -9 <PID>

On Windows:

<!-- language: lang-bash -->
    wmic process get processid,caption,commandline | find "java.exe" | find "server-1.properties" 
    taskkill /pid <PID> /f

See what happened: 

<!-- language: lang-bash -->
    kafka-topics --zookeeper localhost:2181  --describe --topic replicated-topic
    Topic:replicated-topic  PartitionCount:1    ReplicationFactor:3 Configs:
    Topic: replicated-topic Partition: 0    Leader: 2   Replicas: 1,2,0 Isr: 2,0

The leadership has switched to broker 2 and "1" in not in-sync anymore. But the messages are still there (use the consumer to check out by yourself).

# Clean-up

Delete the two topics using:

<!-- language: lang-bash -->
    kafka-topics --zookeeper localhost:2181 --delete --topic test-topic
    kafka-topics --zookeeper localhost:2181 --delete --topic replicated-topic


