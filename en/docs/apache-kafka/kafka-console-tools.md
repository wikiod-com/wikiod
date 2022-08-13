---
title: "kafka console tools"
slug: "kafka-console-tools"
draft: false
images: []
weight: 9092
type: docs
toc: true
---

Kafka offers command-line tools to manage topics, consumer groups, to consume and publish messages and so forth. 

__Important__:  Kafka console scripts are different for Unix-based and Windows platforms. In the examples, you might need to add the extension according to your platform.

*Linux*: scripts located in `bin/` with `.sh` extension.

*Windows*: scripts located in `bin\windows\` and with `.bat` extension.

## kafka-console-producer
This tool lets you produce messages from the command-line.

**Send simple string messages to a topic:**

<!-- language: lang-bash -->
    kafka-console-producer --broker-list localhost:9092 --topic test
    here is a message
    here is another message
    ^D

(each new line is a new message, type ctrl+D or ctrl+C to stop)

**Send messages with keys:**

<!-- language: lang-bash -->
    kafka-console-producer --broker-list localhost:9092 --topic test-topic \
            --property parse.key=true \
            --property key.separator=,
    key 1, message 1
    key 2, message 2
    null, message 3
    ^D

**Send messages from a file:**

<!-- language: lang-bash -->
    kafka-console-producer --broker-list localhost:9092 --topic test_topic < file.log

## kafka-simple-consumer-shell
This consumer is a low-level tool which allows you to consume messages from specific partitions, offsets and replicas. 

Useful parameters:
- `parition`: the specific partition to consume from (default to all)
- `offset`: the beginning offset. Use `-2` to consume messages from the beginning, `-1` to consume from the end. 
- `max-messages`: number of messages to print
- `replica`: the replica, default to the broker-leader (-1) 

Exemple: 

<!-- language: lang-bash -->
    kafka-simple-consumer-shell  \
        --broker-list localhost:9092 \
        --partition 1 \
        --offset 4 \
        --max-messages 3 \
        --topic test-topic

displays 3 messages from partition 1 beginning at offset 4 from topic test-topic.

## kafka-topics
This tool let you list, create, alter and describe topics.

**List topics:**

    kafka-topics  --zookeeper localhost:2181 --list

**Create a topic:** 


<!-- language: lang-bash -->
    kafka-topics  --create --zookeeper localhost:2181 --replication-factor 1 --partitions 1 --topic test

creates a topic with one partition and no replication. 

**Describe a topic:**

<!-- language: lang-bash -->
    kafka-topics  --zookeeper localhost:2181 --describe --topic test

**Alter a topic:**

<!-- language: lang-bash -->
    # change configuration
    kafka-topics  --zookeeper localhost:2181 --alter --topic test --config max.message.bytes=128000
    # add a partition
    kafka-topics  --zookeeper localhost:2181 --alter --topic test --partitions 2

(Beware: Kafka does not support reducing the number of partitions of a topic)
(see [this list of configuration properties](https://kafka.apache.org/documentation/#topic-config))


## kafka-console-consumer
This tool let's you consume messages from a topic.

> to use the old consumer implementation, replace `--bootstrap-server` with `--zookeeper`.


**Display simple messages:**

<!-- language: lang-bash -->
    kafka-console-consumer --bootstrap-server localhost:9092 --topic test 

**Consume old messages:**

In order to see older messages, you can use the `--from-beginning` option.

**Display key-value messages**:

<!-- language: lang-bash -->
    kafka-console-consumer  --bootstrap-server localhost:9092 --topic test-topic \
        --property print.key=true \
        --property key.separator=, 

## kafka-consumer-groups
This tool allows you to list, describe, or delete consumer groups.
Have a look at [this article](https://www.opsclarity.com/understanding-kafka-consumer-lag/) for more information about consumer groups.

> if you still use the old consumer implementation, replace `--bootstrap-server` with `--zookeeper`. 

**List consumer groups:**

<!-- language: lang-bash -->
    kafka-consumer-groups  --bootstrap-server localhost:9092 --list
    octopus

**Describe a consumer-group:**

<!-- language: lang-bash -->
    kafka-consumer-groups  --bootstrap-server localhost:9092 --describe --group octopus
    GROUP          TOPIC           PARTITION  CURRENT-OFFSET  LOG-END-OFFSET  LAG       OWNER
    octopus        test-topic      0          15              15              0         octopus-1/127.0.0.1
    octopus        test-topic      1          14              15              1         octopus-2_/127.0.0.1

_Remarks_: in the output above,
-  `current-offset` is the last committed offset of the consumer instance, 
- `log-end-offset` is the highest offset of the partition (hence, summing this column gives you the total number of messages for the topic)
- `lag` is the difference between the current consumer offset and the highest offset, hence how far behind the consumer is,
- `owner` is the `client.id` of the consumer (if not specified, a default one is displayed).


**Delete a consumer-group:**

> deletion is only available when the group metadata is stored in zookeeper (old consumer api). With the new consumer API, the broker handles everything including metadata deletion: the group is deleted automatically when the last committed offset for the group expires.

<!-- language: lang-bash -->
    kafka-consumer-groups --bootstrap-server localhost:9092 --delete --group octopus

