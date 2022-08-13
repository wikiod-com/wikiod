---
title: "Consumer Groups and Offset Management"
slug: "consumer-groups-and-offset-management"
draft: false
images: []
weight: 9432
type: docs
toc: true
---

## Parameters
Parameter               | Description
----------------------- | -----------
group.id                | The name of the Consumer Group.
enable.auto.commit      | Automatically commit offsets; *default: true*.
auto.commit.interval.ms | The minimum delay in milliseconds between to commits (requires `enable.auto.commit=true`); *default: 5000*.
auto.offset.reset       | What to do when there is no valid committed offset found; *default: latest*.(+)
 |
**(+) Possible Values** | **Description**
earliest            | Automatically reset the offset to the earliest offset.
latest              | Automatically reset the offset to the latest offset.
none                | Throw exception to the consumer if no previous offset is found for the consumer's group.
anything else       | Throw exception to the consumer.

## Consumer Offset Management and Fault-Tolerance
[KafkaConsumers][1] request messages from a Kafka broker via a call to `poll()` and their progress is tracked via *offsets*. Each message within each partition of each topic, has a so-called offset assigned&mdash;its logical sequence number within the partition. A `KafkaConsumer` tracks its current offset for each partition that is assigned to it. Pay attention, that the Kafka brokers are not aware of the current offsets of the consumers. Thus, on `poll()` the consumer needs to send its current offsets to the broker, such that the broker can return the corresponding messages, i.e,. messages with a larger consecutive offset. For example, let us assume we have  a single partition topic and a single consumer with current offset 5. On `poll()` the consumer sends if offset to the broker and the broker return messages for offsets 6,7,8,...

Because consumers track their offsets themselves, this information could get lost if a consumer fails. Thus, offsets must be stored reliably, such that on restart, a consumer can pick up its old offset and resumer where it left of. In Kafka, there is built-in support for this via *offset commits*. The new `KafkaConsumer` can commit its current offset to Kafka and Kafka stores those offsets in a special topic called `__consumer_offsets`. Storing the offsets within a Kafka topic is not just fault-tolerant, but allows to reassign partitions to other consumers during a rebalance, too. Because all consumers of a Consumer Group can access all committed offsets of all partitions, on rebalance, a consumer that gets a new partition assigned just reads the committed offset of this partition from the `__consumer_offsets` topic and resumes where the old consumer left of.


  [1]: http://docs.confluent.io/3.0.0/clients/consumer.html

## How to Commit Offsets
[KafkaConsumers][1] can commit offsets automatically in the background (configuration parameter `enable.auto.commit = true`) what is the default setting. Those auto commits are done within `poll()` ([which is typically called in a loop][2]). How frequently offsets should be committed, can be configured via `auto.commit.interval.ms`. Because, auto commits are embedded in `poll()` and `poll()` is called by the user code, this parameter defines a lower bound for the inter-commit-interval.

As an alternative to auto commit, offsets can also be managed manually. For this, auto commit should be disabled (`enable.auto.commit = false`). For manual committing `KafkaConsumers` offers two methods, namely [commitSync()][3] and [commitAsync()][4]. As the name indicates, `commitSync()` is a blocking call, that does return after offsets got committed successfully, while `commitAsync()` returns immediately. If you want to know if a commit was successful or not, you can provide a call back handler (`OffsetCommitCallback`) a method parameter. Pay attention, that in both commit calls, the consumer commits the offsets of the latest `poll()` call. For example. let us assume a single partition topic with a single consumer and the last call to `poll()` return messages with offsets 4,5,6. On commit, offset 6 will be committed because this is the latest offset tracked by the consumer client. At the same time, both `commitSync()` and `commitAsync()` allow for more control what offset you want to commit: if you use the corresponding overloads that allow you to specify a `Map<TopicPartition, OffsetAndMetadata>` the consumer will commit only the specified offsets (ie, the map can contain any subset of assigned partitions, and the specified offset can have any value).

Semantics of committed offsets
------------------------------
A committed offset indicates, that all messages up to this offset got already processed. Thus, as offsets are consecutive numbers, committing offset `X` implicitly commits all offsets smaller than `X`. Therefore, it is not necessary to commit each offset individually, and committing multiple offsets at once, happens but just committing the largest offset.

Pay attention, that by design it is also possible to commit a smaller offset than the last committed offset. This can be done, if messages should be read a second time.

Processing guarantees
---------------------
Using auto commit provides at-least-once processing semantics. The underlying assumption is, that `poll()` is only called after all previously delivered messages got processed successfully. This ensures, that no message get lost because a commit happens *after* processing. If a consumer fails before a commit, all messages after the last commit are received from Kafka and processed again. However, this retry might result in duplicates, as some message from the last `poll()` call might have been processed but the failure happened right before the auto commit call.

If at-most-once processing semantics are required, auto commit must be disabled and a manual `commitSync()` directly after `poll()` should be done. Afterward, messages get processed. This ensure, that messages are committed *before* there are processed and thus never read a second time. Of course, some message might get lost in case of failure.

  [1]: http://docs.confluent.io/current/clients/consumer.html
  [2]: http://docs.confluent.io/current/clients/consumer.html#basic-poll-loop
  [3]: http://docs.confluent.io/current/clients/consumer.html#synchronous-commits
  [4]: http://docs.confluent.io/current/clients/consumer.html#asynchronous-commits

## How can I Read Topic From its Beginning
There are multiple strategies to read a topic from its beginning. To explain those, we first need to understand what happens at consumer startup. On startup of a consumer, the following happens:
 1. join the configured consumer group, which triggers a rebalance and assigns partitions to the consumer 
 2. look for committed offsets (for all partitions that got assigned to the consumer)
 3. for all partitions with valid offset, resume from this offset
 4. for all partitions with not valid offset, set start offset according to `auto.offset.reset` configuration parameter

Start a new Consumer Group
--------------------------
If you want to process a topic from its beginning, you can simple start a new consumer group (i.e., choose an unused `group.id`) and set `auto.offset.reset = earliest`. Because there are no committed offsets for a new group, auto offset reset will trigger and the topic will be consumed from its beginning. Pay attention, that on consumer restart, if you use the same `group.id` again, it will not read the topic from beginning again, but resume where it left of. Thus, for this strategy, you will need to assign a new `group.id` each time you want to read a topic from its beginning.

Reuse the same Group ID
-----------------------
To avoid setting a new `group.id` each time you want to read a topic from its beginning, you can disable auto commit (via `enable.auto.commit  = false`) before starting the consumer for the very first time (using an unused `group.id` and setting `auto.offset.reset = earliest`). Additionally, you should not commit any offsets manually. Because offsets are never committed using this strategy, on restart, the consumer will read the topic from its beginning again.

However, this strategy has two disadvantages:
 1. it is not fault-tolerant
 2. group rebalance does not work as intended

(1) Because offsets are never committed, a failing and a stopped consumer are handled the same way on restart. For both cases, the topic will be consumed from its beginning.
(2) Because offset are never committed, on rebalance newly assigned partitions will be consumer from the very beginning.

Therefore, this strategy only works for consumer groups with a single consumer and should only be used for development purpose.


Reuse the same Group ID and Commit
----------------------------------
If you want to be fault-tolerant and/or use multiple consumers in your Consumer Group, committing offsets is mandatory. Thus, if you want to read a topic from its beginning, you need to manipulate committed offsets at consumer startup. For this, `KafkaConsumer` provides three methods `seek()`, `seekToBeginning()`, and `seekToEnd()`. While `seek()` can be used to set an arbitrary offset, the second and third method can be use to seek to the beginning or end of a partition, respectively. Thus, on failure and on consumer restart seeking would be omitted and the consumer can resume where it left of. For consumer-stop-and-restart-from-beginning, `seekToBeginning()` would be called explicitly before you enter your `poll()` loop. Note, that `seekXXX()` can only be used after a consumer joined a group -- thus, it's required to do a "dummy-poll" before using `seekXXX()`. The overall code would be something like this:

<!-- language: java -->

    if (consumer-stop-and-restart-from-beginning) {
        consumer.poll(0); // dummy poll() to join consumer group
        consumer.seekToBeginning(...);
    }

    // now you can start your poll() loop
    while (isRunning) {
        for (ConsumerRecord record : consumer.poll(0)) {
            // process a record
        }
    }

## What is a Consumer Group
As of Kafka 0.9, the new high level [KafkaConsumer][1] client is availalbe. It exploits a [new built-in Kafka protocol][2] that allows to combine multiple consumers in a so-called [*Consumer Group*][3]. A Consumer Group can be describes as a single logical consumer that subscribes to a set of topics. The partions over all topics are assigend to the physical consumers within the group, such that each patition is assigned to exaclty one consumer (a single consumer can get multiple partitons assigned). The indiviual consumers belonging to the same group can run on different hosts in a distributed manner.

Consumer Groups are identified via their `group.id`. To make a specific client instance member of a Consumer Group, it is sufficient to assign the groups `group.id` to this client, via the client's configuration:

<!-- language: java -->

    Properties props = new Properties();
    props.put("group.id", "groupName");
    // ...some more properties required
    new KafkaConsumer<K, V>(config);

Thus, all consumers that connect to the same Kafka cluster and use the same `group.id` form a Consumer Group. Consumers can leave a group at any time and new consumers can join a group at any time. For both cases, a so-called *rebalance* is triggered and partitions get reassigned with the Consumer Group to ensure that each partition is processed by exaclty one consumer within the group.

Pay attention, that even a single `KafkaConsumer` forms a Consumer Group with itself as single member.

  [1]: http://docs.confluent.io/3.0.0/clients/consumer.html
  [2]: https://cwiki.apache.org/confluence/display/KAFKA/Kafka+0.9+Consumer+Rewrite+Design
  [3]: http://docs.confluent.io/3.0.0/clients/consumer.html#concepts

