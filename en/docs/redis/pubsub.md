---
title: "PubSub"
slug: "pubsub"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

Redis provides an implementation of the Publish/Subscribe (Pub/Sub) messaging pattern. Instead of sending messages to specific receivers, Publishers send messages to interested receivers via some indirection mechanism.  Receivers specify interest in particular messages.  In Redis this functionality is accessed using the PUBLISH and SUBSCRIBE commands on channels.

## Syntax
- SUBSCRIBE channel [channel ...]
- UNSUBSCRIBE [channel [channel ...]] 
- PUBLISH channel message
- PSUBSCRIBE pattern [pattern ...]
- PUNSUBSCRIBE [pattern [pattern ...]]

To handle the pub/sub in redis, need to have **one client for subscribe & different client for publish**. Both can't be handled by same client. Though all other commands can be still handled with same client.

## Publish & subscribe with redis


Redis has publish/subscribe for sending messages. This is handled by subscribing to a channel & publishing to channel. Yes, subscribers will subscribe to one or more channels. Publisher need not know who are all subscribers. Instead, publisher will publish to specific channel. All the subscribers who are subscribed for that channel will get the message. This decoupling of publishers and subscribers can allow for greater scalability and a more dynamic network topology.


**Example:**
User is subscribing to 2 channels say foo & boo

    SUBSCRIBE foo boo

In console of redis-client1:

    127.0.0.1:6379> SUBSCRIBE foo boo
    Reading messages... (press Ctrl-C to quit)
    1) "subscribe"
    2) "foo"
    3) (integer) 1
    1) "subscribe"
    2) "boo"
    3) (integer) 2

It will start to listen for message. On publish will get data for corresponding channel.

**For example:** When want to send message to all subscribers who are connected with boo, need to publish to that channel.

    PUBLISH boo "Hello Boo"

In console of redis-client1:

    1) "message"
    2) "boo" //channel name
    3) "Hello Boo" //Actual data

To unsubscribe from channel at any point, use 

    UNSUBSCRIBE // to unsubscribe from all channels
    UNSUBSCRIBE foo // to unsubscribe from specific channel

Can do subscribe based on pattern too. When the channel name is not sure/want to subscribe based on pattern use **PSUBSCRIBE**. 

Similarly to unsubscribe based on pattern use **PUNSUBSCRIBE**

