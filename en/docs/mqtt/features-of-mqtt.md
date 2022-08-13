---
title: "Features of MQTT"
slug: "features-of-mqtt"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

The protocol runs over TCP/IP, or over other network protocols that provide ordered, lossless, 
bi-directional connections.
 

## Simple public/subscribe model in MQTT
Its key features include:

 - Use of the publish/subscribe message pattern which provides
   one-to-many message distribution and decoupling of applications.
    
 - A messaging transport that is agnostic to the content of the payload.
   Three qualities of service for message delivery

 - A small transport overhead and protocol exchanges minimized to reduce
   network tra





[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/Ma7HZ.jpg


Generally there are two types of messaging service.

 - Queue(one to one connection)
  
 - Topic(one to one / one to many)

MQTT does'nt support queue which is reliable but MQTT supports topic, by default Topic is unreliable but we can use MQTT features and methods to make it reliable.




**Difference between Topic and Queue**



***Queue:***

 - Point-to-point model
 - Only one consumer gets the message
 - Messages have to be delivered in the order sent
 - A queue only guarantees that each message is processed only once.
 - The Queue knows who the consumer or the JMS client is. The
   destination is known.
 - The JMS client (the consumer) does not have to be  active or
   connected to the queue all the time to receive or read the message.
 - Every message successfully processed is acknowledged by the consumer.

***Topic:*** 

 - Publish/subscribe model

 - Multiple clients subscribe to the message

 - There is no guarantee messages have to be delivered in the order sent

 - There is no guarantees that each message is processed only once. 
   As this can be sensed from the model

 - The Topic, have multiple subscribers and there is a chance that the
   topic does not know all the subscribers. The destination is unknown

 - The subscriber / client needs to the active when the messages are
   produced by the producer, unless the subscription was a durable
   subscription.

 - No, Every message successfully processed is not acknowledged by the
   consumer/subscriber.

> but we can reduce the disadvantages of topic using MQTT. Topic can be reliable and control the duplicates in MQTT features

