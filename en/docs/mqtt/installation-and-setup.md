---
title: "Installation and setup"
slug: "installation-and-setup"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

To implement MQTT

We need MQTT Broker, and MQTT client Library

## MQTT Libraries & MQTT Broker
> To use MQTT in the application we have variety of Libraries available
> for different programming languages.

***MQTT Library***

| LIBRARY| LANGUAGE|   DESCRIPTION    |
| ------ | ------ | -------
| Eclipse Paho| C, C++, Java, Javascript, Python, Go, C# |     Paho clients are among the most popular client library implementations.    |
| Fusesource MQTT Client| Java|   The Fusesource MQTT client is a Java MQTT client with 3 different API styles: Blocking, Future-based, and Callback-based.     |
| MQTT.js| Javascript|  MQTT.js is an MQTT client library for Node.js and web applications, available as a npm module.      |
| ruby-mqtt| Ruby|   ruby-mqtt is an MQTT client available as a Ruby gem. It does not support QoS > 0.      |

*****MQTT Broker***** 

   The broker is primarily responsible for receiving all messages (broker is like  messaging server), filtering them, decide who is interested in it and then sending the message to all subscribed clients .
MQTT Broker implementations:
    The table below shows some of the most popular open source and commercial broker implementations.

| Broker  ______| Description| 
| --------------   | ---------- |
| Apache ActiveMQ  | ActiveMQ is an open-source multi-protocol message broker with a core written around JMS. It supports MQTT and maps MQTT semantics over JMS.|        
| mosquitto|  |  
| Rabbit MQ  | RabbitMQ is a scalable, open-source message queue implementation, written in Erlang. It is an AMQP message broker but has an MQTT plugin available. Does not support all MQTT features (e.g. QoS 2). |  
| HiveMQ | HiveMQ is a scalable, high-performance MQTT broker suitable for mission critical deployments. It fully supports MQTT 3.1 and MQTT 3.1.1 and has features like websockets, clustering, and an open-source plugin system for Java developers. |  
| WebsphereMQ /IBM MQ | Websphere MQ is a commercial message- oriented middleware by IBM. Fully supports MQTT. |  


## steps to  install ActiveMQ broker
Go to ActiveMQ Website and download latest stable version of activeMQ

[click here to activeMQ downloads][1]


 - after  downloading,  unzip it

if you're using windows 32
 - **Go to apache-activemq-5.14.3\bin\win32**

if windows 64

 - **apache-activemq-5.14.3\bin\win64**
 - run the **activemq** batch file
 - thats it, activeMQ server is running on command prompt

***if you want to see the UI Consle for activeMQ . to get how messages are organized and sending***

got to http://localhost:8161/admin/
[![enter image description here][2]][2]


 - by default

 **username=admin**

  **password=admin**

 - then click on topic tab.

  [![enter image description here][3]][3]


  [1]: http://activemq.apache.org/activemq-5143-release.html
  [2]: https://i.stack.imgur.com/JRrGj.jpg
  [3]: https://i.stack.imgur.com/tCSEd.png

Topic tab gives info about how many topics are there and active consumers,produces, messages  delivered or not.

