---
title: "Getting started with rabbitmq"
slug: "getting-started-with-rabbitmq"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing RabbitMQ on Ubuntu Server
*A quick note before actually installing RabbitMQ: Ubuntu 14.04's Erlang packages have issues if you are using SSL with RabbitMQ, so you'll need to install a newer version than what the Ubuntu package maintainers provide, so use the binaries at https://www.erlang-solutions.com/resources/download.html, for Erlang 17.0 or higher.*

Add RabbitMQ to the package repositories list:

    echo 'deb http://www.rabbitmq.com/debian/ testing main' |
        sudo tee /etc/apt/sources.list.d/rabbitmq.list

And then add the signing key:

    wget -O- https://www.rabbitmq.com/rabbitmq-release-signing-key.asc |
        sudo apt-key add -

Then update and install:
    
    sudo apt-get update && sudo apt-get install rabbitmq-server



## RabbitMQ 'Hello World'
This code create a producer which send two messages to a queue, and a consumer which receives all the messages from that queue.

Code for producer.py (using the pika 0.10.0 Python client):

    import pika
    
    connection = pika.BlockingConnection(pika.ConnectionParameters(
            host='localhost'))
    channel = connection.channel()
    
    channel.queue_declare(queue='queueName')
    
    channel.basic_publish(exchange='',
                          routing_key='queueName',
                          body='Hello')
    channel.basic_publish(exchange='',
                          routing_key='queueName',
                          body='World!')
    print("Message sent")
    connection.close()

Code for consumer.py:

    import pika
    
    connection = pika.BlockingConnection(pika.ConnectionParameters(
            host='localhost'))
    channel = connection.channel()
    
    channel.queue_declare(queue='queueName')
    
    def callback(ch, method, properties, body):
        print("Received message: %r" % body)
    
    channel.basic_consume(callback,
                          queue='queueName',
                          no_ack=True)
    
    print('Waiting for messages...')
    channel.start_consuming()

The output is:

    $ python receive.py
    Waiting for messages...
    Received message: 'Hello'
    Received message: 'World!'

Other examples are available in the RabbitMQ tutorial [page][1] for other languages.


  [1]: https://www.rabbitmq.com/tutorials/tutorial-one-java.html

