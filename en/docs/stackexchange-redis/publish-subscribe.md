---
title: "Publish Subscribe"
slug: "publish-subscribe"
draft: false
images: []
weight: 9750
type: docs
toc: true
---

## Basics
Once [connected](https://www.wikiod.com/stackexchange-redis/getting-started-with-stackexchangeredis) you can publish messages by calling the `ISubscriber.Publish` method:

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // publish a message to the 'chat' channel
    subscriber.Publish("chat", "This is a message")

Consumers can subscribe to the channel using the `ISubscriber.Subscribe` method. Messages sent by the publisher will be handled by the handler passed to this method.

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to a messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, message) => {
        // do something with the message
        Console.WriteLine((string)message);
    });

## Complex Data (JSON)
You can broadcast more complex messages by serializing the payload before you publish it:

    // definition of a message
    public class ChatMessage
    {
        public Guid Id { get; set; }
        public string User { get; set; }
        public string Text { get; set; }
    }

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();
    
    var message = new ChatMessage
    {
        Id = Guid.NewGuid(), 
        User = "User 1234", 
        Text = "Hello World!"
    };

    // serialize a ChatMessage
    // this uses JIL to serialize to JSON
    var json = JSON.Serialize(message);
    
    // publish the message to the 'chat' channel
    subscriber.Publish("chat", json)

The subscriber then needs to deserialize the message:

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, json) => {
        var message = JSON.Deserialize<ChatMessage>(json);

        // do something with the message
        Console.WriteLine($"{message.User} said {message.Text}");
    });

## Complex Data (Protobuf)
StackExchange.Redis also supports sending bytes over the pub/sub channel, here we use [protobuf-net](https://github.com/mgravell/protobuf-net) to serialize our message to a byte array before sending it:

    // definition of a message (marked up with Protobuf attributes)
    [ProtoContract]
    public class ChatMessage
    {
        [ProtoMember(1)]
        public Guid Id { get; set; }
        [ProtoMember(2)]
        public string User { get; set; }
        [ProtoMember(3)]
        public string Text { get; set; }
    }
    
    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    var message = new ChatMessage
    {
        Id = Guid.NewGuid(), 
        User = "User 1234", 
        Text = "Hello World!"
    };
    
    using (var memoryStream = new MemoryStream())
    {
        // serialize a ChatMessage using protobuf-net
        Serializer.Serialize(memoryStream, message);

        // publish the message to the 'chat' channel
        subscriber.Publish("chat", memoryStream.ToArray());
    }

Again the subscriber needs to deserialize the message upon receipt:

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();
    
    // subscribe to messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, bytes) => {
        using (var memoryStream = new MemoryStream(bytes))
        {
            var message = Serializer.Deserialize<ChatMessage>(memoryStream);

            // do something with the message
            Console.WriteLine($"{message.User} said {message.Text}");
        }
    });

