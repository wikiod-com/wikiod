---
title: "Publier S'abonner"
slug: "publier-sabonner"
draft: false
images: []
weight: 9750
type: docs
toc: true
---

## Bases
Une fois [connecté](https://www.wikiod.com/fr/stackexchange-redis/premiers-pas-avec-stackexchangeredis), vous pouvez publier des messages en appelant la méthode `ISubscriber.Publish` :

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // publish a message to the 'chat' channel
    subscriber.Publish("chat", "This is a message")

Les consommateurs peuvent s'abonner à la chaîne en utilisant la méthode "ISubscriber.Subscribe". Les messages envoyés par l'éditeur seront gérés par le gestionnaire passé à cette méthode.

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to a messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, message) => {
        // do something with the message
        Console.WriteLine((string)message);
    });

## Données complexes (JSON)
Vous pouvez diffuser des messages plus complexes en sérialisant la charge utile avant de la publier :

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

L'abonné doit alors désérialiser le message :

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, json) => {
        var message = JSON.Deserialize<ChatMessage>(json);

        // do something with the message
        Console.WriteLine($"{message.User} said {message.Text}");
    });

## Données complexes (Protobuf)
StackExchange.Redis prend également en charge l'envoi d'octets sur le canal pub/sub. Ici, nous utilisons [protobuf-net](https://github.com/mgravell/protobuf-net) pour sérialiser notre message dans un tableau d'octets avant de l'envoyer :

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

Encore une fois, l'abonné doit désérialiser le message à la réception :

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

