---
title: "Publicar Suscribirse"
slug: "publicar-suscribirse"
draft: false
images: []
weight: 9750
type: docs
toc: true
---

## Conceptos básicos
Una vez [conectado](https://www.wikiod.com/es/stackexchange-redis/primeros-pasos-con-stackexchangeredis) puede publicar mensajes llamando al método `ISubscriber.Publish`:

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // publish a message to the 'chat' channel
    subscriber.Publish("chat", "This is a message")

Los consumidores pueden suscribirse al canal usando el método `ISubscriber.Subscribe`. Los mensajes enviados por el editor serán manejados por el controlador pasado a este método.

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to a messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, message) => {
        // do something with the message
        Console.WriteLine((string)message);
    });

## Datos complejos (JSON)
Puede transmitir mensajes más complejos serializando la carga útil antes de publicarla:

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

El suscriptor luego necesita deserializar el mensaje:

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, json) => {
        var message = JSON.Deserialize<ChatMessage>(json);

        // do something with the message
        Console.WriteLine($"{message.User} said {message.Text}");
    });

## Datos complejos (Protobuf)
StackExchange.Redis también admite el envío de bytes a través del canal pub/sub, aquí usamos [protobuf-net](https://github.com/mgravell/protobuf-net) para serializar nuestro mensaje en una matriz de bytes antes de enviarlo:

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

Nuevamente, el suscriptor debe deserializar el mensaje al recibirlo:

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

