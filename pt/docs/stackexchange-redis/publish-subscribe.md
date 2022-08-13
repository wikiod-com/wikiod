---
title: "Publicar Assinar"
slug: "publicar-assinar"
draft: false
images: []
weight: 9750
type: docs
toc: true
---

## Fundamentos
Uma vez [conectado](https://www.wikiod.com/pt/stackexchange-redis/introducao-ao-stackexchangeredis), você pode publicar mensagens chamando o método `ISoscriber.Publish`:

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // publish a message to the 'chat' channel
    subscriber.Publish("chat", "This is a message")

Os consumidores podem se inscrever no canal usando o método `ISubscriber.Subscribe`. As mensagens enviadas pelo editor serão tratadas pelo manipulador passado para este método.

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to a messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, message) => {
        // do something with the message
        Console.WriteLine((string)message);
    });

## Dados complexos (JSON)
Você pode transmitir mensagens mais complexas serializando a carga útil antes de publicá-la:

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

O assinante então precisa desserializar a mensagem:

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, json) => {
        var message = JSON.Deserialize<ChatMessage>(json);

        // do something with the message
        Console.WriteLine($"{message.User} said {message.Text}");
    });

## Dados Complexos (Protobuf)
O StackExchange.Redis também suporta o envio de bytes pelo canal pub/sub, aqui usamos [protobuf-net](https://github.com/mgravell/protobuf-net) para serializar nossa mensagem para uma matriz de bytes antes de enviá-la:

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

Novamente, o assinante precisa desserializar a mensagem após o recebimento:

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

