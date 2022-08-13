---
title: "Yayınla Abone Ol"
slug: "yaynla-abone-ol"
draft: false
images: []
weight: 9750
type: docs
toc: true
---

## Temel Bilgiler
[Bağlandığında](https://www.wikiod.com/tr/stackexchange-redis/stackexchangeredisi-kullanmaya-baslama) "ISubscriber.Publish" yöntemini çağırarak iletileri yayınlayabilirsiniz:

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // publish a message to the 'chat' channel
    subscriber.Publish("chat", "This is a message")

Tüketiciler, `ISubscriber.Subscribe' yöntemini kullanarak kanala abone olabilirler. Yayıncı tarafından gönderilen iletiler, bu yönteme iletilen işleyici tarafından işlenecektir.

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to a messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, message) => {
        // do something with the message
        Console.WriteLine((string)message);
    });

## Karmaşık Veriler (JSON)
Yükü yayınlamadan önce seri hale getirerek daha karmaşık mesajlar yayınlayabilirsiniz:

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

Abonenin daha sonra mesajı seri durumdan çıkarması gerekir:

    // grab an instance of an ISubscriber
    var subscriber = connection.GetSubscriber();

    // subscribe to messages over the 'chat' channel
    subscriber.Subscribe("chat", (channel, json) => {
        var message = JSON.Deserialize<ChatMessage>(json);

        // do something with the message
        Console.WriteLine($"{message.User} said {message.Text}");
    });

## Karmaşık Veriler (Protobuf)
StackExchange.Redis ayrıca pub/sub kanalı üzerinden bayt göndermeyi de destekler, burada mesajımızı göndermeden önce bir bayt dizisine seri hale getirmek için [protobuf-net](https://github.com/mgravell/protobuf-net) kullanıyoruz:

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

Yine abonenin, alındıktan sonra mesajı seri durumdan çıkarması gerekir:

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

