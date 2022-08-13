---
title: "Premiers pas avec StackExchange.Redis"
slug: "premiers-pas-avec-stackexchangeredis"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Utilisation de base
    using StackExchange.Redis;

    // ...

    // connect to the server
    ConnectionMultiplexer connection = ConnectionMultiplexer.Connect("localhost");
    
    // select a database (by default, DB = 0)
    IDatabase db = connection.GetDatabase();

    // run a command, in this case a GET
    RedisValue myVal = db.StringGet("mykey");

## Réutiliser le multiplexeur dans l'application
    class Program
    {
        private static Lazy<ConnectionMultiplexer> _multiplexer =
            new Lazy<ConnectionMultiplexer>(
            () => ConnectionMultiplexer.Connect("localhost"), 
            LazyThreadSafetyMode.ExecutionAndPublication);

        static void Main(string[] args)
        {
            IDatabase db1 = _multiplexer.Value.GetDatabase(1);
            IDatabase db2 = _multiplexer.Value.GetDatabase(2);
        }
    }

##Options de configuration
  
**Connectez-vous au serveur Redis et autorisez les commandes d'administration (risquées)**
  

 
    ConfigurationOptions options = new ConfigurationOptions()
                {
                    EndPoints = { { "localhost", 6379}},
                    AllowAdmin = true,
                    ConnectTimeout = 60*1000,
                };
    ConnectionMultiplexer multiplexer = ConnectionMultiplexer.Connect(options);

ou

    ConnectionMultiplexer multiplexer = 
        ConnectionMultiplexer.Connect("localhost:6379,allowAdmin=True,connectTimeout=60000");

**Se connecter au serveur Redis via SSL**

     ConfigurationOptions options = new ConfigurationOptions()
                {
                    EndPoints = { { "localhost", 6380}},
                    Ssl = true,
                    Password = "12345"
                };
    ConnectionMultiplexer multiplexer = ConnectionMultiplexer.Connect(options);

ou

    ConnectionMultiplexer multiplexer =
         ConnectionMultiplexer.Connect("localhost:6380,ssl=True,password=12345");

