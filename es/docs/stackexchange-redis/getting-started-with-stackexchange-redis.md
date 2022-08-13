---
title: "Primeros pasos con StackExchange.Redis"
slug: "primeros-pasos-con-stackexchangeredis"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Uso básico
    using StackExchange.Redis;

    // ...

    // connect to the server
    ConnectionMultiplexer connection = ConnectionMultiplexer.Connect("localhost");
    
    // select a database (by default, DB = 0)
    IDatabase db = connection.GetDatabase();

    // run a command, in this case a GET
    RedisValue myVal = db.StringGet("mykey");

## Reutilice el multiplexor en todas las aplicaciones
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

## Opciones de configuración
  
**Conéctese al servidor Redis y permita comandos de administración (riesgosos)**
  

 
    ConfigurationOptions options = new ConfigurationOptions()
                {
                    EndPoints = { { "localhost", 6379}},
                    AllowAdmin = true,
                    ConnectTimeout = 60*1000,
                };
    ConnectionMultiplexer multiplexer = ConnectionMultiplexer.Connect(options);

o

    ConnectionMultiplexer multiplexer = 
        ConnectionMultiplexer.Connect("localhost:6379,allowAdmin=True,connectTimeout=60000");

**Conéctese al servidor Redis a través de SSL**

     ConfigurationOptions options = new ConfigurationOptions()
                {
                    EndPoints = { { "localhost", 6380}},
                    Ssl = true,
                    Password = "12345"
                };
    ConnectionMultiplexer multiplexer = ConnectionMultiplexer.Connect(options);

o

    ConnectionMultiplexer multiplexer =
         ConnectionMultiplexer.Connect("localhost:6380,ssl=True,password=12345");

