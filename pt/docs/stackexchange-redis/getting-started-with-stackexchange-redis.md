---
title: "Introdução ao StackExchange.Redis"
slug: "introducao-ao-stackexchangeredis"
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

## Reutilize o multiplexador em todo o aplicativo
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

## Opções de configuração
  
**Conecte-se ao servidor Redis e permita comandos de administrador (arriscados)**
  

 
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

**Conecte-se ao servidor Redis via SSL**

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

