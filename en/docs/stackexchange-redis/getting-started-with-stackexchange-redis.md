---
title: "Getting started with StackExchange.Redis"
slug: "getting-started-with-stackexchangeredis"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic Usage
    using StackExchange.Redis;

    // ...

    // connect to the server
    ConnectionMultiplexer connection = ConnectionMultiplexer.Connect("localhost");
    
    // select a database (by default, DB = 0)
    IDatabase db = connection.GetDatabase();

    // run a command, in this case a GET
    RedisValue myVal = db.StringGet("mykey");

## Reuse Multiplexer Across Application
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

## Configuration Options
  
**Connect to Redis server and allow admin (risky) commands**
  

 
    ConfigurationOptions options = new ConfigurationOptions()
                {
                    EndPoints = { { "localhost", 6379}},
                    AllowAdmin = true,
                    ConnectTimeout = 60*1000,
                };
    ConnectionMultiplexer multiplexer = ConnectionMultiplexer.Connect(options);

or

    ConnectionMultiplexer multiplexer = 
        ConnectionMultiplexer.Connect("localhost:6379,allowAdmin=True,connectTimeout=60000");

**Connect to Redis server via SSL**

     ConfigurationOptions options = new ConfigurationOptions()
                {
                    EndPoints = { { "localhost", 6380}},
                    Ssl = true,
                    Password = "12345"
                };
    ConnectionMultiplexer multiplexer = ConnectionMultiplexer.Connect(options);

or

    ConnectionMultiplexer multiplexer =
         ConnectionMultiplexer.Connect("localhost:6380,ssl=True,password=12345");

