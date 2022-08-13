---
title: "Scan"
slug: "scan"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Syntax
- `public IEnumerable<RedisKey> Keys(int database = 0, RedisValue pattern = default(RedisValue), int pageSize = CursorUtils.DefaultPageSize, long cursor = CursorUtils.Origin, int pageOffset = 0, CommandFlags flags = CommandFlags.None)`  

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| database  | Redis database index to connect to|  
| pattern   | *Unsure* |  
| pageSize  | Number of items to return per page |  
| cursor    | *Unsure* |
| pageOffset| Number of pages to offset the results by |
| flags     | *Unsure* |


The `Keys()` call will select either the `KEYS` or `SCAN` command based on the version of the Redis server. Where possible it will prefer the usage of `SCAN` which returns an `IEnumerable<RedisKey>` and does not block. `KEYS` on the other hand will block when scanning the key space.

## Basic scanning of all keys on server
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    // Write out each key in the server
    foreach(var key in server.Keys()) {
        Console.WriteLine(key);
    }



## Iterating using a cursor
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    var seq = server.Keys();
    IScanningCursor scanningCursor = (IScanningCursor)seq;
       
    // Use the cursor in some way...

