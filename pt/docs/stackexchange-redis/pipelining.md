---
title: "Pipeline"
slug: "pipeline"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Pipeline e Multiplexação
    var multiplexer = ConnectionMultiplexer.Connect("localhost");
    IDatabase db = multiplexer.GetDatabase();
    
    // intialize key with empty string
    await db.StringSetAsync("key", "");
    
    // create transaction that utilize multiplexing and pipelining
    ITransaction transacton = db.CreateTransaction();
    Task<long> appendA = transacton.StringAppendAsync("key", "a");
    Task<long> appendB = transacton.StringAppendAsync("key", "b");
                
    if (await transacton.ExecuteAsync()) // sends "MULTI APPEND KEY a APPEND KEY b EXEC
    // in single request to redis server
    {
        // order here doesn't matter, result is always - "abc". 
        // 'a' and 'b' append always together in isolation of other Redis commands
        // 'c' appends to "ab" because transaction is already executed successfully
        await appendA;
        await db.StringAppendAsync("key", "c");
        await appendB;
    }
                
    string value = db.StringGet("key"); // value is "abc"

