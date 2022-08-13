---
title: "Keys and Values"
slug: "keys-and-values"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Setting Values
All values in Redis are ultimately stored as a ```RedisValue``` type:    

    //"myvalue" here is implicitly converted to a RedisValue type
    //The RedisValue type is rarely seen in practice.
    db.StringSet("key", "aValue");

## Setting and getting an int
    db.StringSet("key", 11021);
    int i = (int)db.StringGet("key");

Or using [StackExchange.Redis.Extensions](https://github.com/imperugo/StackExchange.Redis.Extensions):

    db.Add("key", 11021);
    int i = db.Get<int>("key");



