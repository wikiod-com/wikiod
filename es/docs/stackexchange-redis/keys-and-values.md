---
title: "Claves y valores"
slug: "claves-y-valores"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Configuración de valores
Todos los valores en Redis se almacenan finalmente como un tipo ```RedisValue```:

    //"myvalue" here is implicitly converted to a RedisValue type
    //The RedisValue type is rarely seen in practice.
    db.StringSet("key", "aValue");

## Establecer y obtener un int
    db.StringSet("key", 11021);
    int i = (int)db.StringGet("key");

O usando [StackExchange.Redis.Extensions](https://github.com/imperugo/StackExchange.Redis.Extensions):

    db.Add("key", 11021);
    int i = db.Get<int>("key");



