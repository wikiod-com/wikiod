---
title: "Chaves e Valores"
slug: "chaves-e-valores"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Definindo valores
Todos os valores no Redis são armazenados como um tipo ```RedisValue```:

    //"myvalue" here is implicitly converted to a RedisValue type
    //The RedisValue type is rarely seen in practice.
    db.StringSet("key", "aValue");

## Configurando e obtendo um int
    db.StringSet("key", 11021);
    int i = (int)db.StringGet("key");

Ou usando [StackExchange.Redis.Extensions](https://github.com/imperugo/StackExchange.Redis.Extensions):

    db.Add("key", 11021);
    int i = db.Get<int>("key");



