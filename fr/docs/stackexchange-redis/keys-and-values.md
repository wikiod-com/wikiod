---
title: "Clés et valeurs"
slug: "cles-et-valeurs"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Valeurs de réglage
Toutes les valeurs dans Redis sont finalement stockées sous la forme d'un type ```RedisValue``` :

    //"myvalue" here is implicitly converted to a RedisValue type
    //The RedisValue type is rarely seen in practice.
    db.StringSet("key", "aValue");

## Définition et obtention d'un int
    db.StringSet("key", 11021);
    int i = (int)db.StringGet("key");

Ou en utilisant [StackExchange.Redis.Extensions](https://github.com/imperugo/StackExchange.Redis.Extensions) :

    db.Add("key", 11021);
    int i = db.Get<int>("key");



