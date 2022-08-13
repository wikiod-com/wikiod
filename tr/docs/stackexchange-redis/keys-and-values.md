---
title: "Anahtarlar ve Değerler"
slug: "anahtarlar-ve-degerler"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Değerleri Ayarlama
Redis'teki tüm değerler sonuçta bir "RedisValue" türü olarak depolanır:

    //"myvalue" here is implicitly converted to a RedisValue type
    //The RedisValue type is rarely seen in practice.
    db.StringSet("key", "aValue");

## Bir int ayarlama ve alma
    db.StringSet("key", 11021);
    int i = (int)db.StringGet("key");

Veya [StackExchange.Redis.Extensions](https://github.com/imperugo/StackExchange.Redis.Extensions) kullanarak:

    db.Add("key", 11021);
    int i = db.Get<int>("key");



