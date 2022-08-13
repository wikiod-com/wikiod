---
title: "Tarama"
slug: "tarama"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Sözdizimi
- `public IEnumerable<RedisKey> Anahtarlar(int database = 0, RedisValue pattern = default(RedisValue), int pageSize = CursorUtils.DefaultPageSize, long imleç = CursorUtils.Origin, int pageOffset = 0, CommandFlags flags = CommandFlags.None)`

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| veritabanı | Bağlanılacak Redis veritabanı dizini|
| desen | *Emin değilim* |
| sayfaBoyutu | Sayfa başına döndürülecek öğe sayısı |
| imleç | *Emin değilim* |
| sayfaOfset| Sonuçları dengelemek için sayfa sayısı |
| bayraklar | *Emin değilim* |


`Keys()` çağrısı, Redis sunucusunun sürümüne bağlı olarak `KEYS` veya `SCAN` komutunu seçecektir. Mümkün olduğunda, bir "IEnumerable<RedisKey>" döndüren ve engellemeyen "SCAN" kullanımını tercih edecektir. Öte yandan `KEYS`, anahtar alanı tararken engellenecektir.

## Sunucudaki tüm anahtarların temel olarak taranması
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    // Write out each key in the server
    foreach(var key in server.Keys()) {
        Console.WriteLine(key);
    }



## İmleç kullanarak yineleme
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    var seq = server.Keys();
    IScanningCursor scanningCursor = (IScanningCursor)seq;
       
    // Use the cursor in some way...

