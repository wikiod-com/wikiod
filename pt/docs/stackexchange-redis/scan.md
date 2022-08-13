---
title: "Varredura"
slug: "varredura"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Sintaxe
- `public IEnumerable<RedisKey> Keys(int database = 0, RedisValue pattern = default(RedisValue), int pageSize = CursorUtils.DefaultPageSize, long cursor = CursorUtils.Origin, int pageOffset = 0, CommandFlags flags = CommandFlags.None)`

## Parâmetros
| Parâmetro | Detalhes |
| --------- | ------- |  
| banco de dados | Índice do banco de dados Redis para se conectar |
| padrão | *Não tenho certeza* |
| pageSize | Número de itens a devolver por página |
| cursor | *Não tenho certeza* |
| pageOffset| Número de páginas para compensar os resultados por |
| bandeiras | *Não tenho certeza* |


A chamada `Keys()` selecionará o comando `KEYS` ou `SCAN` com base na versão do servidor Redis. Sempre que possível, ele preferirá o uso de `SCAN` que retorna um `IEnumerable<RedisKey>` e não bloqueia. `KEYS`, por outro lado, bloqueará ao digitalizar o espaço da chave.

## Verificação básica de todas as chaves no servidor
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    // Write out each key in the server
    foreach(var key in server.Keys()) {
        Console.WriteLine(key);
    }



## Iterando usando um cursor
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    var seq = server.Keys();
    IScanningCursor scanningCursor = (IScanningCursor)seq;
       
    // Use the cursor in some way...

