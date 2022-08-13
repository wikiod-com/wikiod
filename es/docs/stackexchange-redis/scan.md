---
title: "Escanear"
slug: "escanear"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Sintaxis
- `Public IEnumerable<RedisKey> Keys(int base de datos = 0, RedisValue patrón = predeterminado(RedisValue), int pageSize = CursorUtils.DefaultPageSize, cursor largo = CursorUtils.Origin, int pageOffset = 0, CommandFlags flags = CommandFlags.None)`

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| base de datos | Índice de la base de datos de Redis a la que conectarse|
| patrón | *No estoy seguro* |
| tamaño de página | Número de artículos a devolver por página |
| cursor | *No estoy seguro* |
| Desplazamiento de página | Número de páginas para compensar los resultados por |
| banderas | *No estoy seguro* |


La llamada `Keys()` seleccionará el comando `KEYS` o `SCAN` según la versión del servidor Redis. Siempre que sea posible, preferirá el uso de `SCAN`, que devuelve un `IEnumerable<RedisKey>` y no bloquea. `KEYS`, por otro lado, se bloqueará al escanear el espacio de la clave.

## Escaneo básico de todas las claves en el servidor
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    // Write out each key in the server
    foreach(var key in server.Keys()) {
        Console.WriteLine(key);
    }



## Iterando usando un cursor
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    var seq = server.Keys();
    IScanningCursor scanningCursor = (IScanningCursor)seq;
       
    // Use the cursor in some way...

