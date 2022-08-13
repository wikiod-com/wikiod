---
title: "Analyse"
slug: "analyse"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Syntaxe
- `clés publiques IEnumerable<RedisKey>(base de données int = 0, modèle RedisValue = default(RedisValue), int pageSize = CursorUtils.DefaultPageSize, curseur long = CursorUtils.Origin, int pageOffset = 0, indicateurs CommandFlags = CommandFlags.None)`

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| base de données | Index de la base de données Redis pour se connecter |
| motif | *Incertain* |
| pageTaille | Nombre d'articles à retourner par page |
| curseur | *Incertain* |
| décalage de page| Nombre de pages pour décaler les résultats de |
| drapeaux | *Incertain* |


L'appel `Keys()` sélectionnera la commande `KEYS` ou `SCAN` en fonction de la version du serveur Redis. Dans la mesure du possible, il préférera l'utilisation de `SCAN` qui renvoie un `IEnumerable<RedisKey>` et ne bloque pas. `KEYS`, d'autre part, bloquera lors de la numérisation de l'espace clé.

## Analyse de base de toutes les clés sur le serveur
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    // Write out each key in the server
    foreach(var key in server.Keys()) {
        Console.WriteLine(key);
    }



## Itération à l'aide d'un curseur
    // Connect to a target server using your ConnectionMultiplexer instance
    IServer server = conn.GetServer("localhost", 6379);
    
    var seq = server.Keys();
    IScanningCursor scanningCursor = (IScanningCursor)seq;
       
    // Use the cursor in some way...

