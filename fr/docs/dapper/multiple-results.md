---
title: "Résultats multiples"
slug: "resultats-multiples"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

## Syntaxe
- public static SqlMapper.GridReader QueryMultiple(this IDbConnection cnn, string sql, object param = null, IDbTransaction transaction = null, int? commandTimeout = null, CommandType? commandType = null)
- public statique SqlMapper.GridReader QueryMultiple (cette IDbConnection cnn, commande CommandDefinition)

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| cnn | Votre connexion à la base de données doit déjà être ouverte |
| sql | La chaîne sql à traiter contient plusieurs requêtes |
| paramètre | Objet dont on extrait les paramètres |
| SqlMapper.GridReader | Fournit des interfaces pour lire plusieurs ensembles de résultats à partir d'une requête Dapper |


## Exemple de résultats multiples de base
Pour récupérer plusieurs grilles dans une seule requête, la méthode "QueryMultiple" est utilisée. Cela vous permet ensuite de récupérer chaque grille * séquentiellement * par des appels successifs contre le `GridReader` renvoyé.

    var sql = @"select * from Customers where CustomerId = @id
                select * from Orders where CustomerId = @id
                select * from Returns where CustomerId = @id";
    
    using (var multi = connection.QueryMultiple(sql, new {id=selectedId}))
    {
       var customer = multi.Read<Customer>().Single();
       var orders = multi.Read<Order>().ToList();
       var returns = multi.Read<Return>().ToList();
    } 

