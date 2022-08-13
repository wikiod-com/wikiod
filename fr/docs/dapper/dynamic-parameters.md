---
title: "Paramètres dynamiques"
slug: "parametres-dynamiques"
draft: false
images: []
weight: 9497
type: docs
toc: true
---

## Utilisation de base
Il n'est pas toujours possible de regrouper soigneusement tous les paramètres dans un seul objet/appel. Pour aider avec des scénarios plus compliqués, dapper permet au paramètre `param` d'être une instance `IDynamicParameters`. Si vous faites cela, votre méthode personnalisée `AddParameters` est appelée au moment approprié et reçoit la commande à ajouter. Dans la plupart des cas, cependant, il suffit d'utiliser le type "DynamicParameters" préexistant :

    var p = new DynamicParameters(new { a = 1, b = 2 });
    p.Add("c", dbType: DbType.Int32, direction: ParameterDirection.Output);
    connection.Execute(@"set @c = @a + @b", p);
    int updatedValue = p.Get<int>("@c");

Ceci montre:

- (facultatif) peuplement à partir d'un objet existant
- (facultatif) ajout de paramètres supplémentaires à la volée
- passer les paramètres à la commande
- récupérer toute valeur mise à jour après la fin de la commande

Notez qu'en raison du fonctionnement des protocoles RDBMS, il n'est généralement fiable d'obtenir des valeurs de paramètre mises à jour **qu'après** que des données (à partir d'une opération `Query` ou QueryMultiple`) aient été **complètement** consommées (par exemple, sur SQL Server, les valeurs de paramètre mises à jour se trouvent à la *fin* du flux TDS).

## Paramètres dynamiques dans Dapper
    
    connection.Execute(@"some Query with @a,@b,@c", new {a=somevalueOfa,b=somevalueOfb,c=somevalueOfc});

## Utilisation d'un objet modèle
Vous pouvez utiliser une instance d'un objet pour former vos paramètres

    public class SearchParameters {
      public string SearchString { get; set; }
      public int Page { get; set; }
    }

    var template= new SearchParameters {
      SearchString = "Dapper",
      Page = 1
    };

    var p = new DynamicParameters(template);

Vous pouvez également utiliser un objet anonyme ou un "Dictionnaire"

