---
title: "Interface interrogeable"
slug: "interface-interrogeable"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Traduction d'une requête LINQ en requête SQL
Les interfaces `IQueryable` et `IQueryable<T>` permettent aux développeurs de traduire une requête LINQ (une requête 'intégrée au langage') vers une source de données spécifique, par exemple une base de données relationnelle. Prenez cette requête LINQ écrite en C# :

    var query = from book in books
                where book.Author == "Stephen King" 
                select book;

Si la variable `books` est d'un type qui implémente `IQueryable<Book>` alors la requête ci-dessus est transmise au fournisseur (défini sur la propriété `IQueryable.Provider`) sous la forme d'un arbre d'expression, une structure de données qui reflète la structure du code.

Le fournisseur peut inspecter l'arborescence d'expressions lors de l'exécution pour déterminer :

- qu'il existe un prédicat pour la propriété `Author` de la classe `Book` ;
- que la méthode de comparaison utilisée est 'equals' (`==`);
- que la valeur qu'il doit égaler est `"Stephen King"`.

Avec ces informations, le fournisseur peut traduire la requête C# en une requête SQL lors de l'exécution et transmettre cette requête à une base de données relationnelle pour récupérer uniquement les livres qui correspondent au prédicat :

    select *
    from Books
    where Author = 'Stephen King'

Le fournisseur est appelé lorsque la variable `query` est itérée (`IQueryable` implémente `IEnumerable`).

(Le fournisseur utilisé dans cet exemple nécessiterait des métadonnées supplémentaires pour savoir quelle table interroger et savoir comment faire correspondre les propriétés de la classe C# aux colonnes de la table, mais ces métadonnées sortent du cadre de l'interface `IQueryable`. )

