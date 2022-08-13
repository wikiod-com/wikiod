---
title: "Types anonymes"
slug: "types-anonymes"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Anonyme vs dynamique
Les types anonymes permettent la création d'objets sans avoir à définir explicitement leurs types à l'avance, tout en maintenant une vérification de type statique.

    var anon = new { Value = 1 };
    Console.WriteLine(anon.Id); // compile time error

Inversement, `dynamic` a une vérification de type dynamique, optant pour les erreurs d'exécution, au lieu des erreurs de compilation.
    
    dynamic val = "foo";
    Console.WriteLine(val.Id); // compiles, but throws runtime error

## Création d'un type anonyme
Comme les types anonymes ne sont pas nommés, les variables de ces types doivent être implicitement typées (`var`).

    var anon = new { Foo = 1, Bar = 2 };
    // anon.Foo == 1
    // anon.Bar == 2
    
Si les noms de membre ne sont pas spécifiés, ils sont définis sur le nom de la propriété/variable utilisée pour initialiser l'objet.

    int foo = 1;
    int bar = 2;
    var anon2 = new { foo, bar };
    // anon2.foo == 1
    // anon2.bar == 2

Notez que les noms ne peuvent être omis que lorsque l'expression dans la déclaration de type anonyme est un simple accès à une propriété ; pour les appels de méthode ou les expressions plus complexes, un nom de propriété doit être spécifié.

    string foo = "some string";
    var anon3 = new { foo.Length };
    // anon3.Length == 11
    var anon4 = new { foo.Length <= 10 ? "short string" : "long string" };
    // compiler error - Invalid anonymous type member declarator.
    var anon5 = new { Description = foo.Length <= 10 ? "short string" : "long string" };
    // OK

## Égalité de type anonyme
L'égalité de type anonyme est donnée par la méthode d'instance `Equals`. Deux objets sont égaux s'ils ont le même type et des valeurs égales (via `a.Prop.Equals(b.Prop)`) pour chaque propriété.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 1, Bar = 2 };
    var anon3 = new { Foo = 5, Bar = 10 };
    var anon3 = new { Foo = 5, Bar = 10 };
    var anon4 = new { Bar = 2, Foo = 1 };
    // anon.Equals(anon2) == true
    // anon.Equals(anon3) == false
    // anon.Equals(anon4) == false (anon and anon4 have different types, see below)

Deux types anonymes sont considérés comme identiques si et seulement si leurs propriétés ont le même nom et le même type et apparaissent dans le même ordre.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 7, Bar = 1 };
    var anon3 = new { Bar = 1, Foo = 3 };
    var anon4 = new { Fa = 1, Bar = 2 };
    // anon and anon2 have the same type
    // anon and anon3 have diferent types (Bar and Foo appear in different orders)
    // anon and anon4 have different types (property names are different)

## Méthodes génériques avec des types anonymes
Les méthodes génériques permettent l'utilisation de types anonymes via l'inférence de type.

    void Log<T>(T obj) {
        // ...
    }
    Log(new { Value = 10 });

Cela signifie que les expressions LINQ peuvent être utilisées avec des types anonymes :

    var products = new[] {
        new { Amount = 10, Id = 0 },
        new { Amount = 20, Id = 1 },
        new { Amount = 15, Id = 2 }
    };
    var idsByAmount = products.OrderBy(x => x.Amount).Select(x => x.Id);
    // idsByAmount: 0, 2, 1

## Instanciation de types génériques avec des types anonymes
L'utilisation de constructeurs génériques nécessiterait que les types anonymes soient nommés, ce qui n'est pas possible. Alternativement, des méthodes génériques peuvent être utilisées pour permettre l'inférence de type.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 5, Bar = 10 };
    List<T> CreateList<T>(params T[] items) {
        return new List<T>(items);
    }
    
    var list1 = CreateList(anon, anon2);

Dans le cas de `List<T>`, les tableaux implicitement typés peuvent être convertis en `List<T>` via la méthode LINQ `ToList` :

    var list2 = new[] {anon, anon2}.ToList();

## Tableaux implicitement typés
Des tableaux de types anonymes peuvent être créés avec un typage implicite.

    var arr = new[] {
        new { Id = 0 },
        new { Id = 1 }
    };

