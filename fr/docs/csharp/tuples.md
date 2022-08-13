---
title: "Tuples"
slug: "tuples"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Accéder aux éléments de tuple
Pour accéder aux éléments de tuple, utilisez les propriétés `Item1`-`Item8`. Seules les propriétés avec un numéro d'index inférieur ou égal à la taille du tuple seront disponibles (c'est-à-dire qu'on ne peut pas accéder à la propriété `Item3` dans `Tuple<T1,T2>`).

    var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());
    var item1 = tuple.Item1; // "foo"
    var item2 = tuple.Item2; // 123
    var item3 = tuple.Item3; // true
    var item4 = tuple.Item4; // new My Class()

## Création de tuples
Les tuples sont créés en utilisant les types génériques `Tuple<T1>`-`Tuple<T1,T2,T3,T4,T5,T6,T7,T8>`. Chacun des types représente un tuple contenant de 1 à 8 éléments. Les éléments peuvent être de différents types.

    // tuple with 4 elements
    var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());

Les tuples peuvent également être créés à l'aide de méthodes statiques "Tuple.Create". Dans ce cas, les types des éléments sont déduits par le compilateur C#.

    // tuple with 4 elements
    var tuple = Tuple.Create("foo", 123, true, new MyClass());
<!-- si version [gte 7.0] -->
Depuis C# 7.0, les tuples peuvent être facilement créés à l'aide de [ValueTuple][1].

    var tuple = ("foo", 123, true, new MyClass());

Les éléments peuvent être nommés pour une décomposition plus facile.

    (int number, bool flag, MyClass instance) tuple = (123, true, new MyClass());

<!-- fin de version si -->


[1] : https://www.wikiod.com/fr/docs/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514

## Comparer et trier les tuples
Les tuples peuvent être comparés en fonction de leurs éléments.

Par exemple, un énumérable dont les éléments sont de type `Tuple` peut être trié en fonction d'opérateurs de comparaison définis sur un élément spécifié :

    List<Tuple<int, string>> list = new List<Tuple<int, string>>();
    list.Add(new Tuple<int, string>(2, "foo"));
    list.Add(new Tuple<int, string>(1, "bar"));
    list.Add(new Tuple<int, string>(3, "qux"));

    list.Sort((a, b) => a.Item2.CompareTo(b.Item2)); //sort based on the string element

    foreach (var element in list) {
        Console.WriteLine(element);
    }
    
    // Output:
    // (1, bar)
    // (2, foo)
    // (3, qux)

Ou pour inverser le tri, utilisez :

    list.Sort((a, b) => b.Item2.CompareTo(a.Item2));

## Renvoie plusieurs valeurs d'une méthode
Les tuples peuvent être utilisés pour renvoyer plusieurs valeurs à partir d'une méthode sans utiliser de paramètres. Dans l'exemple suivant, `AddMultiply` est utilisé pour renvoyer deux valeurs (somme, produit).

    void Write()
    {
        var result = AddMultiply(25, 28);
        Console.WriteLine(result.Item1);
        Console.WriteLine(result.Item2);
    }
 
    Tuple<int, int> AddMultiply(int a, int b)
    {
        return new Tuple<int, int>(a + b, a * b);
    }

Production:

> 53
> 700


Désormais, C# 7.0 offre un autre moyen de renvoyer plusieurs valeurs à partir de méthodes utilisant des tuples de valeurs [Plus d'informations sur la structure `ValueTuple`] [1].


[1] : https://www.wikiod.com/fr/docs/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514

