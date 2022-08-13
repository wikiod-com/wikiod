---
title: "Itérateurs"
slug: "iterateurs"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

Un itérateur est une méthode, un accesseur get ou un opérateur qui effectue une itération personnalisée sur un tableau ou une classe de collection à l'aide du mot-clé yield

## Création d'itérateurs à l'aide de Yield
Les itérateurs *produisent* des énumérateurs. En C#, les énumérateurs sont produits en définissant des méthodes, des propriétés ou des indexeurs qui contiennent des instructions `yield`.

La plupart des méthodes rendront le contrôle à leur appelant via des instructions `return` normales, qui suppriment tous les états locaux de cette méthode. En revanche, les méthodes qui utilisent des instructions `yield` leur permettent de renvoyer plusieurs valeurs à l'appelant sur demande tout en * préservant * l'état local entre les retours de ces valeurs. Ces valeurs renvoyées constituent une séquence. Il existe deux types d'instructions "yield" utilisées dans les itérateurs :

- `yield return`, qui rend le contrôle à l'appelant mais préserve l'état. L'appelé continuera l'exécution à partir de cette ligne lorsque le contrôle lui sera rendu.

- `yield break`, qui fonctionne de la même manière qu'une instruction `return` normale - cela signifie la fin de la séquence. Les instructions `return` normales elles-mêmes sont illégales dans un bloc d'itérateur.


Cet exemple ci-dessous illustre une méthode itérative qui peut être utilisée pour générer la [séquence de Fibonacci][1] :

    IEnumerable<int> Fibonacci(int count)
    {
        int prev = 1;
        int curr = 1;
        
        for (int i = 0; i < count; i++)
        {
            yield return prev;
            int temp = prev + curr;
            prev = curr;
            curr = temp;
        }
    }

Cet itérateur peut ensuite être utilisé pour produire un énumérateur de la suite de Fibonacci qui peut être consommé par une méthode appelante. Le code ci-dessous montre comment les dix premiers termes de la suite de Fibonacci peuvent être énumérés :

    void Main()
    {
        foreach (int term in Fibonacci(10))
        {
            Console.WriteLine(term);
        }
    }

**Production**

    1
    1
    2
    3
    5
    8
    13
    21
    34
    55

[1] : https://en.wikipedia.org/wiki/Fibonacci_number

## Exemple d'itérateur numérique simple

Un cas d'utilisation courant pour les itérateurs consiste à effectuer une opération sur une collection de nombres. L'exemple ci-dessous montre comment chaque élément d'un tableau de nombres peut être imprimé individuellement sur la console.

Cela est possible car les tableaux implémentent l'interface `IEnumerable`, permettant aux clients d'obtenir un itérateur pour le tableau à l'aide de la méthode `GetEnumerator()`. Cette méthode renvoie un *énumérateur*, qui est un curseur en lecture seule, avant uniquement sur chaque nombre du tableau.

    int[] numbers = { 1, 2, 3, 4, 5 };

    IEnumerator iterator = numbers.GetEnumerator();

    while (iterator.MoveNext())
    {
        Console.WriteLine(iterator.Current);
    }

**Production**

    1
    2
    3
    4
    5

Il est également possible d'obtenir les mêmes résultats en utilisant une instruction `foreach` :

    foreach (int number in numbers)
    {
        Console.WriteLine(number);
    }



