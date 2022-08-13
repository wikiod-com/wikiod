---
title: "Bouclage"
slug: "bouclage"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Boucle Foreach
foreach itérera sur tout objet d'une classe qui implémente `IEnumerable` (notez que `IEnumerable<T>` en hérite). Ces objets incluent certains objets intégrés, mais sans s'y limiter : `List<T>`, `T[]` (tableaux de tout type), `Dictionary<TKey, TSource>`, ainsi que des interfaces comme `IQueryable` et `ICollection`, etc.

**syntaxe**

    foreach(ItemType itemVariable in enumerableObject)
        statement;

**remarques**

1. Le type `ItemType` n'a pas besoin de correspondre au type précis des éléments, il doit juste être attribuable à partir du type des éléments
2. Au lieu de `ItemType`, vous pouvez également utiliser `var` qui déduira le type d'éléments de l'enumerableObject en inspectant l'argument générique de l'implémentation `IEnumerable`
3. L'instruction peut être un bloc, une seule instruction ou même une instruction vide (`;`)
4. Si `enumerableObject` n'implémente pas `IEnumerable`, le code ne compilera pas
5. Au cours de chaque itération, l'élément actuel est casté en `ItemType` (même si cela n'est pas spécifié mais déduit par le compilateur via `var`) et si l'élément ne peut pas être transtypé, une `InvalidCastException` sera levée.

Considérez cet exemple :

    var list = new List<string>();
    list.Add("Ion");
    list.Add("Andrei");
    foreach(var name in list)
    {
        Console.WriteLine("Hello " + name);
    }

est équivalent à:

    var list = new List<string>();
    list.Add("Ion");
    list.Add("Andrei");
    IEnumerator enumerator;
    try
    {
        enumerator = list.GetEnumerator();
        while(enumerator.MoveNext())
        {
            string name = (string)enumerator.Current;
            Console.WriteLine("Hello " + name);
        }
    }
    finally
    {
        if (enumerator != null)
            enumerator.Dispose();
    }

## Pour la boucle
Une boucle For est idéale pour faire des choses pendant un certain temps. C'est comme une boucle While mais l'incrément est inclus dans la condition.

Une boucle For est configurée comme ceci :

    for (Initialization; Condition; Increment)
    {
        // Code
    }

> Initialisation - Crée une nouvelle variable locale qui ne peut être utilisée que dans la boucle.
> Condition - La boucle ne s'exécute que lorsque la condition est vraie.
> Incrément - Comment la variable change à chaque fois que la boucle s'exécute.

Un exemple:

    for (int i = 0; i < 5; i++)
    {
        Console.WriteLine(i);
    }

Production:

> 0
> 1
> 2
> 3
> 4

Vous pouvez également omettre des espaces dans la boucle For, mais vous devez avoir tous les points-virgules pour que cela fonctionne.

    int input = Console.ReadLine();    

    for ( ; input < 10; input + 2)
    {
        Console.WriteLine(input);
    }

Sortie pour 3 :
>3
>5
>7
>9
>11

## Do - Boucle While
Elle est similaire à une boucle `while`, sauf qu'elle teste la condition à la *fin* du corps de la boucle. La boucle Do - While exécute la boucle une fois, que la condition soit vraie ou non.

    int[] numbers = new int[] { 6, 7, 8, 10 };
        
    // Sum values from the array until we get a total that's greater than 10,
    // or until we run out of values.
    int sum = 0;
    int i = 0;
    do
    {
        sum += numbers[i];
        i++;
    } while (sum <= 10 && i < numbers.Length);
        
    System.Console.WriteLine(sum); // 13


## Styles de boucle
**Alors que**

Le type de boucle le plus trivial. Le seul inconvénient est qu'il n'y a aucun indice intrinsèque pour savoir où vous en êtes dans la boucle.

    /// loop while the condition satisfies
    while(condition)
    {
        /// do something
    }

**Fais**

Semblable à `while`, mais la condition est évaluée à la fin de la boucle au lieu du début. Cela se traduit par l'exécution des boucles au moins une fois.

    do
    {
        /// do something
    } while(condition) /// loop while the condition satisfies


**Pour**

Un autre style de boucle trivial. Pendant la boucle, un index (`i`) est augmenté et vous pouvez l'utiliser. Il est généralement utilisé pour gérer les tableaux.

    for ( int i = 0; i < array.Count; i++ )
    {
        var currentItem = array[i];
        /// do something with "currentItem"
    }

**Pour chaque**

Manière modernisée de parcourir les objets `IEnumarable`. Heureusement que vous n'avez pas à penser à l'index de l'élément ou au nombre d'éléments de la liste.

    foreach ( var item in someList )
    {
        /// do something with "item"
    }

**Méthode Foreach**

Alors que les autres styles sont utilisés pour sélectionner ou mettre à jour les éléments dans les collections, ce style est généralement utilisé pour * appeler une méthode * immédiatement pour tous les éléments d'une collection.

    list.ForEach(item => item.DoSomething());

    // or
    list.ForEach(item => DoSomething(item));

    // or using a method group
    list.ForEach(Console.WriteLine);

    // using an array
    Array.ForEach(myArray, Console.WriteLine);

Il est important de noter que cette méthode n'est disponible que sur les instances `List<T>` et en tant que méthode statique sur `Array` - elle ne fait **pas** partie de Linq.

**Linq Parallel Foreach**

Tout comme Linq Foreach, sauf que celui-ci fait le travail en parallèle. Cela signifie que tous les éléments de la collection exécuteront l'action donnée en même temps, simultanément.

    collection.AsParallel().ForAll(item => item.DoSomething());

    /// or
    collection.AsParallel().ForAll(item => DoSomething(item));

## Boucles imbriquées
    // Print the multiplication table up to 5s
    for (int i = 1; i <= 5; i++)
    {
        for (int j = 1; j <= 5; j++)
        {
            int product = i * j;
            Console.WriteLine("{0} times {1} is {2}", i, j, product);
        }
    }

## Pause
Parfois, la condition de boucle doit être vérifiée au milieu de la boucle. Le premier est sans doute plus élégant que le second :

    for (;;)
    {
        // precondition code that can change the value of should_end_loop expression
    
        if (should_end_loop)
            break;
    
        // do something
    }

Alternative:

    bool endLoop = false;
    for (; !endLoop;)
    {
        // precondition code that can set endLoop flag
    
        if (!endLoop)
        {
            // do something
        }
    }

Remarque : dans les boucles imbriquées et/ou `switch` doit utiliser plus qu'un simple `break`.

## Boucle tant que
    int n = 0;
    while (n < 5) 
    {
        Console.WriteLine(n);
        n++;
    }

Production:

> 0
> 1
> 2
> 3
> 4

Les IEnumerators peuvent être itérés avec une boucle while :

    // Call a custom method that takes a count, and returns an IEnumerator for a list
    // of strings with the names of theh largest city metro areas.
    IEnumerator<string> largestMetroAreas = GetLargestMetroAreas(4);

    while (largestMetroAreas.MoveNext())
    {
        Console.WriteLine(largestMetroAreas.Current);
    }

Exemple de sortie :

> Tokyo/Yokohama
> Métro de New York
> São Paulo
> Séoul/Incheon

## Continuez
En plus de `break`, il y a aussi le mot-clé `continue`. Au lieu de casser complètement la boucle, il sautera simplement l'itération en cours. Cela peut être utile si vous ne voulez pas qu'un code soit exécuté si une valeur particulière est définie.

Voici un exemple simple :

    for (int i = 1; i <= 10; i++)
    {
        if (i < 9)
            continue;

        Console.WriteLine(i);
    }

Aura pour résultat:

    9
    10

**Remarque :** "Continuer" est souvent plus utile dans les boucles while ou do-while. Les boucles for, avec des conditions de sortie bien définies, peuvent ne pas en bénéficier autant.

