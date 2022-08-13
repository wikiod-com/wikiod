---
title: "Mot clé de rendement"
slug: "mot-cle-de-rendement"
draft: false
images: []
weight: 8758
type: docs
toc: true
---

Lorsque vous utilisez le mot clé yield dans une instruction, vous indiquez que la méthode, l'opérateur ou l'accesseur get dans lequel il apparaît est un itérateur. L'utilisation de yield pour définir un itérateur supprime le besoin d'une classe supplémentaire explicite (la classe qui contient l'état d'une énumération) lorsque vous implémentez le modèle IEnumerable et IEnumerator pour un type de collection personnalisé.

## Syntaxe
- rendement de rendement [TYPE]
- rupture de rendement

Mettre le mot clé `yield` dans une méthode avec le type de retour `IEnumerable`, `IEnumerable<T>`, `IEnumerator` ou `IEnumerator<T>` indique au compilateur de générer une implémentation du type de retour (`IEnumerable ` ou `IEnumerator`) qui, lorsqu'il est bouclé, exécute la méthode jusqu'à chaque "rendement" pour obtenir chaque résultat.

Le mot clé `yield` est utile lorsque vous souhaitez renvoyer "l'élément suivant" d'une séquence théoriquement illimitée, de sorte qu'il serait impossible de calculer la séquence entière à l'avance, ou lorsque le calcul de la séquence complète de valeurs avant de revenir conduirait à une pause indésirable pour l'utilisateur.

`yield break` peut également être utilisé pour terminer la séquence à tout moment.

Comme le mot clé `yield` nécessite un type d'interface d'itérateur comme type de retour, tel que `IEnumerable<T>`, vous ne pouvez pas l'utiliser dans une méthode asynchrone car cela renvoie un objet `Task<IEnumerable<T>>`.

**Autres lectures**

- https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx

## Utilisation simple
Le mot clé `yield` est utilisé pour définir une fonction qui renvoie un `IEnumerable` ou `IEnumerator` (ainsi que leurs variantes génériques dérivées) dont les valeurs sont générées paresseusement lorsqu'un appelant parcourt la collection renvoyée. En savoir plus sur l'objectif dans la [section remarques](https://www.wikiod.com/fr/docs/c%23/61/yield-keyword#remarks).

L'exemple suivant a une instruction yield return qui se trouve à l'intérieur d'une boucle `for`.

    public static IEnumerable<int> Count(int start, int count)
    {
        for (int i = 0; i <= count; i++)
        {
            yield return start + i;
        }
    }

Ensuite, vous pouvez l'appeler:

    foreach (int value in Count(start: 4, count: 10))
    {
        Console.WriteLine(value);
    }

**Sortie console**

    4
    5
    6
    ...
    14


[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/qtKObr)

Chaque itération du corps de l'instruction `foreach` crée un appel à la fonction d'itération `Count`. Chaque appel à la fonction itérateur procède à la prochaine exécution de l'instruction `yield return`, qui se produit lors de la prochaine itération de la boucle `for`.

## Vérifier correctement les arguments
Une méthode d'itérateur n'est pas exécutée tant que la valeur de retour n'est pas énumérée. Il est donc avantageux d'affirmer les préconditions en dehors de l'itérateur.

    public static IEnumerable<int> Count(int start, int count)
    {
        // The exception will throw when the method is called, not when the result is iterated
        if (count < 0)
            throw new ArgumentOutOfRangeException(nameof(count));

        return CountCore(start, count);
    }

    private static IEnumerable<int> CountCore(int start, int count)
    {
        // If the exception was thrown here it would be raised during the first MoveNext()
        // call on the IEnumerator, potentially at a point in the code far away from where
        // an incorrect value was passed.
        for (int i = 0; i < count; i++)
        {
            yield return start + i;
        }
    }

**Code côté appelant (utilisation) :**
        
    // Get the count
    var count = Count(1,10);
    // Iterate the results
    foreach(var x in count)
    {
        Console.WriteLine(x);
    }
**Production:**
>1
>2
>3
>4
>5
>6
>7
>8
>9
>10

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/yIYxo6)

Lorsqu'une méthode utilise `yield` pour générer un énumérable, le compilateur crée une machine à états qui, une fois itérée, exécutera du code jusqu'à un `yield`. Il renvoie ensuite l'élément généré et enregistre son état.

Cela signifie que vous ne découvrirez pas les arguments invalides (en passant `null` etc.) lorsque vous appelez la méthode pour la première fois (car cela crée la machine d'état), uniquement lorsque vous essayez d'accéder au premier élément (car alors seulement le code dans la méthode est exécuté par la machine d'état). En l'enveloppant dans une méthode normale qui vérifie d'abord les arguments, vous pouvez les vérifier lorsque la méthode est appelée. Ceci est un exemple d'échec rapide.

Lors de l'utilisation de C # 7+, la fonction `CountCore` peut être facilement masquée dans la fonction `Count` en tant que _fonction locale_. Voir exemple [ici](https://www.wikiod.com/fr/docs/c%23/1936/c-sharp-7-0-features/6330/local-functions#t=201607251321358412005#t=201607251057101259341).

## Résiliation anticipée
Vous pouvez étendre la fonctionnalité des méthodes `yield` existantes en transmettant une ou plusieurs valeurs ou éléments qui pourraient définir une condition de fin dans la fonction en appelant un `yield break` pour arrêter l'exécution de la boucle interne.

    public static IEnumerable<int> CountUntilAny(int start, HashSet<int> earlyTerminationSet)
    {
        int curr = start;

        while (true)
        {
            if (earlyTerminationSet.Contains(curr))
            {
                // we've hit one of the ending values
                yield break;
            }

            yield return curr;

            if (curr == Int32.MaxValue)
            {
                // don't overflow if we get all the way to the end; just stop
                yield break;
            }

            curr++;
        }
    }

La méthode ci-dessus itérerait à partir d'une position `start` donnée jusqu'à ce que l'une des valeurs de `earlyTerminationSet` soit rencontrée.

    // Iterate from a starting point until you encounter any elements defined as 
    // terminating elements
    var terminatingElements = new HashSet<int>{ 7, 9, 11 };
    // This will iterate from 1 until one of the terminating elements is encountered (7)
    foreach(var x in CountUntilAny(1,terminatingElements))
    {
        // This will write out the results from 1 until 7 (which will trigger terminating)
        Console.WriteLine(x);
    }
**Production:**
>1
>2
>3
>4
>5
>6

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/pctiOz)

## Utilisation plus pertinente
    public IEnumerable<User> SelectUsers()
    {
        // Execute an SQL query on a database.
        using (IDataReader reader = this.Database.ExecuteReader(CommandType.Text, "SELECT Id, Name FROM Users"))
        {
            while (reader.Read())
            {
                int id = reader.GetInt32(0);
                string name = reader.GetString(1);
                yield return new User(id, name);
            }
        }
    }

Il existe d'autres façons d'obtenir un `IEnumerable<User>` à partir d'une base de données SQL, bien sûr - cela démontre simplement que vous pouvez utiliser `yield` pour transformer tout ce qui a une sémantique de "séquence d'éléments" en un `IEnumerable<T> ` que quelqu'un peut parcourir.

## Évaluation paresseuse
Ce n'est que lorsque l'instruction `foreach` passe à l'élément suivant que le bloc itérateur évalue jusqu'à l'instruction `yield` suivante.

Considérez l'exemple suivant :

    private IEnumerable<int> Integers()
    {
        var i = 0;
        while(true)
        {
            Console.WriteLine("Inside iterator: " + i);
            yield return i;
            i++;
        }
    }
    
    private void PrintNumbers()
    {
        var numbers = Integers().Take(3);
        Console.WriteLine("Starting iteration");

        foreach(var number in numbers)
        {
            Console.WriteLine("Inside foreach: " + number);
        }
    }


Cela affichera :

>Démarrage de l'itération
>Itérateur intérieur : 0
> À l'intérieur de chaque : 0
> Itérateur intérieur : 1
> Intérieur pour chaque : 1
> Itérateur intérieur : 2
> Intérieur pour chaque : 2

[Voir la démo][1]

En conséquence:

- "Starting iteration" est imprimé en premier même si la méthode iterator a été appelée avant la ligne qui l'imprime car la ligne `Integers().Take(3);` ne démarre pas réellement l'itération (pas d'appel à `IEnumerator.MoveNext()` a été faite)
- Les lignes imprimées sur la console alternent entre celle à l'intérieur de la méthode iterator et celle à l'intérieur de `foreach`, plutôt que toutes celles à l'intérieur de la méthode iterator évaluant en premier
- Ce programme se termine à cause de la méthode `.Take()`, même si la méthode de l'itérateur a un `while true` dont il ne sort jamais.


[1] : https://dotnetfiddle.net/2qGV0B

## Essayez... enfin
Si une méthode d'itérateur a un rendement à l'intérieur d'un `try...finally`, alors le `IEnumerator` renvoyé exécutera l'instruction `finally` lorsque `Dispose` est appelé dessus, tant que le point d'évaluation actuel est à l'intérieur du bloc "essayer".

Étant donné la fonction :

    private IEnumerable<int> Numbers()
    {
        yield return 1;
        try
        {
            yield return 2;
            yield return 3;
        }
        finally
        {
            Console.WriteLine("Finally executed");
        }
    }

Lors de l'appel :

    private void DisposeOutsideTry()
    {
        var enumerator = Numbers().GetEnumerator();

        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.Dispose();
    }

Ensuite, il imprime :
>1

[Voir la démo][1]

Lors de l'appel :

    private void DisposeInsideTry()
    {
        var enumerator = Numbers().GetEnumerator();

        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.Dispose();
    }

Ensuite, il imprime :
>1
>2
>Enfin exécuté

[Voir la démo][2]


[1] : https://dotnetfiddle.net/MJt7dt
[2] : https://dotnetfiddle.net/HlMroV

## Utilisation de yield pour créer un IEnumerator<T> lors de l'implémentation de IEnumerable<T>
L'interface `IEnumerable<T>` a une seule méthode, `GetEnumerator()`, qui renvoie un `IEnumerator<T>`.

Alors que le mot clé `yield` peut être utilisé pour créer directement un `IEnumerable<T>`, il peut *également* être utilisé exactement de la même manière pour créer un `IEnumerator<T>`. La seule chose qui change est le type de retour de la méthode.

Cela peut être utile si nous voulons créer notre propre classe qui implémente `IEnumerable<T>` :

    public class PrintingEnumerable<T> : IEnumerable<T>
    {
        private IEnumerable<T> _wrapped;
    
        public PrintingEnumerable(IEnumerable<T> wrapped)
        {
            _wrapped = wrapped;
        }
    
        // This method returns an IEnumerator<T>, rather than an IEnumerable<T>
        // But the yield syntax and usage is identical.
        public IEnumerator<T> GetEnumerator()
        {
            foreach(var item in _wrapped)
            {
                Console.WriteLine("Yielding: " + item);
                yield return item;
            }
        }
    
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

(Notez que cet exemple particulier est juste illustratif et pourrait être implémenté plus proprement avec une seule méthode itérative renvoyant un `IEnumerable<T>`.)
    

## Évaluation impatiente
Le mot clé `yield` permet une évaluation paresseuse de la collection. Le chargement forcé de toute la collection en mémoire s'appelle **évaluation hâtive**.

Le code suivant le montre :

    IEnumerable<int> myMethod()
    {
        for(int i=0; i <= 8675309; i++)
        {
            yield return i;
        }
    }
    ...
    // define the iterator
    var it = myMethod.Take(3);
    // force its immediate evaluation
    // list will contain 0, 1, 2
    var list = it.ToList();

Appeler `ToList`, `ToDictionary` ou `ToArray` forcera l'évaluation immédiate de l'énumération, récupérant tous les éléments dans une collection.

## Renvoie un autre Enumerable dans une méthode renvoyant Enumerable
    public IEnumerable<int> F1()
    {
        for (int i = 0; i < 3; i++)
            yield return i;
    
        //return F2(); // Compile Error!!
        foreach (var element in F2())
            yield return element;
    }
    
    public int[] F2()
    {
        return new[] { 3, 4, 5 };
    }

## Exemple d'évaluation paresseuse : nombres de Fibonacci
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Numerics; // also add reference to System.Numberics
    
    namespace ConsoleApplication33
    {
        class Program
        {
            private static IEnumerable<BigInteger> Fibonacci()
            {
                BigInteger prev = 0;
                BigInteger current = 1;
                while (true)
                {
                    yield return current;
                    var next = prev + current;
                    prev = current;
                    current = next;
                }
            }
    
            static void Main()
            {
                // print Fibonacci numbers from 10001 to 10010
                var numbers = Fibonacci().Skip(10000).Take(10).ToArray();
                Console.WriteLine(string.Join(Environment.NewLine, numbers));
            }
        }
    }

Comment cela fonctionne sous le capot (je recommande de décompiler le fichier .exe résultant dans l'outil IL Disaambler):
1. Le compilateur C# génère une classe implémentant `IEnumerable<BigInteger>` et `IEnumerator<BigInteger>` (`<Fibonacci>d__0` dans ildasm).
2. Cette classe implémente une machine d'état. L'état se compose de la position actuelle dans la méthode et des valeurs des variables locales.
3. Le code le plus intéressant se trouve dans la méthode `bool IEnumerator.MoveNext()`. Fondamentalement, ce que fait `MoveNext()` :
- Restaure l'état actuel. Des variables comme `prev` et `current` deviennent des champs dans notre classe (`<current>5__2` et `<prev>5__1` dans ildasm). Dans notre méthode, nous avons deux positions (`<>1__state`): la première à l'accolade ouvrante, la seconde à `yield return`.
- Exécute le code jusqu'au prochain `yield return` ou `yield break`/`}`.
- Pour `yield return` la valeur résultante est enregistrée, donc la propriété `Current` peut la renvoyer. "vrai" est renvoyé. À ce stade, l'état actuel est à nouveau enregistré pour la prochaine invocation de "MoveNext".
- Pour la méthode `yield break`/`}` renvoie simplement `false`, ce qui signifie que l'itération est terminée.

Notez également que le 10001e numéro fait 468 octets. La machine d'état enregistre uniquement les variables "current" et "prev" en tant que champs. Alors que si nous souhaitons enregistrer tous les nombres dans la séquence du premier au 10000e, la taille de la mémoire consommée sera supérieure à 4 mégaoctets. Ainsi, l'évaluation paresseuse, si elle est correctement utilisée, peut réduire l'empreinte mémoire dans certains cas.

## La différence entre break et yield break
Utiliser `yield break` plutôt que `break` n'est peut-être pas aussi évident qu'on pourrait le penser. Il existe de nombreux mauvais exemples sur Internet où l'utilisation des deux est interchangeable et ne démontre pas vraiment la différence.

La partie déroutante est que les deux mots-clés (ou phrases clés) n'ont de sens que dans des boucles (`foreach`, `while` ...) Alors, quand choisir l'un plutôt que l'autre ?

Il est important de réaliser qu'une fois que vous utilisez le mot-clé [`yield`](https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx) dans une méthode, vous transformez effectivement la méthode en un [itérateur]( https://msdn.microsoft.com/en-us/library/mt639331.aspx). Le seul but d'une telle méthode est alors d'itérer sur une collection finie ou infinie et de produire (sortie) ses éléments. Une fois l'objectif atteint, il n'y a aucune raison de poursuivre l'exécution de la méthode. Parfois, cela se produit naturellement avec la dernière parenthèse fermante de la méthode `}`. Mais parfois, vous souhaitez mettre fin à la méthode prématurément. Dans une méthode normale (sans itération), vous utiliseriez le mot clé [`return`](https://msdn.microsoft.com/en-us/library/1h3swy84.aspx). Mais vous ne pouvez pas utiliser `return` dans un itérateur, vous devez utiliser `yield break`. En d'autres termes, `yield break` pour un itérateur est le même que `return` pour une méthode standard. Alors que l'instruction [`break`](https://msdn.microsoft.com/en-us/library/adbctzc4.aspx) termine simplement la boucle la plus proche.

Voyons quelques exemples :

```
    /// <summary>
    /// Yields numbers from 0 to 9
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9}</returns>
    public static IEnumerable<int> YieldBreak()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Indicates that the iteration has ended, everything 
                // from this line on will be ignored
                yield break;
            }
        }
        yield return 10; // This will never get executed
    }
```

```
    /// <summary>
    /// Yields numbers from 0 to 10
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9,10}</returns>
    public static IEnumerable<int> Break()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Terminates just the loop
                break;
            }
        }
        // Execution continues
        yield return 10;
    }
```

