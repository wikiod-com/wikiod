---
title: "Requêtes LINQ"
slug: "requetes-linq"
draft: false
images: []
weight: 6793
type: docs
toc: true
---

LINQ est un acronyme qui signifie **L**anguage **IN**tegrated **Q**uery. Il s'agit d'un concept qui intègre un langage de requête en offrant un modèle cohérent pour travailler avec des données sur différents types de sources et de formats de données ; vous utilisez les mêmes modèles de codage de base pour interroger et transformer des données dans des documents XML, des bases de données SQL, des ensembles de données ADO.NET, des collections .NET et tout autre format pour lequel un fournisseur LINQ est disponible.

## Syntaxe
- Syntaxe de requête :
    - from \<range variable\> in \<collection\>
    - [from \<range variable\> in \<collection\>, ...]
    - \<filter, joining, grouping, aggregate operators, ...\> \<lambda expression\>
    - \<select or groupBy operator\> \<formulate the result\>

- Syntaxe de la méthode :

    - Enumerable.Aggregate(func)
    - Enumerable.Aggregate(seed, func)
    - Enumerable.Aggregate(seed, func, resultSelector)
    - Enumerable.All(predicate)
    - Enumerable.Any()
    - Enumerable.Any(predicate)
    - Enumerable.AsEnumerable()
    - Enumerable.Average()
    - Enumerable.Average(selector)
    - Enumerable.Cast\<Result\>()
    - Enumerable.Concat(second)
    - Enumerable.Contains(value)
    - Enumerable.Contains(value, comparer)
    - Enumerable.Count()
    - Enumerable.Count(predicate)
    - Enumerable.DefaultIfEmpty()
    - Enumerable.DefaultIfEmpty(defaultValue)
    - Enumerable.Distinct()
    - Enumerable.Distinct(comparer)
    - Enumerable.ElementAt(index)
    - Enumerable.ElementAtOrDefault(index)
    - Enumerable.Empty()
    - Enumerable.Except(second)
    - Enumerable.Except(second, comparer)
    - Enumerable.First()
    - Enumerable.First(predicate)
    - Enumerable.FirstOrDefault()
    - Enumerable.FirstOrDefault(predicate)
    - Enumerable.GroupBy(keySelector)
    - Enumerable.GroupBy(keySelector, resultSelector)
    - Enumerable.GroupBy(keySelector, elementSelector)
    - Enumerable.GroupBy(keySelector, comparer)
    - Enumerable.GroupBy(keySelector, resultSelector, comparer)
    - Enumerable.GroupBy(keySelector, elementSelector, resultSelector)
    - Enumerable.GroupBy(keySelector, elementSelector, comparer)
    - Enumerable.GroupBy(keySelector, elementSelector, resultSelector, comparer)
    - Enumerable.Intersect(second)
    - Enumerable.Intersect(second, comparer)
    - Enumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector)
    - Enumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector, comparer)
    - Enumerable.Last()
    - Enumerable.Last(predicate)
    - Enumerable.LastOrDefault()
    - Enumerable.LastOrDefault(predicate)
    - Enumerable.LongCount()
    - Enumerable.LongCount(predicate)
    - Enumerable.Max()
    - Enumerable.Max(selector)
    - Enumerable.Min()
    - Enumerable.Min(selector)
    - Enumerable.OfType\<TResult\>()
    - Enumerable.OrderBy(keySelector)
    - Enumerable.OrderBy(keySelector, comparer)
    - Enumerable.OrderByDescending(keySelector)
    - Enumerable.OrderByDescending(keySelector, comparer)
    - Enumerable.Range(start, count)
    - Enumerable.Repeat(element, count)
    - Enumerable.Reverse()
    - Enumerable.Select(selector)
    - Enumerable.SelectMany(selector)
    - Enumerable.SelectMany(collectionSelector, resultSelector)
    - Enumerable.SequenceEqual(second)
    - Enumerable.SequenceEqual(second, comparer)
    - Enumerable.Single()
    - Enumerable.Single(predicate)
    - Enumerable.SingleOrDefault()
    - Enumerable.SingleOrDefault(predicate)
    - Enumerable.Skip(count)
    - Enumerable.SkipWhile(predicate)
    - Enumerable.Sum()
    - Enumerable.Sum(selector)
    - Enumerable.Take(count)
    - Enumerable.TakeWhile(predicate)
    - orderedEnumerable.ThenBy(keySelector)
    - orderedEnumerable.ThenBy(keySelector, comparer)
    - orderedEnumerable.ThenByDescending(keySelector)
    - orderedEnumerable.ThenByDescending(keySelector, comparer)
    - Enumerable.ToArray()
    - Enumerable.ToDictionary(keySelector)
    - Enumerable.ToDictionary(keySelector, elementSelector)
    - Enumerable.ToDictionary(keySelector, comparer)
    - Enumerable.ToDictionary(keySelector, elementSelector, comparer)
    - Enumerable.ToList()
    - Enumerable.ToLookup(keySelector)
    - Enumerable.ToLookup(keySelector, elementSelector)
    - Enumerable.ToLookup(keySelector, comparer)
    - Enumerable.ToLookup(keySelector, elementSelector, comparer)
    - Enumerable.Union(second)
    - Enumerable.Union(second, comparer)
    - Enumerable.Where(predicate)
    - Enumerable.Zip(second, resultSelector)

Pour utiliser les requêtes LINQ, vous devez importer `System.Linq`.

La syntaxe de méthode est plus puissante et flexible, mais la syntaxe de requête peut être plus simple et plus familière. Toutes les requêtes écrites dans la syntaxe Query sont traduites dans la syntaxe fonctionnelle par le compilateur, de sorte que les performances sont les mêmes.

Les objets de requête ne sont pas évalués tant qu'ils ne sont pas utilisés, ils peuvent donc être modifiés ou ajoutés sans pénaliser les performances.

## Méthodes de chaînage
[De nombreuses fonctions LINQ][1] fonctionnent toutes deux sur un `IEnumerable<TSource>` et renvoient également un `IEnumerable<TResult>`. Les paramètres de type `TSource` et `TResult` peuvent ou non faire référence au même type, selon la méthode en question et les fonctions qui lui sont transmises.

Quelques exemples en sont

    public static IEnumerable<TResult> Select<TSource, TResult>(
        this IEnumerable<TSource> source,
        Func<TSource, TResult> selector
    )

    public static IEnumerable<TSource> Where<TSource>(
        this IEnumerable<TSource> source,
        Func<TSource, int, bool> predicate
    )

    public static IOrderedEnumerable<TSource> OrderBy<TSource, TKey>(
        this IEnumerable<TSource> source,
        Func<TSource, TKey> keySelector
    )

Alors que certains chaînages de méthodes peuvent nécessiter un ensemble complet avant de passer à autre chose, LINQ tire parti de [l'exécution différée] (https://www.wikiod.com/fr/docs/c%23/68/linq-queries/8001/deferred- exécution) en utilisant [yield return <sup>**MSDN**</sup>](https://blogs.msdn.microsoft.com/oldnewthing/20080812-00/?p=21273/) qui crée un Enumerable et un recenseur dans les coulisses. Le processus de chaînage dans LINQ consiste essentiellement à construire un énumérable (itérateur) pour l'ensemble d'origine - qui est différé - jusqu'à ce qu'il soit matérialisé par [enumérant l'énumérable] (https://www.wikiod.com/fr/docs/c%23/68/ linq-queries/17356/enumerating-the-enumerable).

Cela permet à ces fonctions d'être [fluently chained <sup>**wiki**</sup>](https://en.wikipedia.org/wiki/Fluent_interface), où une fonction peut agir directement sur le résultat d'une autre. Ce style de code peut être utilisé pour effectuer de nombreuses opérations basées sur des séquences dans une seule instruction.

Par exemple, il est possible de combiner `Select`, `Where` et `OrderBy` pour transformer, filtrer et trier une séquence en une seule instruction.

    var someNumbers = { 4, 3, 2, 1 };

    var processed = someNumbers
            .Select(n => n * 2)   // Multiply each number by 2
            .Where(n => n != 6)   // Keep all the results, except for 6
            .OrderBy(n => n);     // Sort in ascending order

**Production:**
>2
>4
>8

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/3Gta8X)

Toutes les fonctions qui étendent et renvoient à la fois le type générique `IEnumerable<T>` peuvent être utilisées comme clauses chaînées dans une seule instruction. Ce style de programmation fluide est puissant et doit être pris en compte lors de la création de vos propres [méthodes d'extension][2].


[1] : https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx
[2] : https://www.wikiod.com/fr/docs/c%23/20/extension-methods#t=201607220826369208865

## First, FirstOrDefault, Last, LastOrDefault, Single et SingleOrDefault
Les six méthodes renvoient une valeur unique du type séquence et peuvent être appelées avec ou sans prédicat.

En fonction du nombre d'éléments qui correspondent au "prédicat" ou, si aucun "prédicat" n'est fourni, du nombre d'éléments dans la séquence source, ils se comportent comme suit :

# Première()

* Renvoie le premier élément d'une séquence, ou le premier élément correspondant au "prédicat" fourni.
* Si la séquence ne contient aucun élément, une `InvalidOperationException` est levée avec le message : "La séquence ne contient aucun élément".
* Si la séquence ne contient aucun élément correspondant au `prédicat` fourni, une `InvalidOperationException` est levée avec le message "La séquence ne contient aucun élément correspondant".

**Exemple**

    // Returns "a":
    new[] { "a" }.First();
    
    // Returns "a":
    new[] { "a", "b" }.First();
    
    // Returns "b":
    new[] { "a", "b" }.First(x => x.Equals("b"));
    
    // Returns "ba":
    new[] { "ba", "be" }.First(x => x.Contains("b"));
    
    // Throws InvalidOperationException:
    new[] { "ca", "ce" }.First(x => x.Contains("b"));
    
    // Throws InvalidOperationException:
    new string[0].First();

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/ESYLcU)


# PremierOuDéfaut()

* Renvoie le premier élément d'une séquence, ou le premier élément correspondant au "prédicat" fourni.
* Si la séquence ne contient aucun élément, ou aucun élément correspondant au `prédicat` fourni, renvoie la valeur par défaut du type de séquence en utilisant [`default(T)`](https://www.wikiod.com/fr/docs/c%23/ 26/keywords/109/default#t=201702071640321629621).

**Exemple**

    // Returns "a":
    new[] { "a" }.FirstOrDefault();
    
    // Returns "a":
    new[] { "a", "b" }.FirstOrDefault();
    
    // Returns "b":
    new[] { "a", "b" }.FirstOrDefault(x => x.Equals("b"));
    
    // Returns "ba":
    new[] { "ba", "be" }.FirstOrDefault(x => x.Contains("b"));
    
    // Returns null:
    new[] { "ca", "ce" }.FirstOrDefault(x => x.Contains("b"));
    
    // Returns null:
    new string[0].FirstOrDefault();

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/XJ93lr)


# Dernier()

* Renvoie le dernier élément d'une séquence, ou le dernier élément correspondant au "prédicat" fourni.
* Si la séquence ne contient aucun élément, une `InvalidOperationException` est levée avec le message "La séquence ne contient aucun élément."
* Si la séquence ne contient aucun élément correspondant au `prédicat` fourni, une `InvalidOperationException` est levée avec le message "La séquence ne contient aucun élément correspondant".

**Exemple**

    // Returns "a":
    new[] { "a" }.Last();
    
    // Returns "b":
    new[] { "a", "b" }.Last();
    
    // Returns "a":
    new[] { "a", "b" }.Last(x => x.Equals("a"));
    
    // Returns "be":
    new[] { "ba", "be" }.Last(x => x.Contains("b"));
    
    // Throws InvalidOperationException:
    new[] { "ca", "ce" }.Last(x => x.Contains("b"));

    // Throws InvalidOperationException:
    new string[0].Last(); 


# DernierOuDéfaut()

* Renvoie le dernier élément d'une séquence, ou le dernier élément correspondant au "prédicat" fourni.
* Si la séquence ne contient aucun élément, ou aucun élément correspondant au `prédicat` fourni, renvoie la valeur par défaut du type de séquence en utilisant `default(T)`.

**Exemple**

    // Returns "a":
    new[] { "a" }.LastOrDefault();
    
    // Returns "b":
    new[] { "a", "b" }.LastOrDefault();
    
    // Returns "a":
    new[] { "a", "b" }.LastOrDefault(x => x.Equals("a"));
    
     // Returns "be":
    new[] { "ba", "be" }.LastOrDefault(x => x.Contains("b"));
    
    // Returns null:
    new[] { "ca", "ce" }.LastOrDefault(x => x.Contains("b")); 
    
    // Returns null:
    new string[0].LastOrDefault();


# Seul()

* Si la séquence contient exactement un élément, ou exactement un élément correspondant au "prédicat" fourni, cet élément est renvoyé.
* Si la séquence ne contient aucun élément, ou aucun élément correspondant au `prédicat` fourni, une `InvalidOperationException` est levée avec le message "La séquence ne contient aucun élément".
* Si la séquence contient plus d'un élément, ou plus d'un élément correspondant au `prédicat` fourni, une `InvalidOperationException` est levée avec le message "La séquence contient plus d'un élément".
* __Remarque :__ afin d'évaluer si la séquence contient exactement un élément, au plus deux éléments doivent être énumérés.

**Exemple**

    // Returns "a":
    new[] { "a" }.Single();
    
    // Throws InvalidOperationException because sequence contains more than one element:
    new[] { "a", "b" }.Single();
    
    // Returns "b":
    new[] { "a", "b" }.Single(x => x.Equals("b"));
    
    // Throws InvalidOperationException:
    new[] { "a", "b" }.Single(x => x.Equals("c"));
    
    // Throws InvalidOperationException:
    new string[0].Single(); 
    
    // Throws InvalidOperationException because sequence contains more than one element:
    new[] { "a", "a" }.Single();


# SingleOrDefault()
* Si la séquence contient exactement un élément, ou exactement un élément correspondant au "prédicat" fourni, cet élément est renvoyé.
* Si la séquence ne contient aucun élément, ou aucun élément correspondant au `prédicat` fourni, `default(T)` est renvoyé.
* Si la séquence contient plus d'un élément, ou plus d'un élément correspondant au `prédicat` fourni, une `InvalidOperationException` est levée avec le message "La séquence contient plus d'un élément".
* Si la séquence ne contient aucun élément correspondant au `prédicat` fourni, renvoie la valeur par défaut du type de séquence en utilisant `default(T)`.
* __Remarque :__ afin d'évaluer si la séquence contient exactement un élément, au plus deux éléments doivent être énumérés.

**Exemple**

    // Returns "a":
    new[] { "a" }.SingleOrDefault();

    // returns "a"
    new[] { "a", "b" }.SingleOrDefault(x => x == "a"); 

    // Returns null:
    new[] { "a", "b" }.SingleOrDefault(x => x == "c");

    // Throws InvalidOperationException:
    new[] { "a", "a" }.SingleOrDefault(x => x == "a");

    // Throws InvalidOperationException:
    new[] { "a", "b" }.SingleOrDefault();

    // Returns null:
    new string[0].SingleOrDefault();

# Recommandations

- Bien que vous puissiez utiliser `FirstOrDefault`, `LastOrDefault` ou `SingleOrDefault` pour vérifier si une séquence contient des éléments, `Any` ou `Count` sont plus fiables. En effet, une valeur de retour de `default(T)` de l'une de ces trois méthodes ne prouve pas que la séquence est vide, car la valeur du premier / dernier / élément unique de la séquence pourrait également être `default(T )`

- Décidez quelles méthodes correspondent le mieux à l'objectif de votre code. Par exemple, n'utilisez `Single` que si vous devez vous assurer qu'un seul élément de la collection correspond à votre prédicat &mdash; sinon, utilisez `Premier` ; as `Single` lève une exception si la séquence a plus d'un élément correspondant. Ceci s'applique bien sûr également aux contreparties "*OrDefault".

- En ce qui concerne l'efficacité : bien qu'il soit souvent approprié de s'assurer qu'il n'y a qu'un seul élément (`Single`) ou qu'un seul ou aucun élément (`SingleOrDefault`) est renvoyé par une requête, ces deux méthodes nécessitent plus, et souvent le l'intégralité de la collection à examiner pour s'assurer qu'il n'y a pas de deuxième correspondance avec la requête. Ceci est différent du comportement, par exemple, de la méthode `First`, qui peut être satisfaite après avoir trouvé la première correspondance.

## À l'exception
La méthode Except renvoie l'ensemble des éléments contenus dans la première collection mais non contenus dans la seconde. La valeur par défaut [`IEqualityComparer`][1] est utilisée pour comparer les éléments des deux ensembles. Il existe une surcharge qui accepte un [`IEqualityComparer`][1] comme argument.

**Exemple:**

    int[] first = { 1, 2, 3, 4 };
    int[] second = { 0, 2, 3, 5 };
    
    IEnumerable<int> inFirstButNotInSecond = first.Except(second);
    // inFirstButNotInSecond = { 1, 4 }

**Production:**
>1
>4
 
[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/m3EqTQ)

Dans ce cas, `.Except(second)` exclut les éléments contenus dans le tableau `second`, à savoir 2 et 3 (0 et 5 ne sont pas contenus dans le tableau `first` et sont ignorés).

Notez que "Sauf" implique "Distinct" (c'est-à-dire qu'il supprime les éléments répétés). Par exemple:
    
    int[] third = { 1, 1, 1, 2, 3, 4 };
    
    IEnumerable<int> inThirdButNotInSecond = third.Except(second);
    // inThirdButNotInSecond = { 1, 4 }

**Production:**
>1
>4

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/VlXBUp)

Dans ce cas, les éléments 1 et 4 ne sont retournés qu'une seule fois.

---

Implémenter [`IEquatable`][2] ou fournir la fonction an [`IEqualityComparer`][1] permettra d'utiliser une méthode différente pour comparer les éléments.
Notez que la méthode [`GetHashCode`][3] doit également être remplacée afin qu'elle renvoie un code de hachage identique pour `object` qui sont identiques selon l'implémentation [`IEquatable`][2].

***Exemple avec IEquatable :***

    class Holiday : IEquatable<Holiday>
    {
        public string Name { get; set; }
    
        public bool Equals(Holiday other)
        {
            return Name == other.Name;
        }
    
        // GetHashCode must return true whenever Equals returns true.
        public override int GetHashCode()
        {
            //Get hash code for the Name field if it is not null.
            return Name?.GetHashCode() ?? 0;
        }
    }
    
    public class Program
    {
        public static void Main()
        {
            List<Holiday> holidayDifference = new List<Holiday>();

            List<Holiday> remoteHolidays = new List<Holiday>
            {
                new Holiday { Name = "Xmas" },
                new Holiday { Name = "Hanukkah" },
                new Holiday { Name = "Ramadan" }
            };

            List<Holiday> localHolidays = new List<Holiday>
            {
                new Holiday { Name = "Xmas" },
                new Holiday { Name = "Ramadan" }
            };

            holidayDifference = remoteHolidays
                .Except(localHolidays)
                .ToList();

            holidayDifference.ForEach(x => Console.WriteLine(x.Name));
        }
    }

Production:

> Hanoucca

[Démo en direct sur .NET Fiddle][4]


[1] : https://msdn.microsoft.com/en-us/library/ms132151(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx
[3] : https://msdn.microsoft.com/en-us/library/system.object.gethashcode(v=vs.110).aspx
[4] : https://dotnetfiddle.net/9ilGqy

## SélectionnerPlusieurs
La méthode SelectMany linq "aplatit" un `IEnumerable<IEnumerable<T>>` en un `IEnumerable<T>`. Tous les éléments T des instances `IEnumerable` contenus dans la source `IEnumerable` seront combinés en un seul `IEnumerable`.

    var words = new [] { "a,b,c", "d,e", "f" };
    var splitAndCombine = words.SelectMany(x => x.Split(','));
    // returns { "a", "b", "c", "d", "e", "f" }

Si vous utilisez une fonction de sélection qui transforme les éléments d'entrée en séquences, le résultat sera les éléments de ces séquences renvoyés un par un.

Notez que, contrairement à `Select()`, le nombre d'éléments dans la sortie n'a pas besoin d'être le même que dans l'entrée.

** Plus d'exemples concrets **

    class School
    {
        public Student[] Students { get; set; }
    }
    
    class Student 
    {
        public string Name { get; set; }
    }    
      
    var schools = new [] {
        new School(){ Students = new [] { new Student { Name="Bob"}, new Student { Name="Jack"} }},
        new School(){ Students = new [] { new Student { Name="Jim"}, new Student { Name="John"} }}
    };
                   
    var allStudents = schools.SelectMany(s=> s.Students);
                 
    foreach(var student in allStudents)
    {
        Console.WriteLine(student.Name);
    }

Production:

> Bob
>Cric
> Jim
    John

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/LNyymI)

## N'importe quel
`Any` est utilisé pour vérifier si **un** élément d'une collection correspond à une condition ou non.
<br/>*voir aussi : [.All](https://www.wikiod.com/fr/docs/c%23/68/linq-queries/2773/all#t=201707041340119289445), [Any et FirstOrDefault : meilleure pratique] (https://www.wikiod.com/fr/docs/c%23/68/linq-queries/16731/any-and-firstordefault-best-practice#t=201707041441456087738)*

## 1. Paramètre vide ##
**Any** : renvoie "true" si la collection contient des éléments et "false" si la collection est vide :

    var numbers = new List<int>();
    bool result = numbers.Any(); // false

    var numbers = new List<int>(){ 1, 2, 3, 4, 5};
    bool result = numbers.Any(); //true

## 2. Expression lambda comme paramètre ##
**Any** : renvoie "true" si la collection contient un ou plusieurs éléments qui remplissent la condition dans l'expression lambda :

    var arrayOfStrings = new string[] { "a", "b", "c" };
    arrayOfStrings.Any(item => item == "a");    // true
    arrayOfStrings.Any(item => item == "d");    // false
    
## 3. Vider la collection ##
**Any** : renvoie "false" si la collection est vide et qu'une expression lambda est fournie :
  
    var numbers = new List<int>();
    bool result = numbers.Any(i => i >= 0); // false

**Noter:**
`Any` arrêtera l'itération de la collection dès qu'il trouvera un élément correspondant à la condition. Cela signifie que la collection ne sera pas nécessairement entièrement dénombrée ; il ne sera énuméré que suffisamment loin pour trouver le premier élément correspondant à la condition.

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/IQ4wG4)


## JOINT
Les jointures sont utilisées pour combiner différentes listes ou tables contenant des données via une clé commune.

Comme dans SQL, les types de jointures suivants sont pris en charge dans LINQ : <br/>
** Jointures intérieures, gauches, droites, croisées ** et ** extérieures complètes **.

Les deux listes suivantes sont utilisées dans les exemples ci-dessous :

    var first = new List<string>(){ "a","b","c"}; // Left data
    var second = new List<string>(){ "a", "c", "d"}; // Right data

## (Jointure interne

    var result = from f in first
                 join s in second on f equals s
                 select new { f, s };

    var result = first.Join(second, 
                            f => f, 
                            s => s,
                            (f, s) => new { f, s });

    // Result: {"a","a"}
    //         {"c","c"}

## Jointure externe gauche
    var leftOuterJoin = from f in first
                        join s in second on f equals s into temp
                        from t in temp.DefaultIfEmpty()
                        select new { First = f, Second = t};

    // Or can also do:
    var leftOuterJoin = from f in first
                        from s in second.Where(x => x == f).DefaultIfEmpty()
                        select new { First = f, Second = s};

    // Result: {"a","a"}
    //         {"b", null}  
    //         {"c","c"}  


    // Left outer join method syntax
    var leftOuterJoinFluentSyntax = first.GroupJoin(second,
                                          f => f,
                                          s => s,
                                          (f, s) => new { First = f, Second = s })
                                       .SelectMany(temp => temp.Second.DefaultIfEmpty(),
                                          (f, s) => new { First = f.First, Second = s });




## Jointure externe droite
    var rightOuterJoin = from s in second
                         join f in first on s equals f into temp
                         from t in temp.DefaultIfEmpty()
                         select new {First=t,Second=s};

    // Result: {"a","a"}
    //         {"c","c"}  
    //         {null,"d"}  


## Jointure croisée

    var CrossJoin = from f in first
                    from s in second
                    select new { f, s };

    // Result: {"a","a"}
    //         {"a","c"}  
    //         {"a","d"}  
    //         {"b","a"}
    //         {"b","c"}  
    //         {"b","d"}  
    //         {"c","a"}
    //         {"c","c"}  
    //         {"c","d"}

## Jointure externe complète

    var fullOuterjoin = leftOuterJoin.Union(rightOuterJoin);

    // Result: {"a","a"}
    //         {"b", null}  
    //         {"c","c"}  
    //         {null,"d"}

## **Exemple pratique** ##
Les exemples ci-dessus ont une structure de données simple afin que vous puissiez vous concentrer sur la compréhension technique des différentes jointures LINQ, mais dans le monde réel, vous auriez des tables avec des colonnes que vous devez joindre.

Dans l'exemple suivant, il n'y a qu'une seule classe `Region` utilisée, en réalité vous joindrez deux ou plusieurs tables différentes qui détiennent la même clé (dans cet exemple `first` et `second` sont joints via la clé commune `ID` ).

**Exemple :** Considérons la structure de données suivante :

    public class Region 
    {
        public Int32 ID;
        public string RegionDescription;
        
        public Region(Int32 pRegionID, string pRegionDescription=null)
        {
            ID = pRegionID; RegionDescription = pRegionDescription;
        }
    }

Préparez maintenant les données (c'est-à-dire remplissez-les avec des données) :

    // Left data
    var first = new List<Region>() 
                     { new Region(1), new Region(3), new Region(4) }; 
    // Right data
    var second = new List<Region>() 
                     { 
                        new Region(1, "Eastern"),  new Region(2, "Western"),
                        new Region(3, "Northern"), new Region(4, "Southern")
                     }; 

Vous pouvez voir que dans cet exemple `first` ne contient aucune description de région, vous souhaitez donc les joindre à partir de `second`. Ensuite, la jointure interne ressemblerait à :

    // do the inner join
    var result = from f in first
                 join s in second on f.ID equals s.ID
                 select new { f.ID, s.RegionDescription };
 

     // Result: {1,"Eastern"}
     //         {3, Northern}  
     //         {4,"Southern"}  

Ce résultat a créé des objets anonymes à la volée, ce qui est bien, mais nous avons déjà créé une classe appropriée - nous pouvons donc la spécifier : au lieu de `select new { f.ID, s.RegionDescription } ;` nous pouvons dire `select new Region(f.ID, s.RegionDescription);`, qui retournera les mêmes données mais créera des objets de type `Region` - qui maintiendront la compatibilité avec les autres objets.



[Démo en direct sur le violon .NET] (https://dotnetfiddle.net/pP6enP)


## Sauter et prendre
La méthode Skip renvoie une collection excluant un certain nombre d'éléments depuis le début de la collection source. Le nombre d'éléments exclus est le nombre donné en argument. S'il y a moins d'éléments dans la collection que spécifié dans l'argument, une collection vide est renvoyée.

La méthode Take renvoie une collection contenant un certain nombre d'éléments depuis le début de la collection source. Le nombre d'éléments inclus est le nombre donné en argument. S'il y a moins d'éléments dans la collection que spécifié dans l'argument, la collection renvoyée contiendra les mêmes éléments que la collection source.

    var values = new [] { 5, 4, 3, 2, 1 };

    var skipTwo        = values.Skip(2);         // { 3, 2, 1 }
    var takeThree      = values.Take(3);         // { 5, 4, 3 }
    var skipOneTakeTwo = values.Skip(1).Take(2); // { 4, 3 }
    var takeZero       = values.Take(0);         // An IEnumerable<int> with 0 items

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/U2b76y)

**Skip and Take** sont couramment utilisés ensemble pour paginer les résultats, par exemple :

    IEnumerable<T> GetPage<T>(IEnumerable<T> collection, int pageNumber, int resultsPerPage) {
        int startIndex = (pageNumber - 1) * resultsPerPage;
        return collection.Skip(startIndex).Take(resultsPerPage);
    }


> **Avertissement :** LINQ to Entities prend uniquement en charge Ignorer les [requêtes ordonnées][1]. Si vous essayez d'utiliser Skip sans commander, vous obtiendrez une **NotSupportedException** avec le message "La méthode 'Skip' n'est prise en charge que pour les entrées triées dans LINQ to Entities. La méthode 'OrderBy' doit être appelée avant la méthode 'Skip '."


[1] : https://www.wikiod.com/fr/docs/c%23/68/linq-queries/4389/query-ordering#t=201607261110520529272

## Définition d'une variable dans une requête Linq (let mot-clé)
Afin de définir une variable dans une expression linq, vous pouvez utiliser le mot-clé **let**. Ceci est généralement fait afin de stocker les résultats de sous-requêtes intermédiaires, par exemple :

     int[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

     var aboveAverages = from number in numbers
                         let average = numbers.Average()
                         let nSquared = Math.Pow(number,2)
                         where nSquared > average
                         select number;

     Console.WriteLine("The average of the numbers is {0}.", numbers.Average());

     foreach (int n in aboveAverages)
     {
       Console.WriteLine("Query result includes number {0} with square of {1}.", n, Math.Pow(n,2));
     }

**Production:**

>La moyenne des nombres est de 4,5.
> Le résultat de la requête inclut le nombre 3 avec un carré de 9.
> Le résultat de la requête inclut le numéro 4 avec un carré de 16.
>Le résultat de la requête inclut le nombre 5 avec un carré de 25.
> Le résultat de la requête inclut le nombre 6 avec un carré de 36.
> Le résultat de la requête inclut le nombre 7 avec un carré de 49.
>Le résultat de la requête inclut le nombre 8 avec un carré de 64.
> Le résultat de la requête inclut le nombre 9 avec un carré de 81.

[Voir la démo][1]


[1] : https://dotnetfiddle.net/zbjrHZ

## Zipper
La méthode d'extension `Zip` agit sur deux collections. Il associe chaque élément des deux séries en fonction de la position. Avec une instance `Func`, nous utilisons `Zip` pour gérer les éléments des deux collections C # par paires. Si les séries diffèrent en taille, les éléments supplémentaires de la plus grande série seront ignorés.

Pour prendre un exemple du livre "C# in a Nutshell",

    int[] numbers = { 3, 5, 7 };
    string[] words = { "three", "five", "seven", "ignored" };
    IEnumerable<string> zip = numbers.Zip(words, (n, w) => n + "=" + w);

**Production:**

>3=trois
>5=cinq
>7=sept

[Voir la démo][1]


[1] : https://dotnetfiddle.net/nIA5E9

## Plage et répétition
Les méthodes statiques `Range` et `Repeat` sur `Enumerable` peuvent être utilisées pour générer des séquences simples.

**Intervalle**
---------

`Enumerable.Range()` génère une séquence d'entiers avec une valeur de départ et un nombre.

    // Generate a collection containing the numbers 1-100 ([1, 2, 3, ..., 98, 99, 100])
    var range = Enumerable.Range(1,100);

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/jA0VB1)

**Répéter**
------

`Enumerable.Repeat()` génère une séquence d'éléments répétitifs en fonction d'un élément et du nombre de répétitions requises.

    // Generate a collection containing "a", three times (["a","a","a"])
    var repeatedValues = Enumerable.Repeat("a", 3);

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/KpZfpt)

## Tout
`All` est utilisé pour vérifier si tous les éléments d'une collection correspondent à une condition ou non.
<br/>*voir aussi : [.Any](https://www.wikiod.com/fr/docs/c%23/68/linq-queries/5098/any#t=201707041342119744775)*
## 1. Paramètre vide ##

**All** : ne peut pas être utilisé avec un paramètre vide.

## 2. Expression lambda comme paramètre ##

**All** : renvoie "true" si tous les éléments de la collection satisfont l'expression lambda et "false" dans le cas contraire :

    var numbers = new List<int>(){ 1, 2, 3, 4, 5};
    bool result = numbers.All(i => i < 10); // true
    bool result = numbers.All(i => i >= 3); // false

## 3. Vider la collection ##

**All** : renvoie "true" si la collection est vide et qu'une expression lambda est fournie :

    var numbers = new List<int>();
    bool result = numbers.All(i => i >= 0); // true

**Noter:**
`All` arrêtera l'itération de la collection dès qu'il trouvera un élément **ne correspondant pas** à la condition. Cela signifie que la collection ne sera pas nécessairement entièrement dénombrée ; il ne sera énuméré que suffisamment loin pour trouver le premier élément ** ne correspondant pas ** à la condition.


## Bases
LINQ est largement bénéfique pour interroger des collections (ou des tableaux).

Par exemple, étant donné les exemples de données suivants :

    var classroom = new Classroom
    {
        new Student { Name = "Alice", Grade = 97, HasSnack = true  },
        new Student { Name = "Bob",   Grade = 82, HasSnack = false },
        new Student { Name = "Jimmy", Grade = 71, HasSnack = true  },
        new Student { Name = "Greg",  Grade = 90, HasSnack = false },
        new Student { Name = "Joe",   Grade = 59, HasSnack = false }
    }

Nous pouvons "interroger" sur ces données en utilisant la syntaxe LINQ. Par exemple, pour récupérer tous les élèves qui ont un goûter aujourd'hui :

    var studentsWithSnacks = from s in classroom.Students
                             where s.HasSnack
                             select s;

Ou, pour récupérer les étudiants avec une note de 90 ou plus, et ne renvoyer que leurs noms, pas l'objet `Student` complet :

    var topStudentNames = from s in classroom.Students
                          where s.Grade >= 90
                          select s.Name;

La fonctionnalité LINQ est composée de deux syntaxes qui exécutent les mêmes fonctions, ont des performances presque identiques, mais sont écrites très différemment. La syntaxe de l'exemple ci-dessus est appelée **syntaxe de requête**. L'exemple suivant, cependant, illustre la **syntaxe de la méthode**. Les mêmes données seront renvoyées que dans l'exemple ci-dessus, mais la façon dont la requête est écrite est différente.

    var topStudentNames = classroom.Students
                                   .Where(s => s.Grade >= 90)
                                   .Select(s => s.Name);
                                        

## Agrégat
`Aggregate` Applique une fonction d'accumulateur sur une séquence.
 
    int[] intList = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    int sum = intList.Aggregate((prevSum, current) => prevSum + current);
    // sum = 55

- A la première étape `prevSum = 1`
- A la seconde `prevSum = prevSum(at
la première étape) + 2`
- A la ième étape `prevSum = prevSum(au (i-1)
étape) + i-ème élément du tableau`


    string[] stringList = { "Hello", "World", "!" };
    string joinedString = stringList.Aggregate((prev, current) => prev + " " + current);
    // joinedString = "Hello World !"


----------

Une deuxième surcharge de `Aggregate` reçoit également un paramètre `seed` qui est la valeur initiale de l'accumulateur. Cela peut être utilisé pour calculer plusieurs conditions sur une collection sans l'itérer plus d'une fois.

    List<int> items = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };

Pour la collection d'éléments, nous voulons calculer

1. Le total `.Count`
2. Le nombre de nombres pairs
3. Collectez chaque quatrième article

En utilisant `Aggregate`, cela peut être fait comme ceci :

    var result = items.Aggregate(new { Total = 0, Even = 0, FourthItems = new List<int>() },
                    (accumelative,item) =>
                    new {
                        Total = accumelative.Total + 1,
                        Even = accumelative.Even + (item % 2 == 0 ? 1 : 0),
                        FourthItems = (accumelative.Total + 1)%4 == 0 ? 
                            new List<int>(accumelative.FourthItems) { item } : 
                            accumelative.FourthItems 
                    });
    // Result:
    // Total = 12
    // Even = 6
    // FourthItems = [4, 8, 12]

_Notez que l'utilisation d'un type anonyme comme graine doit instancier un nouvel objet pour chaque élément car les propriétés sont en lecture seule. En utilisant une classe personnalisée, on peut simplement attribuer les informations et aucun "nouveau" n'est nécessaire (uniquement lors de l'attribution du paramètre initial "seed"_

## SelectMany : Aplatir une séquence de séquences
    var sequenceOfSequences = new [] { new [] { 1, 2, 3 }, new [] { 4, 5 }, new [] { 6 } };
    var sequence = sequenceOfSequences.SelectMany(x => x);
    // returns { 1, 2, 3, 4, 5, 6 }

Utilisez `SelectMany ()` si vous avez, ou si vous créez une séquence de séquences, mais que vous voulez que le résultat soit une longue séquence.

Dans la syntaxe de requête LINQ :

    var sequence = from subSequence in sequenceOfSequences
                   from item in subSequence
                   select item;

Si vous avez une collection de collections et souhaitez pouvoir travailler sur les données de la collection parent et enfant en même temps, c'est également possible avec `SelectMany`.

Définissons des classes simples

    public class BlogPost
    {
        public int Id { get; set; }
        public string Content { get; set; }
        public List<Comment> Comments { get; set; }
    }

    public class Comment
    {
        public int Id { get; set; }
        public string Content { get; set; }
    }

Supposons que nous ayons la collection suivante.

    List<BlogPost> posts = new List<BlogPost>()
    {
        new BlogPost()
        {
            Id = 1,
            Comments = new List<Comment>()
            {
                new Comment()
                {
                    Id = 1,
                    Content = "It's really great!",
                },
                new Comment()
                {
                    Id = 2,
                    Content = "Cool post!"
                }
            }
        },
        new BlogPost()
        {
            Id = 2,
            Comments = new List<Comment>()
            {
                new Comment()
                {
                    Id = 3,
                    Content = "I don't think you're right",
                },
                new Comment()
                {
                    Id = 4,
                    Content = "This post is a complete nonsense"
                }
            }
        }
    };

Maintenant, nous voulons sélectionner les commentaires `Content` avec `Id` de `BlogPost` associé à ce commentaire. Pour ce faire, nous pouvons utiliser la surcharge `SelectMany` appropriée.

    var commentsWithIds = posts.SelectMany(p => p.Comments, (post, comment) => new { PostId = post.Id, CommentContent = comment.Content });

Notre `commentsWithIds` ressemble à ceci

    {
        PostId = 1,
        CommentContent = "It's really great!"
    },
    {
        PostId = 1,
        CommentContent = "Cool post!"
    },
    {
        PostId = 2,
        CommentContent = "I don't think you're right"
    },
    {
        PostId = 2,
        CommentContent = "This post is a complete nonsense"
    }




## Distinct
Renvoie des valeurs uniques à partir d'un `IEnumerable`. L'unicité est déterminée à l'aide du comparateur d'égalité par défaut.

    int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };

    var distinct = array.Distinct();
    // distinct = { 1, 2, 3, 4, 5 }

Pour comparer un type de données personnalisé, nous devons implémenter l'interface `IEquatable<T>` et fournir les méthodes `GetHashCode` et `Equals` pour le type. Ou le comparateur d'égalité peut être remplacé :

    class SSNEqualityComparer : IEqualityComparer<Person> {
        public bool Equals(Person a, Person b) => return a.SSN == b.SSN;
        public int GetHashCode(Person p) => p.SSN;
    }

    List<Person> people;

    distinct = people.Distinct(SSNEqualityComparer);

## Collection de requêtes par type / éléments cast pour type
    interface IFoo { }
    class Foo : IFoo { }
    class Bar : IFoo { }

----------
    var item0 = new Foo();
    var item1 = new Foo();
    var item2 = new Bar();
    var item3 = new Bar();
    var collection = new IFoo[] { item0, item1, item2, item3 };

Utilisation de `OfType`

    var foos = collection.OfType<Foo>(); // result: IEnumerable<Foo> with item0 and item1
    var bars = collection.OfType<Bar>(); // result: IEnumerable<Bar> item item2 and item3
    var foosAndBars = collection.OfType<IFoo>(); // result: IEnumerable<IFoo> with all four items

Utilisation de "Où"

    var foos = collection.Where(item => item is Foo); // result: IEnumerable<IFoo> with item0 and item1
    var bars = collection.Where(item => item is Bar); // result: IEnumerable<IFoo> with item2 and item3

Utilisation de "Diffuser"

    var bars = collection.Cast<Bar>();                // throws InvalidCastException on the 1st item
    var foos = collection.Cast<Foo>();                // throws InvalidCastException on the 3rd item
    var foosAndBars = collection.Cast<IFoo>();        // OK 
    

## Par groupe
GroupBy est un moyen simple de trier une collection d'éléments `IEnumerable<T>` en groupes distincts.
## Exemple simple ##
Dans ce premier exemple, nous nous retrouvons avec deux groupes, pairs et impairs.

    List<int> iList = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    var grouped = iList.GroupBy(x => x % 2 == 0);

    //Groups iList into odd [13579] and even[2468] items 
           
    foreach(var group in grouped)
    {
        foreach (int item in group)
        {
            Console.Write(item); // 135792468  (first odd then even)
        }
    }

## Exemple plus complexe ##

Prenons l'exemple du regroupement d'une liste de personnes par âge.
Tout d'abord, nous allons créer un objet Person qui a deux propriétés, Name et Age.

    public class Person
    {
        public int Age {get; set;}
        public string Name {get; set;}
    }

Ensuite, nous créons notre liste d'échantillons de personnes avec différents noms et âges.

    List<Person> people = new List<Person>();
    people.Add(new Person{Age = 20, Name = "Mouse"});
    people.Add(new Person{Age = 30, Name = "Neo"});
    people.Add(new Person{Age = 40, Name = "Morpheus"});
    people.Add(new Person{Age = 30, Name = "Trinity"});
    people.Add(new Person{Age = 40, Name = "Dozer"});
    people.Add(new Person{Age = 40, Name = "Smith"});

Ensuite, nous créons une requête LINQ pour regrouper notre liste de personnes par âge.

    var query = people.GroupBy(x => x.Age);

Ce faisant, nous pouvons voir l'âge de chaque groupe et avoir une liste de chaque personne du groupe.

    foreach(var result in query)
    {
        Console.WriteLine(result.Key);
                    
        foreach(var person in result)
            Console.WriteLine(person.Name);
    }

Cela se traduit par la sortie suivante :

    20
    Mouse
    30
    Neo
    Trinity
    40
    Morpheus
    Dozer
    Smith

Vous pouvez jouer avec la [démo en direct sur .NET Fiddle][1]


[1] : https://dotnetfiddle.net/VFOZ1x

## Enumérer l'énumérable
L'interface IEnumerable<<a>T> est l'interface de base pour tous les énumérateurs génériques et constitue un élément essentiel de la compréhension de LINQ. À la base, il représente la séquence.

Cette interface sous-jacente est héritée par toutes les collections génériques, telles que [Collection<<a>T>](https://msdn.microsoft.com/en-us/library/ms132397(v=vs.110).aspx ), [Array](https://msdn.microsoft.com/en-us/library/system.array(v=vs.110).aspx), [List<<a>T>](https:// msdn.microsoft.com/en-us/library/6sh2ey19(v=vs.110).aspx), [Dictionary<TKey, TValue> Class](https://msdn.microsoft.com/en-us/library/ xfhwa508(v=vs.110).aspx) et [HashSet<<a>T>](https://msdn.microsoft.com/en-us/library/bb359438(v=vs.110).aspx) .

En plus de représenter la séquence, toute classe qui hérite de IEnumerable<<a>T> doit fournir un IEnumerator<<a>T>. L'énumérateur expose l'itérateur pour l'énumérable, et ces deux interfaces et idées interconnectées sont à l'origine du dicton "énumérer l'énumérable".

"Énumérer l'énumérable" est une expression importante. L'énumérable est simplement une structure pour savoir comment itérer, il ne contient aucun objet matérialisé. Par exemple, lors du tri, un énumérable peut contenir les critères du champ à trier, mais l'utilisation de `.OrderBy()` renverra en soi un IEnumerable<<a>T> qui ne sait *comment* trier. L'utilisation d'un appel qui matérialisera les objets, comme itérer l'ensemble, est connue sous le nom d'énumération (par exemple `.ToList()`). Le processus d'énumération utilisera la définition énumérable de *comment* afin de se déplacer dans la série et de renvoyer les objets pertinents (dans l'ordre, filtrés, projetés, etc.).

Ce n'est qu'une fois que l'énumérable a été énuméré qu'il provoque la matérialisation des objets, c'est-à-dire lorsque des métriques comme [complexité temporelle](https://en.wikipedia.org/wiki/Time_complexity) (combien de temps cela devrait prendre par rapport à la taille de la série ) et la complexité spatiale (combien d'espace doit être utilisé par rapport à la taille de la série) peuvent être mesurées.

Créer votre propre classe qui hérite de IEnumerable<<a>T> peut être un peu compliqué selon la série sous-jacente qui doit être énumérable. En général, il est préférable d'utiliser l'une des collections génériques existantes. Cela dit, il est également possible d'hériter de l'interface IEnumerable<<a>T> sans avoir de tableau défini comme structure sous-jacente.

Par exemple, en utilisant la série de Fibonacci comme séquence sous-jacente. Notez que l'appel à `Where` construit simplement un `IEnumerable`, et ce n'est que lorsqu'un appel pour énumérer cet énumérable est effectué que l'une des valeurs est matérialisée.

    void Main()
    {
        Fibonacci Fibo = new Fibonacci();
        IEnumerable<long> quadrillionplus = Fibo.Where(i => i > 1000000000000);
        Console.WriteLine("Enumerable built");
        Console.WriteLine(quadrillionplus.Take(2).Sum());
        Console.WriteLine(quadrillionplus.Skip(2).First());
    
        IEnumerable<long> fibMod612 = Fibo.OrderBy(i => i % 612);
        Console.WriteLine("Enumerable built");
        Console.WriteLine(fibMod612.First());//smallest divisible by 612
    }
    
    public class Fibonacci : IEnumerable<long>
    {
        private int max = 90;
    
        //Enumerator called typically from foreach
        public IEnumerator GetEnumerator() {
            long n0 = 1;
            long n1 = 1;
            Console.WriteLine("Enumerating the Enumerable");
            for(int i = 0; i < max; i++){
                yield return n0+n1;
                n1 += n0;
                n0 = n1-n0;
            }
        }
        
        //Enumerable called typically from linq
        IEnumerator<long> IEnumerable<long>.GetEnumerator() {
            long n0 = 1;
            long n1 = 1;
            Console.WriteLine("Enumerating the Enumerable");
            for(int i = 0; i < max; i++){
                yield return n0+n1;
                n1 += n0;
                n0 = n1-n0;
            }
        }
    }

Production

    Enumerable built
    Enumerating the Enumerable
    4052739537881
    Enumerating the Enumerable
    4052739537881
    Enumerable built
    Enumerating the Enumerable
    14930352

La force du deuxième ensemble (le fibMod612) est que même si nous avons fait l'appel pour ordonner tout notre ensemble de nombres de Fibonacci, puisqu'une seule valeur a été prise en utilisant `.First()`, la complexité temporelle était O(n) car seulement 1 valeur à comparer lors de l'exécution de l'algorithme de tri. C'est parce que notre recenseur n'a demandé qu'une seule valeur, et donc l'énumérable entier n'a pas dû être matérialisé. Si nous avions utilisé `.Take(5)` au lieu de `.First()`, l'énumérateur aurait demandé 5 valeurs, et au plus 5 valeurs auraient dû être matérialisées. Par rapport à la nécessité de commander un ensemble complet * puis * de prendre les 5 premières valeurs, le principe de permet de gagner beaucoup de temps et d'espace d'exécution.



## Où
Renvoie un sous-ensemble d'éléments pour lesquels le prédicat spécifié est vrai.

    List<string> trees = new List<string>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };

## Syntaxe de la méthode

    // Select all trees with name of length 3
    var shortTrees = trees.Where(tree => tree.Length == 3); // Oak, Elm

## Syntaxe de la requête

    var shortTrees = from tree in trees
                     where tree.Length == 3
                     select tree; // Oak, Elm


## Utilisation de Range avec diverses méthodes Linq
Vous pouvez utiliser la classe Enumerable avec les requêtes Linq pour convertir les boucles for en Linq one liners.

**Sélectionner un exemple**

Opposé à faire ceci :


    var asciiCharacters = new List<char>();
    for (var x = 0; x < 256; x++)
    {
        asciiCharacters.Add((char)x);
    }

Tu peux le faire:

    var asciiCharacters = Enumerable.Range(0, 256).Select(a => (char) a);


**Où Exemple**

Dans cet exemple, 100 numéros seront générés et même certains seront extraits

    var evenNumbers = Enumerable.Range(1, 100).Where(a => a % 2 == 0);



## Utilisation de SelectMany au lieu de boucles imbriquées
Étant donné 2 listes

    var list1 = new List<string> { "a", "b", "c" };
    var list2 = new List<string> { "1", "2", "3", "4" };

si vous voulez sortir toutes les permutations, vous pouvez utiliser des boucles imbriquées comme

    var result = new List<string>();
    foreach (var s1 in list1)
        foreach (var s2 in list2)
            result.Add($"{s1}{s2}");

En utilisant SelectMany, vous pouvez faire la même opération que

    var result = list1.SelectMany(x => list2.Select(y => $"{x}{y}", x, y)).ToList();


## Contient
MSDN :
> Détermine si une séquence contient un élément spécifié à l'aide d'un
> spécifié `IEqualityComparer<T>`


    List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };
    var result1 = numbers.Contains(4); // true
    var result2 = numbers.Contains(8); // false

    List<int> secondNumberCollection = new List<int> { 4, 5, 6, 7 };
    // Note that can use the Intersect method in this case
    var result3 = secondNumberCollection.Where(item => numbers.Contains(item)); // will be true only for 4,5


Utilisation d'un objet défini par l'utilisateur :

    public class Person
    {
       public string Name { get; set; }
    }

    List<Person> objects = new List<Person>
    {
        new Person { Name = "Nikki"},
        new Person { Name = "Gilad"},
        new Person { Name = "Phil"},
        new Person { Name = "John"}
    };

    //Using the Person's Equals method - override Equals() and GetHashCode() - otherwise it
    //will compare by reference and result will be false
    var result4 = objects.Contains(new Person { Name = "Phil" }); // true

Utilisation de la surcharge `Enumerable.Contains(value, comparer)` :

    public class Compare : IEqualityComparer<Person>
    {
        public bool Equals(Person x, Person y)
        {
            return x.Name == y.Name;
        }
        public int GetHashCode(Person codeh)
        {
            return codeh.Name.GetHashCode();
        }
    }

    var result5 = objects.Contains(new Person { Name = "Phil" }, new Compare()); // true

** Une utilisation intelligente de ````Contains```` serait de remplacer plusieurs clauses ````if```` par un appel ````Contains````.**

Donc au lieu de faire ça :

    if(status == 1 || status == 3 || status == 4)
    {
        //Do some business operation
    }
    else
    {
        //Do something else
    }
 
Faites ceci :
    
    if(new int[] {1, 3, 4 }.Contains(status)
    {
        //Do some business operaion
    }
    else 
    {
        //Do something else
    }

## GroupBy un ou plusieurs champs
Supposons que nous ayons un modèle de film :

    public class Film {
        public string Title { get; set; }
        public string Category { get; set; }
        public int Year { get; set; }
    }

Regrouper par propriété de catégorie :

    foreach (var grp in films.GroupBy(f => f.Category)) {
        var groupCategory = grp.Key;
        var numberOfFilmsInCategory = grp.Count();
    }

Regrouper par catégorie et année :

    foreach (var grp in films.GroupBy(f => new { Category = f.Category, Year = f.Year })) {
        var groupCategory = grp.Key.Category;
        var groupYear = grp.Key.Year;
        var numberOfFilmsInCategory = grp.Count();
    }



## Ordre des requêtes - OrderBy() ThenBy() OrderByDescending() ThenByDescending()
    string[] names= { "mark", "steve", "adam" };

**Ascendant:**

*Syntaxe de la requête*

    var sortedNames =
        from name in names
        orderby name
        select name;

*Syntaxe de la méthode*

    var sortedNames = names.OrderBy(name => name);

sortedNames contient les noms dans l'ordre suivant :
"adam","marque","steve"

**Descendant:**


*Syntaxe de la requête*

    var sortedNames =
        from name in names
        orderby name descending
        select name;

*Syntaxe de la méthode*

    var sortedNames = names.OrderByDescending(name => name);

sortedNames contient les noms dans l'ordre suivant :
"steve","marque","adam"

**Ordre par plusieurs champs**

    Person[] people =
    {
        new Person { FirstName = "Steve", LastName = "Collins", Age = 30},
        new Person { FirstName = "Phil" , LastName = "Collins", Age = 28},
        new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 29},
        new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 15}
    };

*Syntaxe de la requête*

    var sortedPeople = from person in people
                       orderby person.LastName, person.FirstName, person.Age descending
                       select person;

*Syntaxe de la méthode*

     sortedPeople = people.OrderBy(person => person.LastName)
                          .ThenBy(person => person.FirstName)
                          .ThenByDescending(person => person.Age);

*Résultat*

    1. Adam Ackerman 29
    2. Adam Ackerman 15
    3. Phil Collins  28
    4. Steve Collins 30



## AuDictionnaire
La méthode LINQ `ToDictionary()` peut être utilisée pour générer une collection `Dictionary<TKey, TElement>` basée sur une source `IEnumerable<T>` donnée.

    IEnumerable<User> users = GetUsers();
    Dictionary<int, User> usersById = users.ToDictionary(x => x.Id);

Dans cet exemple, l'unique argument passé à `ToDictionary` est de type `Func<TSource, TKey>`, qui renvoie la clé de chaque élément.

Il s'agit d'une manière concise d'effectuer l'opération suivante :

    Dictionary<int, User> usersById = new Dictionary<int User>();
    foreach (User u in users) 
    {
      usersById.Add(u.Id, u);
    }

Vous pouvez également passer un deuxième paramètre à la méthode `ToDictionary`, qui est de type `Func<TSource, TElement>` et renvoie la `Value` à ajouter pour chaque entrée.

    IEnumerable<User> users = GetUsers();
    Dictionary<int, string> userNamesById = users.ToDictionary(x => x.Id, x => x.Name);

Il est également possible de spécifier le `IComparer` qui est utilisé pour comparer les valeurs clés. Cela peut être utile lorsque la clé est une chaîne et que vous souhaitez qu'elle soit insensible à la casse.

    IEnumerable<User> users = GetUsers();
    Dictionary<string, User> usersByCaseInsenstiveName = users.ToDictionary(x => x.Name, StringComparer.InvariantCultureIgnoreCase);

    var user1 = usersByCaseInsenstiveName["john"];
    var user2 = usersByCaseInsenstiveName["JOHN"];
    user1 == user2; // Returns true

Remarque : la méthode "ToDictionary" nécessite que toutes les clés soient uniques, il ne doit pas y avoir de clés en double. S'il y en a, une exception est levée : `ArgumentException : un élément avec la même clé a déjà été ajouté.` Si vous avez un scénario dans lequel vous savez que vous aurez plusieurs éléments avec la même clé, alors vous feriez mieux d'utiliser [`ToLookup`](https://www.wikiod.com/fr/docs/c%23/68/linq-queries/14871/tolookup) à la place.




## Sauter pendant
`SkipWhile()` est utilisé pour exclure des éléments jusqu'à la première non-correspondance (cela peut être contre-intuitif pour la plupart)

    int[] list = { 42, 42, 6, 6, 6, 42 };
    var result = list.SkipWhile(i => i == 42); 
    // Result: 6, 6, 6, 42

## Par défautSiVide
DefaultIfEmpty est utilisé pour renvoyer un élément par défaut si la séquence ne contient aucun élément. Cet élément peut être la valeur par défaut du type ou une instance définie par l'utilisateur de ce type. Exemple:

    var chars = new List<string>() { "a", "b", "c", "d" };

    chars.DefaultIfEmpty("N/A").FirstOrDefault(); // returns "a";
    
    chars.Where(str => str.Length > 1)
         .DefaultIfEmpty("N/A").FirstOrDefault(); // return "N/A"

    chars.Where(str => str.Length > 1)
            .DefaultIfEmpty().First(); // returns null;

**Utilisation dans les jointures gauches** :
--------------------

Avec `DefaultIfEmpty`, la jointure Linq traditionnelle peut renvoyer un objet par défaut si aucune correspondance n'a été trouvée. Agissant ainsi comme une jointure gauche SQL. Exemple:

    var leftSequence = new List<int>() { 99, 100, 5, 20, 102, 105 };
    var rightSequence = new List<char>() { 'a', 'b', 'c', 'i', 'd' };
    
    var numbersAsChars = from l in leftSequence
                         join r in rightSequence
                         on l equals (int)r into leftJoin
                         from result in leftJoin.DefaultIfEmpty('?')
                         select new
                         {
                             Number = l,
                             Character = result
                         };
    
    foreach(var item in numbersAsChars)
    {
        Console.WriteLine("Num = {0} ** Char = {1}", item.Number, item.Character);
    }

    ouput: 

    Num = 99         Char = c
    Num = 100        Char = d
    Num = 5          Char = ?
    Num = 20         Char = ?
    Num = 102        Char = ?
    Num = 105        Char = i

Dans le cas où un `DefaultIfEmpty` est utilisé (sans spécifier de valeur par défaut) et qu'il n'en résultera aucun élément correspondant sur la bonne séquence, il faut s'assurer que l'objet n'est pas `null` avant d'accéder à ses propriétés. Sinon, cela entraînera une `NullReferenceException`. Exemple:

    var leftSequence = new List<int> { 1, 2, 5 };
    var rightSequence = new List<dynamic>()
        {
            new { Value = 1 },
            new { Value = 2 },
            new { Value = 3 },
            new { Value = 4 },
        };

    var numbersAsChars = (from l in leftSequence
                            join r in rightSequence
                            on l equals r.Value into leftJoin
                            from result in leftJoin.DefaultIfEmpty()
                            select new
                            {
                                Left = l,
                                // 5 will not have a matching object in the right so result 
                                // will be equal to null. 
                                // To avoid an error use:
                                //    -  C# 6.0 or above - ?. 
                                //    -  Under           - result == null ? 0 : result.Value
                                Right = result?.Value
                            }).ToList();



## SéquenceÉgal
`SequenceEqual` est utilisé pour comparer deux séquences `IEnumerable<T>` entre elles.


    int[] a = new int[] {1, 2, 3};
    int[] b = new int[] {1, 2, 3};
    int[] c = new int[] {1, 3, 2};

    bool returnsTrue = a.SequenceEqual(b);
    bool returnsFalse = a.SequenceEqual(c);

## ElementAt et ElementAtOrDefault
`ElementAt` renverra l'élément à l'index `n`. Si `n` n'est pas dans la plage de l'énumérable, lève une `ArgumentOutOfRangeException`.

    int[] numbers  = { 1, 2, 3, 4, 5 };
    numbers.ElementAt(2);  // 3
    numbers.ElementAt(10); // throws ArgumentOutOfRangeException

`ElementAtOrDefault` renverra l'élément à l'index `n`. Si `n` n'est pas dans la plage de l'énumérable, renvoie un `default(T)`.

    int[] numbers  = { 1, 2, 3, 4, 5 };
    numbers.ElementAtOrDefault(2);  // 3
    numbers.ElementAtOrDefault(10); // 0 = default(int)

`ElementAt` et `ElementAtOrDefault` sont optimisés lorsque la source est un `IList<T>` et l'indexation normale sera utilisée dans ces cas.

Notez que pour `ElementAt`, si l'index fourni est supérieur à la taille de `IList<T>`, la liste devrait (mais ce n'est techniquement pas garanti) lancer une `ArgumentOutOfRangeException`.


## Joindre plusieurs séquences
Considérez les entités `Customer`, `Purchase` et `PurchaseItem` comme suit :

    public class Customer
    {
       public string Id { get; set } // A unique Id that identifies customer    
       public string Name  {get; set; }
    }

    public class Purchase
    {
       public string Id { get; set }
       public string CustomerId {get; set; }
       public string Description { get; set; }
    }

    public class PurchaseItem
    {
       public string Id { get; set }
       public string PurchaseId {get; set; }
       public string Detail { get; set; }
    }

Considérez les exemples de données suivants pour les entités ci-dessus :

    var customers = new List<Customer>()             
     {
        new Customer() {
            Id = Guid.NewGuid().ToString(),
            Name = "Customer1"            
        },
                
        new Customer() {
            Id = Guid.NewGuid().ToString(),
            Name = "Customer2"            
        }
     };        
        
     var purchases = new List<Purchase>() 
     {
         new Purchase() {                
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[0].Id,
             Description = "Customer1-Purchase1"            
         },

         new Purchase() {
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[0].Id,
             Description = "Customer1-Purchase2"            
         },
         
         new Purchase() {
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[1].Id,
             Description = "Customer2-Purchase1"            
         },

         new Purchase() {
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[1].Id,
             Description = "Customer2-Purchase2"            
         }
      };
        
     var purchaseItems = new List<PurchaseItem>() 
     {
         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[0].Id,
             Detail = "Purchase1-PurchaseItem1"            
         },

         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[1].Id,
             Detail = "Purchase2-PurchaseItem1"            
         },
         
         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[1].Id,
             Detail = "Purchase2-PurchaseItem2"            
         },

         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[3].Id,
             Detail = "Purchase3-PurchaseItem1"
         }
     };

Maintenant, considérons la requête linq ci-dessous :

    var result = from c in customers
                join p in purchases on c.Id equals p.CustomerId           // first join
                join pi in purchaseItems on p.Id equals pi.PurchaseId     // second join
                select new
                {
                   c.Name, p.Description, pi.Detail
                };


Pour afficher le résultat de la requête ci-dessus :

    foreach(var resultItem in result)
    {
        Console.WriteLine($"{resultItem.Name}, {resultItem.Description}, {resultItem.Detail}");
    }
        
Le résultat de la requête serait :

> Client1, Client1-Achat1, Achat1-PurchaseItem1
> 
> Client1, Client1-Achat2, Achat2-PurchaseItem1
> 
> Client1, Client1-Achat2, Achat2-PurchaseItem2
> 
> Client2, Client2-Achat2, Achat3-PurchaseItem1

[Démo en direct sur .NET Fiddle][1]


[1] : https://dotnetfiddle.net/Db8uqp

## Joindre sur plusieurs clés
  

      PropertyInfo[] stringProps = typeof (string).GetProperties();//string properties
      PropertyInfo[] builderProps = typeof(StringBuilder).GetProperties();//stringbuilder properties
        
        var query =
            from s in stringProps
            join b in builderProps
                on new { s.Name, s.PropertyType } equals new { b.Name, b.PropertyType }
            select new
            {
                s.Name,
                s.PropertyType,
                StringToken = s.MetadataToken,
                StringBuilderToken = b.MetadataToken
            };

Notez que les types anonymes dans `join` ci-dessus doivent contenir les mêmes propriétés puisque les objets ne sont considérés comme égaux que si toutes leurs propriétés sont égales. Sinon, la requête ne sera pas compilée.

## Somme
La méthode d'extension `Enumerable.Sum` calcule la somme des valeurs numériques.

Dans le cas où les éléments de la collection sont eux-mêmes des nombres, vous pouvez calculer directement la somme.

    int[] numbers = new int[] { 1, 4, 6 };
    Console.WriteLine( numbers.Sum() ); //outputs 11

Si le type des éléments est un type complexe, vous pouvez utiliser une expression lambda pour spécifier la valeur à calculer :

    var totalMonthlySalary = employees.Sum( employee => employee.MonthlySalary );

La méthode d'extension de somme peut calculer avec les types suivants :

- Int32
- Int64
- Seul
- Double
- Décimal

Si votre collection contient des types nullables, vous pouvez utiliser l'opérateur null-coalescing pour définir une valeur par défaut pour les éléments null :

    int?[] numbers = new int?[] { 1, null, 6 };
    Console.WriteLine( numbers.Sum( number => number ?? 0 ) ); //outputs 7

## Pour rechercher
> ToLookup renvoie une structure de données qui permet l'indexation. C'est un
> méthode d'extension. Il produit une instance ILookup qui peut être indexée
> ou énuméré à l'aide d'une boucle foreach. Les entrées sont regroupées en
> regroupements à chaque touche. - dotnetperls

    string[] array = { "one", "two", "three" };
    //create lookup using string length as key
    var lookup = array.ToLookup(item => item.Length);
    
    //join the values whose lengths are 3
    Console.WriteLine(string.Join(",",lookup[3]));
    //output: one,two

Un autre exemple:

    int[] array = { 1,2,3,4,5,6,7,8 };
    //generate lookup for odd even numbers (keys will be 0 and 1)
    var lookup = array.ToLookup(item => item % 2);
    
    //print even numbers after joining
    Console.WriteLine(string.Join(",",lookup[0]));
    //output: 2,4,6,8

    //print odd numbers after joining
    Console.WriteLine(string.Join(",",lookup[1]));
    //output: 1,3,5,7

## Any and First(OrDefault) - meilleure pratique
Je n'expliquerai pas ce que font `Any` et `FirstOrDefault` car il existe déjà deux bons exemples à leur sujet. Voir https://www.wikiod.com/fr/docs/c%23/68/linq-queries/5098/any#t=201707200324548979636 et https://www.wikiod.com/fr/docs/c%23/68/linq-queries/329 /first-firstordefault-last-lastordefault-single-and-singleordefault#t=201707200328069088515 pour plus d'informations.

Un modèle que je vois souvent dans le code qui ** devrait être évité ** est

    if (myEnumerable.Any(t=>t.Foo == "Bob"))
    {
        var myFoo = myEnumerable.First(t=>t.Foo == "Bob");
        //Do stuff
    }

Il pourrait être écrit plus efficacement comme ceci

    var myFoo = myEnumerable.FirstOrDefault(t=>t.Foo == "Bob");
    if (myFoo != null)
    {
        //Do stuff
    }

En utilisant le deuxième exemple, la collection n'est parcourue qu'une seule fois et donne le même résultat que le premier. La même idée peut être appliquée à `Single`.

## GroupBy Somme et compte
Prenons un exemple de classe :

    public class Transaction
    {
        public string Category { get; set; }
        public DateTime Date { get; set; }
        public decimal Amount { get; set; }
    }

Considérons maintenant une liste de transactions :

    var transactions = new List<Transaction>
    {
       new Transaction { Category = "Saving Account", Amount = 56, Date = DateTime.Today.AddDays(1) },
       new Transaction { Category = "Saving Account", Amount = 10, Date = DateTime.Today.AddDays(-10) },
       new Transaction { Category = "Credit Card", Amount = 15, Date = DateTime.Today.AddDays(1) },
       new Transaction { Category = "Credit Card", Amount = 56, Date = DateTime.Today },
       new Transaction { Category = "Current Account", Amount = 100, Date = DateTime.Today.AddDays(5) },
    };

Si vous souhaitez calculer la somme du montant et du nombre par catégorie, vous pouvez utiliser GroupBy comme suit :

    var summaryApproach1 = transactions.GroupBy(t => t.Category)
                               .Select(t => new
                               {
                                   Category = t.Key,
                                   Count = t.Count(),
                                   Amount = t.Sum(ta => ta.Amount),
                               }).ToList();
    
    Console.WriteLine("-- Summary: Approach 1 --");
    summaryApproach1.ForEach(
                row => Console.WriteLine($"Category: {row.Category}, Amount: {row.Amount}, Count: {row.Count}"));

Alternativement, vous pouvez le faire en une seule étape :

    var summaryApproach2 = transactions.GroupBy(t => t.Category, (key, t) =>
    {
            var transactionArray = t as Transaction[] ?? t.ToArray();
            return new
            {
                Category = key,
                Count = transactionArray.Length,
                Amount = transactionArray.Sum(ta => ta.Amount),
            };
    }).ToList();

    Console.WriteLine("-- Summary: Approach 2 --");
    summaryApproach2.ForEach(
    row => Console.WriteLine($"Category: {row.Category}, Amount: {row.Amount}, Count: {row.Count}"));

Le résultat des deux requêtes ci-dessus serait le même :

> Catégorie : Compte d'épargne, Montant : 66, Nombre : 2
>
> Catégorie : Carte de crédit, Montant : 71, Nombre : 2
>
> Catégorie : Compte courant, Montant : 100, Nombre : 1

[Démo en direct dans .NET Fiddle][1]


[1] : https://dotnetfiddle.net/1PfLGq#

## Commandé par
> Ordonne une collection par une valeur spécifiée.

Lorsque la valeur est un **entier**, un **double** ou un **float**, elle commence par la *valeur minimale*, ce qui signifie que vous obtenez d'abord les valeurs négatives, puis zéro et ensuite les valeurs positives (voir Exemple 1).

Lorsque vous commandez par un **char**, la méthode compare les *valeurs ascii* des caractères pour trier la collection (voir l'exemple 2).

Lorsque vous triez des **chaînes**, la méthode OrderBy les compare en examinant leur [CultureInfo][1] mais en commençant normalement par la *première lettre* de l'alphabet (a,b,c...).

Ce type d'ordre est appelé ascendant, si vous le voulez dans l'autre sens, vous devez descendre (voir OrderByDescending).

**Exemple 1:**

    int[] numbers = {2, 1, 0, -1, -2};
    IEnumerable<int> ascending = numbers.OrderBy(x => x);
    // returns {-2, -1, 0, 1, 2}

**Exemple 2 :**

     char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
     IEnumerable<char> ascending = letters.OrderBy(x => x);
     // returns { ' ', '!', '+', '1', '9', '?', 'A', 'B', 'Y', 'Z', '[', 'a', 'b', 'y', 'z', '{' }

**Exemple:**

    class Person
    {
       public string Name { get; set; }
       public int Age { get; set; }
    }

    var people = new[]
    {
        new Person {Name = "Alice", Age = 25},
        new Person {Name = "Bob", Age = 21},
        new Person {Name = "Carol", Age = 43}
    };
    var youngestPerson = people.OrderBy(x => x.Age).First();
    var name = youngestPerson.Name; // Bob


[1] : https://msdn.microsoft.com/en-us/library/xk2wykcz(VS.71).aspx

## Select - Transformer des éléments
Select vous permet d'appliquer une transformation à chaque élément de toute structure de données implémentant IEnumerable.

Obtenir le premier caractère de chaque chaîne dans la liste suivante :

    List<String> trees = new List<String>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };
    
Utilisation de la syntaxe régulière (lambda)

    //The below select stament transforms each element in tree into its first character.
    IEnumerable<String> initials = trees.Select(tree => tree.Substring(0, 1));
    foreach (String initial in initials) {
        System.Console.WriteLine(initial);
    }

**Production:**
> O
>B
>B
>E
>H
> M

[Démo en direct sur .NET Fiddle][1]

Utilisation de la syntaxe de requête LINQ

    initials = from tree in trees
               select tree.Substring(0, 1);


[1] : https://dotnetfiddle.net/yYLT0K

## Syndicat
Fusionne deux collections pour créer une collection distincte à l'aide du comparateur d'égalité par défaut

    int[] numbers1 = { 1, 2, 3 };
    int[] numbers2 = { 2, 3, 4, 5 };
    
    var allElement = numbers1.Union(numbers2);   // AllElement now contains 1,2,3,4,5


[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/oet2Uq)

## Count et LongCount
`Count` renvoie le nombre d'éléments dans un `IEnumerable<T>`. `Count` expose également un paramètre de prédicat facultatif qui vous permet de filtrer les éléments que vous souhaitez compter.

    int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };
    
    int n = array.Count(); // returns the number of elements in the array
    int x = array.Count(i => i > 2); // returns the number of elements in the array greater than 2

`LongCount` fonctionne de la même manière que `Count` mais a un type de retour de `long` et est utilisé pour compter les séquences `IEnumerable<T>` qui sont plus longues que `int.MaxValue`

    int[] array = GetLargeArray();
    
    long n = array.LongCount(); // returns the number of elements in the array
    long x = array.LongCount(i => i > 100); // returns the number of elements in the array greater than 100


## Construction incrémentielle d'une requête
Étant donné que LINQ utilise l'**exécution différée**, nous pouvons avoir un objet de requête qui ne contient pas réellement les valeurs, mais renverra les valeurs lors de l'évaluation. Nous pouvons ainsi construire dynamiquement la requête basée sur notre flux de contrôle, et l'évaluer une fois que nous avons terminé :

    IEnumerable<VehicleModel> BuildQuery(int vehicleType, SearchModel search, int start = 1, int count = -1) {
        IEnumerable<VehicleModel> query = _entities.Vehicles
            .Where(x => x.Active && x.Type == vehicleType)
            .Select(x => new VehicleModel {
                Id = v.Id,
                Year = v.Year,
                Class = v.Class,
                Make = v.Make,
                Model = v.Model,
                Cylinders = v.Cylinders ?? 0
            });

Nous pouvons conditionnellement appliquer des filtres :

        if (!search.Years.Contains("all", StringComparer.OrdinalIgnoreCase))
            query = query.Where(v => search.Years.Contains(v.Year));

        if (!search.Makes.Contains("all", StringComparer.OrdinalIgnoreCase)) {
            query = query.Where(v => search.Makes.Contains(v.Make));
        }

        if (!search.Models.Contains("all", StringComparer.OrdinalIgnoreCase)) {
            query = query.Where(v => search.Models.Contains(v.Model));
        }

        if (!search.Cylinders.Equals("all", StringComparer.OrdinalIgnoreCase)) {
            decimal minCylinders = 0;
            decimal maxCylinders = 0;
            switch (search.Cylinders) {
                case "2-4":
                    maxCylinders = 4;
                    break;
                case "5-6":
                    minCylinders = 5;
                    maxCylinders = 6;
                    break;
                case "8":
                    minCylinders = 8;
                    maxCylinders = 8;
                    break;
                case "10+":
                    minCylinders = 10;
                    break;
            }
            if (minCylinders > 0) {
                query = query.Where(v => v.Cylinders >= minCylinders);
            }
            if (maxCylinders > 0) {
                query = query.Where(v => v.Cylinders <= maxCylinders);
            }
        }

Nous pouvons ajouter un ordre de tri à la requête en fonction d'une condition :

        switch (search.SortingColumn.ToLower()) {
            case "make_model":
                query = query.OrderBy(v => v.Make).ThenBy(v => v.Model);
                break;
            case "year":
                query = query.OrderBy(v => v.Year);
                break;
            case "engine_size":
                query = query.OrderBy(v => v.EngineSize).ThenBy(v => v.Cylinders);
                break;
            default:
                query = query.OrderBy(v => v.Year); //The default sorting.
        }
        
Notre requête peut être définie pour démarrer à partir d'un point donné :
        
        query = query.Skip(start - 1);
        
et défini pour renvoyer un nombre spécifique d'enregistrements :

        if (count > -1) {
            query = query.Take(count);
        }
        return query;
    }
    
---

Une fois que nous avons l'objet de requête, nous pouvons évaluer les résultats avec une boucle `foreach` ou l'une des méthodes LINQ qui renvoie un ensemble de valeurs, telles que `ToList` ou `ToArray` :

    SearchModel sm;
    
    // populate the search model here
    // ...
    
    List<VehicleModel> list = BuildQuery(5, sm).ToList();

## GroupJoin avec une variable de plage externe


    Customer[] customers = Customers.ToArray();
    Purchase[] purchases = Purchases.ToArray();
    
    var groupJoinQuery =
        from c in customers
        join p in purchases on c.ID equals p.CustomerID
        into custPurchases
        select new
        {
            CustName = c.Name,
            custPurchases
        };



## Quantificateurs Linq
Les opérations de quantification renvoient une valeur booléenne si certains ou tous les éléments d'une séquence satisfont à une condition. Dans cet article, nous verrons quelques scénarios LINQ to Objects courants dans lesquels nous pouvons utiliser ces opérateurs.
Il existe 3 opérations Quantifiers qui peuvent être utilisées dans LINQ :

`All` - utilisé pour déterminer si tous les éléments d'une séquence satisfont à une condition.
Par exemple:

    int[] array = { 10, 20, 30 }; 
       
    // Are all elements >= 10? YES
    array.All(element => element >= 10); 
       
    // Are all elements >= 20? NO
    array.All(element => element >= 20);
        
    // Are all elements < 40? YES
    array.All(element => element < 40);
    
`Any` - utilisé pour déterminer si des éléments d'une séquence satisfont à une condition.
Par exemple:

    int[] query=new int[] { 2, 3, 4 }
    query.Any (n => n == 3);

`Contient` - utilisé pour déterminer si une séquence contient un élément spécifié.
Par exemple:

    //for int array
    int[] query =new int[] { 1,2,3 };
    query.Contains(1);
    
    //for string array
    string[] query={"Tom","grey"};
    query.Contains("Tom");
    
    //for a string
    var stringValue="hello";
    stringValue.Contains("h");



## PrendrePendant
`TakeWhile` renvoie les éléments d'une séquence tant que la condition est vraie

    int[] list = { 1, 10, 40, 50, 44, 70, 4 };
    var result = list.TakeWhile(item => item < 50).ToList();
    // result = { 1, 10, 40 }

## Construisez vos propres opérateurs Linq pour IEnumerable<T>
L'un des avantages de Linq est qu'il est si facile à étendre. Il vous suffit de créer une [méthode d'extension][1] dont l'argument est `IEnumerable<T>`.

    public namespace MyNamespace
    {
        public static class LinqExtensions
        {
            public static IEnumerable<List<T>> Batch<T>(this IEnumerable<T> source, int batchSize)
            {
                var batch = new List<T>();
                foreach (T item in source)
                {
                    batch.Add(item);
                    if (batch.Count == batchSize)
                    {
                        yield return batch;
                        batch = new List<T>();
                    }
                }
                if (batch.Count > 0)
                    yield return batch;
            }
        }
    }

Cet exemple divise les éléments d'un `IEnumerable<T>` en listes de taille fixe, la dernière liste contenant le reste des éléments. Remarquez comment l'objet auquel la méthode d'extension est appliquée est passé (argument `source`) comme argument initial à l'aide du mot clé `this`. Ensuite, le mot-clé `yield` est utilisé pour afficher l'élément suivant dans la sortie `IEnumerable<T>` avant de poursuivre l'exécution à partir de ce point (voir [mot-clé rendement][2]).

Cet exemple serait utilisé dans votre code comme ceci :

    //using MyNamespace;
    var items = new List<int> { 2, 3, 4, 5, 6 };
    foreach (List<int> sublist in items.Batch(3))
    {
        // do something
    }

Sur la première boucle, la sous-liste serait `{2, 3, 4}` et sur la seconde `{5, 6}`.

Les méthodes LinQ personnalisées peuvent également être combinées avec les méthodes LinQ standard. par exemple.:

    //using MyNamespace;
    var result = Enumerable.Range(0, 13)         // generate a list
                           .Where(x => x%2 == 0) // filter the list or do something other
                           .Batch(3)             // call our extension method
                           .ToList()             // call other standard methods

Cette requête renverra des nombres pairs regroupés en lots de taille 3 : `{0, 2, 4}, {6, 8, 10}, {12}`

N'oubliez pas que vous avez besoin d'une ligne `using MyNamespace;` pour pouvoir accéder à la méthode d'extension.

[1] : https://www.wikiod.com/fr/docs/c%23/20/extension-methods/33/using-an-extension-method#t=201607280952261411896
[2] : https://www.wikiod.com/fr/docs/c%23/61/yield-keyword#t=201607281004094832778

## Inverse

- Inverse l'ordre des éléments dans une séquence.
- S'il n'y a pas d'éléments, une `ArgumentNullException : la source est nulle.`

***Exemple:***

    // Create an array.
    int[] array = { 1, 2, 3, 4 };                         //Output:
    // Call reverse extension method on the array.        //4
    var reverse = array.Reverse();                        //3
    // Write contents of array to screen.                 //2
    foreach (int value in reverse)                        //1
        Console.WriteLine(value);
  
[Exemple de code en direct] (https://dotnetfiddle.net/ckrWUo)

N'oubliez pas que `Reverse()` peut fonctionner différemment selon l'ordre de la chaîne de vos instructions LINQ.

            //Create List of chars
            List<int> integerlist = new List<int>() { 1, 2, 3, 4, 5, 6 };

            //Reversing the list then taking the two first elements
            IEnumerable<int> reverseFirst = integerlist.Reverse<int>().Take(2);
            
            //Taking 2 elements and then reversing only thos two
            IEnumerable<int> reverseLast = integerlist.Take(2).Reverse();
            
            //reverseFirst output: 6, 5
            //reverseLast output:  2, 1

[Exemple de code en direct] (https://dotnetfiddle.net/ckrWUo)

*Reverse()* fonctionne en tamponnant tout puis en le parcourant en arrière, ce qui n'est pas très efficace, mais OrderBy non plus de ce point de vue.

Dans LINQ-to-Objects, il existe des opérations de mise en mémoire tampon (Reverse, OrderBy, GroupBy, etc.) et des opérations sans mise en mémoire tampon (Where, Take, Skip, etc.).


***Exemple : extension inverse sans mise en mémoire tampon***
    
    public static IEnumerable<T> Reverse<T>(this IList<T> list) {
        for (int i = list.Count - 1; i >= 0; i--) 
            yield return list[i];
    }

[Exemple de code en direct] (https://dotnetfiddle.net/ckrWUo)

Cette méthode peut rencontrer des problèmes si vous modifiez la liste lors de l'itération.





## OrderByDescending
> Ordonne une collection par une valeur spécifiée.

Lorsque la valeur est un **entier**, un **double** ou un **float**, elle commence par la *valeur maximale*, ce qui signifie que vous obtenez d'abord les valeurs positives, puis zéro et ensuite les valeurs négatives (voir Exemple 1).

Lorsque vous commandez par un **char**, la méthode compare les *valeurs ascii* des caractères pour trier la collection (voir l'exemple 2).

Lorsque vous triez des **chaînes**, la méthode OrderBy les compare en examinant leur [CultureInfo][1] mais en commençant normalement par la *dernière lettre* de l'alphabet (z,y,x,...).

Ce type d'ordre est appelé décroissant, si vous le voulez dans l'autre sens, vous devez monter (voir OrderBy).

**Exemple 1:**

    int[] numbers = {-2, -1, 0, 1, 2};
    IEnumerable<int> descending = numbers.OrderByDescending(x => x);
    // returns {2, 1, 0, -1, -2}

**Exemple 2 :**

    char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
    IEnumerable<char> descending = letters.OrderByDescending(x => x);
    // returns { '{', 'z', 'y', 'b', 'a', '[', 'Z', 'Y', 'B', 'A', '?', '9', '1', '+', '!', ' ' }

**Exemple 3 :**
    
    class Person
    {
       public  string Name { get; set; }
       public  int Age { get; set; }
    }

    var people = new[]
    {
        new Person {Name = "Alice", Age = 25},
        new Person {Name = "Bob", Age = 21},
        new Person {Name = "Carol", Age = 43}
    };
    var oldestPerson = people.OrderByDescending(x => x.Age).First();
    var name = oldestPerson.Name; // Carol


[1] : https://msdn.microsoft.com/en-us/library/xk2wykcz(VS.71).aspx

## Concat
Fusionne deux collections (sans supprimer les doublons)

    List<int> foo = new List<int> { 1, 2, 3 };
    List<int> bar = new List<int> { 3, 4, 5 };

    // Through Enumerable static class
    var result = Enumerable.Concat(foo, bar).ToList(); // 1,2,3,3,4,5

    // Through extension method
    var result = foo.Concat(bar).ToList(); // 1,2,3,3,4,5

## Select with Func<TSource, int, TResult> selector - Utiliser pour obtenir le classement des éléments
L'une des surcharges des méthodes d'extension `Select` transmet également l'`index` de l'élément actuel de la collection en cours de `select`ed. Ce sont quelques utilisations de celui-ci.

**Obtenez le "numéro de ligne" des articles**

    var rowNumbers = collection.OrderBy(item => item.Property1)
                               .ThenBy(item => item.Property2)
                               .ThenByDescending(item => item.Property3)
                               .Select((item, index) => new { Item = item, RowNumber = index })
                               .ToList();

**Obtenir le classement d'un élément *au sein* de son groupe**

    var rankInGroup = collection.GroupBy(item => item.Property1)
                                .OrderBy(group => group.Key)
                                .SelectMany(group => group.OrderBy(item => item.Property2)
                                                       .ThenByDescending(item => item.Property3)
                                                       .Select((item, index) => new 
                                                       { 
                                                           Item = item, 
                                                           RankInGroup = index 
                                                       })).ToList();

**Obtenez le classement des groupes (également appelé dense_rank dans Oracle)**

    var rankOfBelongingGroup = collection.GroupBy(item => item.Property1)
                                .OrderBy(group => group.Key)
                                .Select((group, index) => new
                                {
                                    Items = group,
                                    Rank = index
                                })
                                .SelectMany(v => v.Items, (s, i) => new
                                {
                                    Item = i,
                                    DenseRank = s.Rank
                                }).ToList();

Pour tester cela, vous pouvez utiliser:

    public class SomeObject
    {
        public int Property1 { get; set; }
        public int Property2 { get; set; }
        public int Property3 { get; set; }

        public override string ToString()
        {
            return string.Join(", ", Property1, Property2, Property3);
        }
    }

Et les données :

    List<SomeObject> collection = new List<SomeObject>
    {
        new SomeObject { Property1 = 1, Property2 = 1, Property3 = 1},
        new SomeObject { Property1 = 1, Property2 = 2, Property3 = 1},
        new SomeObject { Property1 = 1, Property2 = 2, Property3 = 2},
        new SomeObject { Property1 = 2, Property2 = 1, Property3 = 1},
        new SomeObject { Property1 = 2, Property2 = 2, Property3 = 1},
        new SomeObject { Property1 = 2, Property2 = 2, Property3 = 1},
        new SomeObject { Property1 = 2, Property2 = 3, Property3 = 1}
    };


