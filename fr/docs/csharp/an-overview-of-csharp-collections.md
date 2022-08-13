---
title: "Un aperçu des collections c#"
slug: "un-apercu-des-collections-c"
draft: false
images: []
weight: 9870
type: docs
toc: true
---

## Ensemble de hachage<T>
Il s'agit d'une collection d'éléments uniques, avec une recherche O(1).

    HashSet<int> validStoryPointValues = new HashSet<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(1)

À titre de comparaison, faire un `Contains` sur une List<int> donne de moins bonnes performances :

    List<int> validStoryPointValues = new List<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(n)

`HashSet.Contains` utilise une table de hachage, de sorte que les recherches sont extrêmement rapides, quel que soit le nombre d'éléments dans la collection.

## Dictionary<TKey, TValue>
Dictionary<TKey, TValue> est une carte. Pour une clé donnée, il peut y avoir une valeur dans le dictionnaire.

    using System.Collections.Generic;

    var people = new Dictionary<string, int>
    {
        { "John", 30 }, {"Mary", 35}, {"Jack", 40}
    };

    // Reading data
    Console.WriteLine(people["John"]); // 30
    Console.WriteLine(people["George"]); // throws KeyNotFoundException
    
    int age;
    if (people.TryGetValue("Mary", out age))
    { 
        Console.WriteLine(age); // 35
    }
    
    // Adding and changing data
    people["John"] = 40;    // Overwriting values this way is ok
    people.Add("John", 40); // Throws ArgumentException since "John" already exists

    // Iterating through contents
    foreach(KeyValuePair<string, int> person in people)
    {
        Console.WriteLine("Name={0}, Age={1}", person.Key, person.Value);
    }

    foreach(string name in people.Keys)
    {
        Console.WriteLine("Name={0}", name);
    }

    foreach(int age in people.Values)
    {
        Console.WriteLine("Age={0}", age);
    }
   
# Clé en double lors de l'utilisation de l'initialisation de la collection
    var people = new Dictionary<string, int>
    {
        { "John", 30 }, {"Mary", 35}, {"Jack", 40}, {"Jack", 40}
    }; // throws ArgumentException since "Jack" already exists


## Ensemble trié<T>


## T[ ] (Tableau de T)


## Liste<T>
`List<T>` est une liste d'un type donné. Les éléments peuvent être ajoutés, insérés, supprimés et adressés par index.
    
    using System.Collections.Generic;
    
    var list = new List<int>() { 1, 2, 3, 4, 5 };
    list.Add(6);
    Console.WriteLine(list.Count); // 6
    list.RemoveAt(3);
    Console.WriteLine(list.Count); // 5
    Console.WriteLine(list[3]);    // 5

`List<T>` peut être considéré comme un tableau que vous pouvez redimensionner. L'énumération de la collection dans l'ordre est rapide, tout comme l'accès aux éléments individuels via leur index. Pour accéder aux éléments en fonction d'un aspect de leur valeur ou d'une autre clé, un `Dictionary<T>` fournira une recherche plus rapide.


## Pile<T>


##Listeliée<T>
    // initialize a LinkedList of integers
    LinkedList list = new LinkedList<int>();

    // add some numbers to our list.
    list.AddLast(3);
    list.AddLast(5);
    list.AddLast(8);

    // the list currently is 3, 5, 8

    list.AddFirst(2);
    // the list now is 2, 3, 5, 8

    list.RemoveFirst();
    // the list is now 3, 5, 8

    list.RemoveLast();
    // the list is now 3, 5

Notez que `LinkedList<T>` représente la liste _doublement_ liée. Donc, c'est simplement une collection de nœuds et chaque nœud contient un élément de type `T`. Chaque nœud est lié au nœud précédent et au nœud suivant.


   



## File d'attente


