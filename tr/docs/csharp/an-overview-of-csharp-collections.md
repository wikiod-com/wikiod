---
title: "c# koleksiyonlarına genel bakış"
slug: "c-koleksiyonlarna-genel-baks"
draft: false
images: []
weight: 9870
type: docs
toc: true
---

## HashSet<T>
Bu, O(1) aramalı benzersiz öğelerden oluşan bir koleksiyondur.

    HashSet<int> validStoryPointValues = new HashSet<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(1)

Karşılaştırma yoluyla, <int> Listesinde 'İçerir' yapmak daha düşük performans sağlar:

    List<int> validStoryPointValues = new List<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(n)

"HashSet.Contains", koleksiyondaki öğe sayısından bağımsız olarak aramaların son derece hızlı olması için bir karma tablo kullanır.

## Sözlük<TKey, TValue>
Dictionary<TKey, TValue> bir haritadır. Belirli bir anahtar için sözlükte bir değer olabilir.

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
   
# Koleksiyon başlatmayı kullanırken anahtarı kopyala
    var people = new Dictionary<string, int>
    {
        { "John", 30 }, {"Mary", 35}, {"Jack", 40}, {"Jack", 40}
    }; // throws ArgumentException since "Jack" already exists


## SortedSet<T>


## T[ ] (T dizisi)


## Liste<T>
'List<T>', belirli bir türün listesidir. Öğeler indeks tarafından eklenebilir, eklenebilir, kaldırılabilir ve adreslenebilir.
    
    using System.Collections.Generic;
    
    var list = new List<int>() { 1, 2, 3, 4, 5 };
    list.Add(6);
    Console.WriteLine(list.Count); // 6
    list.RemoveAt(3);
    Console.WriteLine(list.Count); // 5
    Console.WriteLine(list[3]);    // 5

`List<T>` yeniden boyutlandırabileceğiniz bir dizi olarak düşünülebilir. Koleksiyon üzerinde sırayla numaralandırma, dizinleri aracılığıyla bireysel öğelere erişim gibi hızlıdır. Öğelere değerlerinin bazı yönlerine veya başka bir anahtara göre erişmek için bir 'Sözlük<T>' daha hızlı arama sağlayacaktır.


## Yığın<T>


## Bağlantılı Liste<T>
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

'LinkedList<T>' öğesinin _doubly_ bağlantılı listeyi temsil ettiğini unutmayın. Yani, bu sadece düğümlerin toplamıdır ve her düğüm 'T' tipinde bir eleman içerir. Her düğüm bir önceki düğüme ve bir sonraki düğüme bağlıdır.


   



## Sıra


