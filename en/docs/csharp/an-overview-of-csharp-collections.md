---
title: "An overview of c# collections"
slug: "an-overview-of-c-collections"
draft: false
images: []
weight: 9870
type: docs
toc: true
---

## HashSet<T>
This is a collection of unique items, with O(1) lookup.

    HashSet<int> validStoryPointValues = new HashSet<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(1)

By way of comparison, doing a `Contains` on a List<int> yields poorer performance:

    List<int> validStoryPointValues = new List<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(n)

`HashSet.Contains` uses a hash table, so that lookups are extremely fast, regardless of the number of items in the collection.

## Dictionary<TKey, TValue>
Dictionary<TKey, TValue> is a map. For a given key there can be one value in the dictionary. 

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
   
# Duplicate key when using collection initialization
    var people = new Dictionary<string, int>
    {
        { "John", 30 }, {"Mary", 35}, {"Jack", 40}, {"Jack", 40}
    }; // throws ArgumentException since "Jack" already exists


## SortedSet<T>


## T[ ] (Array of T)


## List<T>
`List<T>` is a list of a given type. Items can be added, inserted, removed and addressed by index.
    
    using System.Collections.Generic;
    
    var list = new List<int>() { 1, 2, 3, 4, 5 };
    list.Add(6);
    Console.WriteLine(list.Count); // 6
    list.RemoveAt(3);
    Console.WriteLine(list.Count); // 5
    Console.WriteLine(list[3]);    // 5

`List<T>` can be thought of as an array that you can resize. Enumerating over the collection in order is quick, as is access to individual elements via their index. To access elements based on some aspect of their value, or some other key, a `Dictionary<T>` will provide faster lookup.


## Stack<T>


## LinkedList<T>
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

Note that `LinkedList<T>` represents the _doubly_ linked list. So, it's simply collection of nodes and each node contains an element of type `T`. Each node is linked to the preceding node and the following node.


   



## Queue


