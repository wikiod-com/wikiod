---
title: "Una descripción general de las colecciones de C#"
slug: "una-descripcion-general-de-las-colecciones-de-c"
draft: false
images: []
weight: 9870
type: docs
toc: true
---

## HashSet<T>
Esta es una colección de elementos únicos, con búsqueda O(1).

    HashSet<int> validStoryPointValues = new HashSet<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(1)

A modo de comparación, hacer un 'Contains' en List<int> produce un rendimiento más bajo:

    List<int> validStoryPointValues = new List<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(n)

`HashSet.Contains` utiliza una tabla hash, por lo que las búsquedas son extremadamente rápidas, independientemente del número de elementos de la colección.

## Diccionario<ClaveT, ValorTV>
Dictionary<TKey, TValue> es un mapa. Para una clave determinada puede haber un valor en el diccionario.

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
   
# Clave duplicada al usar la inicialización de la colección
    var people = new Dictionary<string, int>
    {
        { "John", 30 }, {"Mary", 35}, {"Jack", 40}, {"Jack", 40}
    }; // throws ArgumentException since "Jack" already exists


## ConjuntoOrdenado<T>


## T[ ] (Matriz de T)


## Lista<T>
`List<T>` es una lista de un tipo determinado. Los elementos se pueden agregar, insertar, eliminar y direccionar por índice.
    
    using System.Collections.Generic;
    
    var list = new List<int>() { 1, 2, 3, 4, 5 };
    list.Add(6);
    Console.WriteLine(list.Count); // 6
    list.RemoveAt(3);
    Console.WriteLine(list.Count); // 5
    Console.WriteLine(list[3]);    // 5

`List<T>` se puede considerar como una matriz que puede cambiar de tamaño. Enumerar la colección en orden es rápido, al igual que el acceso a elementos individuales a través de su índice. Para acceder a los elementos en función de algún aspecto de su valor, o alguna otra clave, un `Diccionario<T>` proporcionará una búsqueda más rápida.


## Pila<T>


## Lista Vinculada<T>
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

Tenga en cuenta que `LinkedList<T>` representa la lista _doblemente_ enlazada. Entonces, es simplemente una colección de nodos y cada nodo contiene un elemento de tipo `T`. Cada nodo está vinculado al nodo anterior y al nodo siguiente.


   



## Cola


