---
title: "Uma visão geral das coleções c#"
slug: "uma-visao-geral-das-colecoes-c"
draft: false
images: []
weight: 9870
type: docs
toc: true
---

## Conjunto de Hash<T>
Esta é uma coleção de itens exclusivos, com pesquisa O(1).

    HashSet<int> validStoryPointValues = new HashSet<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(1)

A título de comparação, fazer um `Contains` em um List<int> produz um desempenho pior:

    List<int> validStoryPointValues = new List<int>() { 1, 2, 3, 5, 8, 13, 21 };
    bool containsEight = validStoryPointValues.Contains(8); // O(n)

`HashSet.Contains` usa uma tabela de hash, para que as pesquisas sejam extremamente rápidas, independentemente do número de itens na coleção.

## Dicionário<TKey, TValue>
Dicionário<TKey, TValue> é um mapa. Para uma determinada chave, pode haver um valor no dicionário.

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
   
# Chave duplicada ao usar inicialização de coleção
    var people = new Dictionary<string, int>
    {
        { "John", 30 }, {"Mary", 35}, {"Jack", 40}, {"Jack", 40}
    }; // throws ArgumentException since "Jack" already exists


## SortedSet<T>


## T[ ] (Matriz de T)


## Lista<T>
`List<T>` é uma lista de um determinado tipo. Os itens podem ser adicionados, inseridos, removidos e endereçados pelo índice.
    
    using System.Collections.Generic;
    
    var list = new List<int>() { 1, 2, 3, 4, 5 };
    list.Add(6);
    Console.WriteLine(list.Count); // 6
    list.RemoveAt(3);
    Console.WriteLine(list.Count); // 5
    Console.WriteLine(list[3]);    // 5

`List<T>` pode ser pensado como um array que você pode redimensionar. A enumeração da coleção em ordem é rápida, assim como o acesso a elementos individuais por meio de seu índice. Para acessar elementos com base em algum aspecto de seu valor, ou alguma outra chave, um `Dictionary<T>` fornecerá uma pesquisa mais rápida.


## Pilha<T>


##ListaLigada<T>
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

Observe que `LinkedList<T>` representa a lista _doubly_ vinculada. Então, é simplesmente uma coleção de nós e cada nó contém um elemento do tipo `T`. Cada nó está vinculado ao nó anterior e ao nó seguinte.


   



## Fila


