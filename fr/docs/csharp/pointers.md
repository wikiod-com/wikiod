---
title: "Pointeurs"
slug: "pointeurs"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

# Pointeurs et `unsafe`

En raison de leur nature, les pointeurs produisent du code invérifiable. Ainsi, l'utilisation de n'importe quel type de pointeur nécessite un contexte "non sécurisé".

Le type `System.IntPtr` est un wrapper sûr autour d'un `void*`. Il est conçu comme une alternative plus pratique à `void*` lorsqu'un contexte non sécurisé n'est pas autrement requis pour effectuer la tâche à accomplir.

# Comportement indéfini

Comme en C et C++, une utilisation incorrecte des pointeurs peut invoquer un comportement indéfini, avec des effets secondaires possibles comme la corruption de la mémoire et l'exécution de code involontaire. En raison de la nature invérifiable de la plupart des opérations sur les pointeurs, l'utilisation correcte des pointeurs relève entièrement de la responsabilité du programmeur.

# Types prenant en charge les pointeurs

Contrairement à C et C++, tous les types C# n'ont pas de types de pointeurs correspondants. Un type "T" peut avoir un type de pointeur correspondant si les deux critères suivants s'appliquent :

- 'T' est un type struct ou un type pointeur.
- 'T' contient uniquement les membres qui satisfont ces deux critères de manière récursive.

## Pointeurs pour l'accès au tableau
Cet exemple montre comment les pointeurs peuvent être utilisés pour un accès de type C aux tableaux C#.

    unsafe
    {
        var buffer = new int[1024];
        fixed (int* p = &buffer[0])
        {
            for (var i = 0; i < buffer.Length; i++)
            {
                *(p + i) = i;
            }
        }
    }

Le mot-clé `unsafe` est requis car l'accès au pointeur n'émettra aucune vérification des limites qui sont normalement émises lors de l'accès aux tableaux C # de la manière habituelle.

Le mot clé `fixed` indique au compilateur C # d'émettre des instructions pour épingler l'objet de manière sécurisée contre les exceptions. L'épinglage est nécessaire pour garantir que le ramasse-miettes ne déplacera pas le tableau en mémoire, car cela invaliderait tout pointeur pointant dans le tableau.

## Arithmétique des pointeurs
L'addition et la soustraction dans les pointeurs fonctionnent différemment des entiers. Lorsqu'un pointeur est incrémenté ou décrémenté, l'adresse vers laquelle il pointe est augmentée ou diminuée de la taille du type référent.

Par exemple, le type `int` (alias pour `System.Int32`) a une taille de 4. Si un `int` peut être stocké à l'adresse 0, le `int` suivant peut être stocké à l'adresse 4, et ainsi de suite . Dans du code:

    var ptr = (int*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr)); // prints 0
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 4
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 8

De même, le type `long` (alias pour `System.Int64`) a une taille de 8. Si un `long` peut être stocké à l'adresse 0, le `long` suivant peut être stocké à l'adresse 8, et ainsi de suite. Dans du code:

    var ptr = (long*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr)); // prints 0
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 8
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 16

Le type `void` est spécial et les pointeurs `void` sont également spéciaux et ils sont utilisés comme pointeurs fourre-tout lorsque le type n'est pas connu ou n'a pas d'importance. En raison de leur nature indépendante de la taille, les pointeurs "void" ne peuvent pas être incrémentés ou décrémentés :

    var ptr = (void*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr));
    ptr++; // compile-time error
    Console.WriteLine(new IntPtr(ptr));
    ptr++; // compile-time error
    Console.WriteLine(new IntPtr(ptr));

## L'astérisque fait partie du type
En C et C++, l'astérisque dans la déclaration d'une variable pointeur fait *partie de l'expression* déclarée. En C#, l'astérisque dans la déclaration fait *partie du type*.

En C, C++ et C#, l'extrait de code suivant déclare un pointeur "int" :

    int* a;

En C et C++, l'extrait de code suivant déclare un pointeur "int" et une variable "int". En C#, il déclare deux pointeurs `int` :

    int* a, b; 

En C et C++, l'extrait de code suivant déclare deux pointeurs "int". En C#, c'est invalide :

    int *a, *b;

## annuler*
C# hérite de C et C++ l'utilisation de `void*` comme pointeur indépendant du type et de la taille.

    void* ptr;

N'importe quel type de pointeur peut être assigné à `void*` en utilisant une conversion implicite :

    int* p1 = (int*)IntPtr.Zero;
    void* ptr = p1;

L'inverse nécessite une conversion explicite :

    int* p1 = (int*)IntPtr.Zero;
    void* ptr = p1;
    int* p2 = (int*)ptr;

## Accès membre en utilisant ->
C# hérite de C et C++ l'utilisation du symbole `->` comme moyen d'accéder aux membres d'une instance via un pointeur typé.

Considérez la structure suivante :

    struct Vector2
    {
        public int X;
        public int Y;
    }

Voici un exemple d'utilisation de `->` pour accéder à ses membres :

    Vector2 v;
    v.X = 5;
    v.Y = 10;

    Vector2* ptr = &v;
    int x = ptr->X;
    int y = ptr->Y;
    string s = ptr->ToString();

    Console.WriteLine(x); // prints 5
    Console.WriteLine(y); // prints 10
    Console.WriteLine(s); // prints Vector2

## Pointeurs génériques
Les critères qu'un type doit satisfaire pour supporter les pointeurs (voir *Remarques*) ne peuvent pas être exprimés en termes de contraintes génériques. Par conséquent, toute tentative de déclaration d'un pointeur vers un type fourni via un paramètre de type générique échouera.

    void P<T>(T obj) 
        where T : struct
    {
        T* ptr = &obj; // compile-time error
    }


