---
title: "Pointeurs et code dangereux"
slug: "pointeurs-et-code-dangereux"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Introduction au code non sécurisé
C # permet d'utiliser des variables de pointeur dans une fonction de bloc de code lorsqu'il est marqué par le modificateur "unsafe". Le code non sécurisé ou le code non géré est un bloc de code qui utilise une variable de pointeur.

Un pointeur est une variable dont la valeur est l'adresse d'une autre variable, c'est-à-dire l'adresse directe de l'emplacement mémoire. comme pour toute variable ou constante, vous devez déclarer un pointeur avant de pouvoir l'utiliser pour stocker une adresse de variable.

La forme générale d'une déclaration de pointeur est :

    type *var-name;

Voici les déclarations de pointeur valides :

    int    *ip;    /* pointer to an integer */
    double *dp;    /* pointer to a double */
    float  *fp;    /* pointer to a float */
    char   *ch     /* pointer to a character */
L'exemple suivant illustre l'utilisation de pointeurs en C#, en utilisant le modificateur unsafe :

    using System;
    namespace UnsafeCodeApplication
    {
       class Program
       {
          static unsafe void Main(string[] args)
          {
             int var = 20;
             int* p = &var;
             Console.WriteLine("Data is: {0} ",  var);
             Console.WriteLine("Address is: {0}",  (int)p);
             Console.ReadKey();
          }
       }
    }
Lorsque le code ci-dessus a été compilé et exécuté, il produit le résultat suivant :

    Data is: 20
    Address is: 99215364

Au lieu de déclarer une méthode entière comme non sécurisée, vous pouvez également déclarer une partie du code comme non sécurisée :

    // safe code
    unsafe
    {
        // you can use pointers here
    }
    // safe code

## Accéder aux éléments du tableau à l'aide d'un pointeur
En C #, un nom de tableau et un pointeur vers un type de données identique aux données du tableau ne sont pas le même type de variable. Par exemple, `int *p` et `int[] p` ne sont pas du même type. Vous pouvez incrémenter la variable de pointeur `p` car elle n'est pas fixe en mémoire mais une adresse de tableau est fixe en mémoire, et vous ne pouvez pas l'incrémenter.

Par conséquent, si vous avez besoin d'accéder à un tableau de données à l'aide d'une variable de pointeur, comme nous le faisons traditionnellement en C ou C++, vous devez fixer le pointeur à l'aide du mot-clé fixed.

L'exemple suivant le démontre :

    using System;
    namespace UnsafeCodeApplication
    {
       class TestPointer
       {
          public unsafe static void Main()
          {
             int[]  list = {10, 100, 200};
             fixed(int *ptr = list)
             
             /* let us have array address in pointer */
             for ( int i = 0; i < 3; i++)
             {
                Console.WriteLine("Address of list[{0}]={1}",i,(int)(ptr + i));
                Console.WriteLine("Value of list[{0}]={1}", i, *(ptr + i));
             }
             
             Console.ReadKey();
          }
       }
    }

Lorsque le code ci-dessus a été compilé et exécuté, il produit le résultat suivant :

    Address of list[0] = 31627168
    Value of list[0] = 10
    Address of list[1] = 31627172
    Value of list[1] = 100
    Address of list[2] = 31627176
    Value of list[2] = 200

## Compilation de code dangereux
Pour compiler du code non sécurisé, vous devez spécifier le commutateur de ligne de commande `/unsafe` avec le compilateur de ligne de commande.

Par exemple, pour compiler un programme nommé prog1.cs contenant du code non sécurisé, à partir de la ligne de commande, donnez la commande :

    csc /unsafe prog1.cs

Si vous utilisez Visual Studio IDE, vous devez activer l'utilisation de code non sécurisé dans les propriétés du projet.

[![entrez la description de l'image ici][1]][1]

Pour faire ça:

- Ouvrez les propriétés du projet en double-cliquant sur le nœud des propriétés dans le
Explorateur de solution.
- Cliquez sur l'onglet Construire.
- Sélectionnez l'option "Autoriser
code dangereux"


[1] : https://i.stack.imgur.com/2aPFY.png

## Récupération de la valeur de données à l'aide d'un pointeur
Vous pouvez récupérer les données stockées à l'emplacement référencé par la variable de pointeur, à l'aide de la méthode ToString(). L'exemple suivant le démontre :

    using System;
    namespace UnsafeCodeApplication
    {
       class Program
       {
          public static void Main()
          {
             unsafe
             {
                int var = 20;
                int* p = &var;
                Console.WriteLine("Data is: {0} " , var);
                Console.WriteLine("Data is: {0} " , p->ToString());
                Console.WriteLine("Address is: {0} " , (int)p);
             }
             
             Console.ReadKey();
          }
       }
    }
Lorsque le code ci-dessus a été compilé et exécuté, il produit le résultat suivant :

    Data is: 20
    Data is: 20
    Address is: 77128984

## Passer des pointeurs comme paramètres aux méthodes
Vous pouvez passer une variable de pointeur à une méthode en tant que paramètre. L'exemple suivant illustre cela :

    using System;
    namespace UnsafeCodeApplication
    {
       class TestPointer
       {
          public unsafe void swap(int* p, int *q)
          {
             int temp = *p;
             *p = *q;
             *q = temp;
          }
          
          public unsafe static void Main()
          {
             TestPointer p = new TestPointer();
             int var1 = 10;
             int var2 = 20;
             int* x = &var1;
             int* y = &var2;
             
             Console.WriteLine("Before Swap: var1:{0}, var2: {1}", var1, var2);
             p.swap(x, y);
    
             Console.WriteLine("After Swap: var1:{0}, var2: {1}", var1, var2);
             Console.ReadKey();
          }
       }
    }

Lorsque le code ci-dessus est compilé et exécuté, il produit le résultat suivant :

    Before Swap: var1: 10, var2: 20
    After Swap: var1: 20, var2: 10

