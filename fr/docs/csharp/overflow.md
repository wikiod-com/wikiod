---
title: "Débordement"
slug: "debordement"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Débordement d'entier
Il existe une capacité maximale qu'un entier peut stocker. Et lorsque vous dépassez cette limite, cela reviendra du côté négatif. Pour `int`, c'est `2147483647`

    int x = int.MaxValue;                //MaxValue is 2147483647
    x = unchecked(x + 1);                //make operation explicitly unchecked so that the example also works when the check for arithmetic overflow/underflow is enabled in the project settings 
    Console.WriteLine(x);                //Will print -2147483648
    Console.WriteLine(int.MinValue);     //Same as Min value

Pour tous les entiers hors de cette plage, utilisez l'espace de noms System.Numerics qui a le type de données
GrandEntier. Consultez le lien ci-dessous pour plus d'informations https://msdn.microsoft.com/en-us/library/system.numerics.biginteger(v=vs.110).aspx

## Débordement pendant le fonctionnement
Le débordement se produit également pendant l'opération. Dans l'exemple suivant, x est un `int`, 1 est un `int` par défaut. Donc l'addition est une addition `int`. Et le résultat sera un `int`. Et ça va déborder.

    int x = int.MaxValue;               //MaxValue is 2147483647
    long y = x + 1;                     //It will be overflown
    Console.WriteLine(y);               //Will print -2147483648
    Console.WriteLine(int.MinValue);    //Same as Min value

Vous pouvez empêcher cela en utilisant 1L. Maintenant 1 sera un "long" et l'addition sera une addition "longue"

    int x = int.MaxValue;               //MaxValue is 2147483647
    long y = x + 1L;                    //It will be OK
    Console.WriteLine(y);               //Will print 2147483648


## La commande est importante
Il y a débordement dans le code suivant

    int x = int.MaxValue;
    Console.WriteLine(x + x + 1L);  //prints -1

Alors que dans le code suivant il n'y a pas de débordement

    int x = int.MaxValue;
    Console.WriteLine(x + 1L + x);  //prints 4294967295

Cela est dû à l'ordre de gauche à droite des opérations. Dans le premier fragment de code `x + x` déborde et après cela, il devient un `long`. D'autre part `x + 1L` devient `long` et ensuite `x` est ajouté à cette valeur.


