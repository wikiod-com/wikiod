---
title: "Opérateur d'égalité"
slug: "operateur-degalite"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Types d'égalité en c# et opérateur d'égalité
En C#, il existe deux types d'égalité différents : l'égalité de référence et l'égalité de valeur. L'égalité des valeurs est le sens communément admis de l'égalité : cela signifie que deux objets contiennent les mêmes valeurs. Par exemple, deux entiers ayant la valeur 2 ont une égalité de valeur. L'égalité de référence signifie qu'il n'y a pas deux objets à comparer. Au lieu de cela, il existe deux références d'objet, qui font toutes deux référence au même objet.

    object a = new object();
    object b = a;
    System.Object.ReferenceEquals(a, b);  //returns true

Pour les types valeur prédéfinis, l'opérateur d'égalité (==) renvoie true si les valeurs de ses opérandes sont égales, false sinon. Pour les types de référence autres que string, == renvoie true si ses deux opérandes font référence au même objet. Pour le type chaîne, == compare les valeurs des chaînes.
<!-- langage : c# -->

    // Numeric equality: True
    Console.WriteLine((2 + 2) == 4);
    
    // Reference equality: different objects, 
    // same boxed value: False.
    object s = 1;
    object t = 1;
    Console.WriteLine(s == t);
    
    // Define some strings:
    string a = "hello";
    string b = String.Copy(a);
    string c = "hello";
    
    // Compare string values of a constant and an instance: True
    Console.WriteLine(a == b);
    
    // Compare string references; 
    // a is a constant but b is an instance: False.
    Console.WriteLine((object)a == (object)b);
    
    // Compare string references, both constants 
    // have the same value, so string interning
    // points to same reference: True.
    Console.WriteLine((object)a == (object)c);

