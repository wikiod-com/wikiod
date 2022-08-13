---
title: "Operador de igualdade"
slug: "operador-de-igualdade"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Tipos de igualdade em c# e operador de igualdade
Em C#, existem dois tipos diferentes de igualdade: igualdade de referência e igualdade de valor. Igualdade de valor é o significado comumente entendido de igualdade: significa que dois objetos contêm os mesmos valores. Por exemplo, dois inteiros com o valor de 2 têm igualdade de valor. Igualdade de referência significa que não há dois objetos para comparar. Em vez disso, há duas referências de objeto, ambas se referindo ao mesmo objeto.

    object a = new object();
    object b = a;
    System.Object.ReferenceEquals(a, b);  //returns true

Para tipos de valor predefinidos, o operador de igualdade (==) retorna true se os valores de seus operandos forem iguais, false caso contrário. Para tipos de referência diferentes de string, == retorna true se seus dois operandos se referirem ao mesmo objeto. Para o tipo de string, == compara os valores das strings.
<!-- idioma: c# -->

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

