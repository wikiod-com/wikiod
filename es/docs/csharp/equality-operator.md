---
title: "Operador de igualdad"
slug: "operador-de-igualdad"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Tipos de igualdad en c# y operador de igualdad
En C#, hay dos tipos diferentes de igualdad: igualdad de referencia e igualdad de valor. La igualdad de valores es el significado comúnmente entendido de igualdad: significa que dos objetos contienen los mismos valores. Por ejemplo, dos números enteros con el valor de 2 tienen igualdad de valor. La igualdad de referencia significa que no hay dos objetos para comparar. En cambio, hay dos referencias a objetos, las cuales se refieren al mismo objeto.

    object a = new object();
    object b = a;
    System.Object.ReferenceEquals(a, b);  //returns true

Para tipos de valores predefinidos, el operador de igualdad (==) devuelve verdadero si los valores de sus operandos son iguales, falso en caso contrario. Para tipos de referencia que no sean cadenas, == devuelve verdadero si sus dos operandos se refieren al mismo objeto. Para el tipo de cadena, == compara los valores de las cadenas.
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

