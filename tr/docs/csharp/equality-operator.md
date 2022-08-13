---
title: "eşitlik operatörü"
slug: "esitlik-operatoru"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## c#'ta eşitlik türleri ve eşitlik operatörü
C#'ta iki farklı eşitlik türü vardır: referans eşitliği ve değer eşitliği. Değer eşitliği, eşitliğin yaygın olarak anlaşılan anlamıdır: iki nesnenin aynı değerleri içerdiği anlamına gelir. Örneğin, değeri 2 olan iki tamsayı değer eşitliğine sahiptir. Referans eşitliği, karşılaştırılacak iki nesne olmadığı anlamına gelir. Bunun yerine, her ikisi de aynı nesneye atıfta bulunan iki nesne başvurusu vardır.

    object a = new object();
    object b = a;
    System.Object.ReferenceEquals(a, b);  //returns true

Önceden tanımlanmış değer türleri için, eşitlik operatörü (==), işlenenlerin değerleri eşitse true, aksi takdirde false döndürür. Dize dışındaki başvuru türleri için ==, iki işleneni aynı nesneye başvuruyorsa, true değerini döndürür. Dize türü için ==, dizelerin değerlerini karşılaştırır.
<!-- dil: c# -->

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

