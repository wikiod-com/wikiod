---
title: "Structures"
slug: "structures"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Contrairement aux classes, un `struct` est un type valeur et est créé sur la pile locale et non sur le tas géré, *par défaut*. Cela signifie qu'une fois que la pile spécifique sort de la portée, le `struct` est désalloué. Les types de référence contenus de `struct` désalloués sont également balayés, une fois que le GC détermine qu'ils ne sont plus référencés par le `struct`.

Les `struct` ne peuvent pas hériter et ne peuvent pas être des bases d'héritage, ils sont implicitement scellés et ne peuvent pas non plus inclure de membres `protected`. Cependant, un `struct` peut implémenter une interface, comme le font les classes.

## Déclarer une structure
    public struct Vector 
    {
        public int X;
        public int Y;
        public int Z;
    }

    public struct Point
    {
        public decimal x, y;
        
        public Point(decimal pointX, decimal pointY)
        {
            x = pointX;
            y = pointY;
        }
    }

- Les champs d'instance `struct` peuvent être définis via un constructeur paramétré ou individuellement après la construction de `struct`.
- Les membres privés ne peuvent être initialisés que par le constructeur.
- `struct` définit un type scellé qui hérite implicitement de System.ValueType.
- Les structures ne peuvent hériter d'aucun autre type, mais elles peuvent implémenter des interfaces.
- Les structures sont copiées lors de l'affectation, ce qui signifie que toutes les données sont copiées dans la nouvelle instance et que les modifications apportées à l'une d'entre elles ne sont pas reflétées par l'autre.
- Une structure ne peut pas être `null`, bien qu'elle *peut* être utilisée comme type nullable :

       Vector v1 = null; //illegal
       Vector? v2 = null; //OK
       Nullable<Vector> v3 = null // OK

- Les structures peuvent être instanciées avec ou sans l'opérateur `new`.

       //Both of these are acceptable
       Vector v1 = new Vector();
       v1.X = 1;
       v1.Y = 2;
       v1.Z = 3;
    
       Vector v2;
       v2.X = 1;
       v2.Y = 2;
       v2.Z = 3;

    However, the `new` operator must be used in order to use an initializer:

       Vector v1 = new MyStruct { X=1, Y=2, Z=3 }; // OK
       Vector v2 { X=1, Y=2, Z=3 }; // illegal

Une structure peut déclarer tout ce qu'une classe peut déclarer, à quelques exceptions près :
- Une structure ne peut pas déclarer un constructeur sans paramètre. Les champs d'instance `struct` peuvent être définis via un constructeur paramétré ou individuellement après la construction de `struct`. Les membres privés ne peuvent être initialisés que par le constructeur.
- Une structure ne peut pas déclarer des membres comme protégés, car elle est implicitement scellée.
- Les champs struct ne peuvent être initialisés que s'ils sont const ou static.

## Utilisation de la structure
**Avec constructeur :**

    Vector v1 = new Vector();
    v1.X = 1;
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    // Output X=1,Y=2,Z=3

    Vector v1 = new Vector();
    //v1.X is not assigned
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    // Output X=0,Y=2,Z=3

    Point point1 = new Point();
    point1.x = 0.5;
    point1.y = 0.6;
    
    Point point2 = new Point(0.5, 0.6);

**Sans constructeur :**

    Vector v1;
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    //Output ERROR "Use of possibly unassigned field 'X'

    Vector v1;
    v1.X = 1;
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    // Output X=1,Y=2,Z=3

    Point point3;
    point3.x = 0.5;
    point3.y = 0.6;

Si nous utilisons une structure avec son constructeur, nous n'allons pas avoir de problèmes avec le champ non affecté (chaque champ non affecté a une valeur nulle).

Contrairement aux classes, une structure n'a pas besoin d'être construite, c'est-à-dire qu'il n'est pas nécessaire d'utiliser le mot-clé new, à moins que vous n'ayez besoin d'appeler l'un des constructeurs. Une structure ne nécessite pas le nouveau mot-clé car il s'agit d'un type valeur et ne peut donc pas être nul.

## Interface d'implémentation de structure
    public interface IShape
    {
        decimal Area();
    }
    
    public struct Rectangle : IShape
    {
        public decimal Length { get; set; }
        public decimal Width { get; set; }
    
        public decimal Area()
        {
            return Length * Width;
        }
    }

## Les structures sont copiées lors de l'affectation
Les structures Sinse sont des types de valeur, toutes les données sont _copiées_ lors de l'affectation, et toute modification apportée à la nouvelle copie ne modifie pas les données de la copie d'origine. L'extrait de code ci-dessous montre que `p1` est _copié_ vers `p2` et que les modifications apportées à `p1` n'affectent pas l'instance `p2`.

    var p1 = new Point {
        x = 1,
        y = 2
    };
    
    Console.WriteLine($"{p1.x} {p1.y}"); // 1 2
    
    var p2 = p1;
    Console.WriteLine($"{p2.x} {p2.y}"); // Same output: 1 2
    
    p1.x = 3;
    Console.WriteLine($"{p1.x} {p1.y}"); // 3 2
    Console.WriteLine($"{p2.x} {p2.y}"); // p2 remain the same: 1 2

