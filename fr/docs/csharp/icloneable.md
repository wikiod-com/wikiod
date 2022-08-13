---
title: "IClonable"
slug: "iclonable"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Syntaxe
- objet ICloneable.Clone() { return Clone(); } // Implémentation privée de la méthode d'interface qui utilise notre fonction publique Clone() personnalisée.
- public Foo Clone() { return new Foo(this); } // La méthode de clonage public doit utiliser la logique du constructeur de copie.

Le `CLR` nécessite une définition de méthode `object Clone()` qui n'est pas sécurisée. Il est courant de remplacer ce comportement et de définir une méthode de type sécurisée qui renvoie une copie de la classe contenante.

C'est à l'auteur de décider si le clonage signifie uniquement une copie superficielle ou une copie complète. Pour les structures immuables contenant des références, il est recommandé de faire une copie profonde. Pour les classes étant elles-mêmes des références, il est probablement bon d'implémenter une copie superficielle.

<sub>REMARQUE : En `C#`, une méthode d'interface peut être implémentée de manière privée avec la syntaxe indiquée ci-dessus.</sub>

## Implémenter ICloneable dans une classe
Implémentez `ICloneable` dans une classe avec une torsion. Exposez un coffre-fort de type public `Clone()` et implémentez `object Clone()` en privé.

    public class Person : ICloneable
    {
        // Contents of class
        public string Name { get; set; }
        public int Age { get; set; }
        // Constructor
        public Person(string name, int age)
        {
            this.Name=name;
            this.Age=age;
        }
        // Copy Constructor
        public Person(Person other)
        {
            this.Name=other.Name;
            this.Age=other.Age;
        }

        #region ICloneable Members
        // Type safe Clone
        public Person Clone() { return new Person(this); }
        // ICloneable implementation
        object ICloneable.Clone()
        {
            return Clone();
        }
        #endregion
    }

Plus tard, à utiliser comme suit :

    {
        Person bob=new Person("Bob", 25);
        Person bob_clone=bob.Clone();
        Debug.Assert(bob_clone.Name==bob.Name);
    
        bob.Age=56;
        Debug.Assert(bob.Age!=bob.Age);
    }

Notez que changer l'âge de `bob` ne change pas l'âge de `bob_clone`. En effet, la conception utilise le clonage au lieu d'attribuer des variables (de référence).

## Implémenter ICloneable dans une structure
L'implémentation de ICloneable pour une structure n'est généralement pas nécessaire car les structures effectuent une copie membre avec l'opérateur d'affectation `=`. Mais la conception peut nécessiter l'implémentation d'une autre interface qui hérite de `ICloneable`.

Une autre raison serait si la structure contient un type de référence (ou un tableau) qui aurait également besoin d'être copié.

    // Structs are recommended to be immutable objects
    [ImmutableObject(true)]
    public struct Person : ICloneable
    {
        // Contents of class
        public string Name { get; private set; }
        public int Age { get; private set; }
        // Constructor
        public Person(string name, int age)
        {
            this.Name=name;
            this.Age=age;
        }
        // Copy Constructor
        public Person(Person other)
        {
            // The assignment operator copies all members
            this=other;
        }

        #region ICloneable Members
        // Type safe Clone
        public Person Clone() { return new Person(this); }
        // ICloneable implementation
        object ICloneable.Clone()
        {
            return Clone();
        }
        #endregion
    }

Plus tard, à utiliser comme suit :

    static void Main(string[] args)
    {
        Person bob=new Person("Bob", 25);
        Person bob_clone=bob.Clone();
        Debug.Assert(bob_clone.Name==bob.Name);
    }




