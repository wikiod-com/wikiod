---
title: "ICloneable"
slug: "icloneable"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Sintaxis
- objeto ICloneable.Clone() { return Clone(); } // Implementación privada del método de interfaz que utiliza nuestra función Clone() pública personalizada.
- public Foo Clone() { return new Foo(this); } // El método de clonación pública debe utilizar la lógica del constructor de copias.

El `CLR` requiere una definición de método `object Clone()` que no es de tipo seguro. Es una práctica común anular este comportamiento y definir un método seguro de tipos que devuelva una copia de la clase contenedora.

Depende del autor decidir si la clonación significa solo una copia superficial o una copia profunda. Para estructuras inmutables que contienen referencias, se recomienda hacer una copia profunda. Para las clases que son referencias en sí mismas, probablemente esté bien implementar una copia superficial.

<sub>NOTA: En `C#`, un método de interfaz se puede implementar de forma privada con la sintaxis que se muestra arriba.</sub>

## Implementando ICloneable en una clase
Implemente `ICloneable` en una clase con un giro. Exponga un `Clone()` seguro de tipo público e implemente `object Clone()` de forma privada.

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

Más tarde se utilizará de la siguiente manera:

    {
        Person bob=new Person("Bob", 25);
        Person bob_clone=bob.Clone();
        Debug.Assert(bob_clone.Name==bob.Name);
    
        bob.Age=56;
        Debug.Assert(bob.Age!=bob.Age);
    }

Tenga en cuenta que cambiar la edad de `bob` no cambia la edad de `bob_clone`. Esto se debe a que el diseño utiliza la clonación en lugar de la asignación de variables (de referencia).

## Implementando ICloneable en una estructura
La implementación de ICloneable para una estructura generalmente no es necesaria porque las estructuras hacen una copia de los miembros con el operador de asignación `=`. Pero el diseño puede requerir la implementación de otra interfaz que herede de `ICloneable`.

Otra razón sería si la estructura contiene un tipo de referencia (o una matriz) que también necesitaría copiarse.

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

Más tarde se utilizará de la siguiente manera:

    static void Main(string[] args)
    {
        Person bob=new Person("Bob", 25);
        Person bob_clone=bob.Clone();
        Debug.Assert(bob_clone.Name==bob.Name);
    }




