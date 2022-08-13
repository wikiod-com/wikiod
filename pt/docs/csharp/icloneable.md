---
title: "ICloneable"
slug: "icloneable"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Sintaxe
- object ICloneable.Clone() { return Clone(); } // Implementação privada do método de interface que usa nossa função pública personalizada Clone().
- public Foo Clone() { return new Foo(this); } // O método clone público deve utilizar a lógica do construtor de cópia.

O `CLR` requer uma definição de método `object Clone()` que não é de tipo seguro. É uma prática comum substituir esse comportamento e definir um método seguro de tipo que retorne uma cópia da classe que o contém.

Cabe ao autor decidir se clonagem significa apenas cópia superficial ou cópia profunda. Para estruturas imutáveis ​​contendo referências é recomendado fazer uma cópia profunda. Para classes sendo elas próprias referências, provavelmente não há problema em implementar uma cópia superficial.

<sub>NOTA: Em `C#` um método de interface pode ser implementado de forma privada com a sintaxe mostrada acima.</sub>

## Implementando ICloneable em uma classe
Implemente `ICloneable` em uma classe com um toque diferente. Exponha um `Clone()` seguro de tipo público e implemente `object Clone()` de forma privada.

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

Mais tarde para ser usado da seguinte forma:

    {
        Person bob=new Person("Bob", 25);
        Person bob_clone=bob.Clone();
        Debug.Assert(bob_clone.Name==bob.Name);
    
        bob.Age=56;
        Debug.Assert(bob.Age!=bob.Age);
    }

Observe que alterar a idade de `bob` não altera a idade de `bob_clone`. Isso ocorre porque o design usa clonagem em vez de atribuição de variáveis ​​(referência).

## Implementando ICloneable em uma estrutura
A implementação de ICloneable para um struct geralmente não é necessária porque structs fazem uma cópia de membro com o operador de atribuição `=`. Mas o design pode exigir a implementação de outra interface que herda de `ICloneable`.

Outra razão seria se a estrutura contiver um tipo de referência (ou uma matriz) que também precisaria ser copiada.

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

Mais tarde para ser usado da seguinte forma:

    static void Main(string[] args)
    {
        Person bob=new Person("Bob", 25);
        Person bob_clone=bob.Clone();
        Debug.Assert(bob_clone.Name==bob.Name);
    }




