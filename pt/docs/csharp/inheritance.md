---
title: "Herança"
slug: "heranca"
draft: false
images: []
weight: 9785
type: docs
toc: true
---

## Sintaxe
- class DerivedClass : BaseClass
- class DerivedClass : BaseClass, IExampleInterface
- class DerivedClass : BaseClass, IExampleInterface, IAnotherInterface

As classes podem herdar diretamente de apenas uma classe, mas (em vez disso ou ao mesmo tempo) podem implementar uma ou mais interfaces.

As estruturas podem implementar interfaces, mas não podem herdar explicitamente de nenhum tipo. Eles herdam implicitamente de `System.ValueType`, que por sua vez herda diretamente de `System.Object`.

Classes estáticas [não podem][1] implementam interfaces.


[1]: http://stackoverflow.com/a/259079

## Herança. Sequência de chamadas dos construtores
Considere que temos uma classe `Animal` que tem uma classe filha `Dog`

    class Animal
    {
        public Animal()
        {
            Console.WriteLine("In Animal's constructor");
        }
    }
    
    class Dog : Animal
    {
        public Dog()
        {
            Console.WriteLine("In Dog's constructor");
        }
    }

Por padrão, toda classe herda implicitamente a classe `Object`.

Este é o mesmo que o código acima.

    class Animal : Object
    {
        public Animal()
        {
            Console.WriteLine("In Animal's constructor");
        }
    }

Ao criar uma instância da classe `Dog`, o construtor padrão **das classes base (sem parâmetros) será chamado se não houver uma chamada explícita para outro construtor na classe pai**. No nosso caso, primeiro será chamado de construtor `Object's`, depois `Animal's` e no final construtor `Dog's`.

    public class Program
    {
        public static void Main()
        {
            Dog dog = new Dog();
        }
    }

A saída será
>No construtor do Animal
>No construtor do Dog

[Ver demonstração][1]

**Chame o construtor do pai explicitamente.**

Nos exemplos acima, nosso construtor de classe `Dog` chama o construtor **default** da classe `Animal`. Se desejar, você pode especificar qual construtor deve ser chamado: é possível chamar qualquer construtor definido na classe pai.

Considere que temos essas duas classes.

    class Animal
    {
        protected string name;
    
        public Animal()
        {
            Console.WriteLine("Animal's default constructor");
        }    
    
        public Animal(string name)
        {
            this.name = name;
            Console.WriteLine("Animal's constructor with 1 parameter");
            Console.WriteLine(this.name);
        }
    }

    class Dog : Animal
    {
        public Dog() : base()
        {
            Console.WriteLine("Dog's default constructor");
        }  
    
        public Dog(string name) : base(name)
        {
            Console.WriteLine("Dog's constructor with 1 parameter");
            Console.WriteLine(this.name);
        }
    }

**O que está acontecendo aqui?**

Temos 2 construtores em cada classe.

**O que significa `base`?**

`base` é uma referência à classe pai. No nosso caso, quando criamos uma instância da classe `Dog` como esta

    Dog dog = new Dog();

O tempo de execução primeiro chama o `Dog()`, que é o construtor sem parâmetros. Mas seu corpo não funciona imediatamente. Após os parênteses do construtor, temos uma chamada: `base()`, o que significa que quando chamamos o construtor padrão `Dog`, ele por sua vez chamará o construtor **default** do pai. Depois que o construtor do pai for executado, ele retornará e, finalmente, executará o corpo do construtor `Dog()`.

Então a saída será assim:
>Construtor padrão do animal
>Construtor padrão do cão

[Ver demonstração][2]

**E se chamarmos o construtor `Dog's` com um parâmetro?**

    Dog dog = new Dog("Rex");

Você sabe que os membros da classe pai que não são privados são herdados pela classe filha, o que significa que `Dog` também terá o campo `name`.
Neste caso passamos um argumento para o nosso construtor. Ele, por sua vez, passa o argumento para o **construtor com um parâmetro** da classe pai, que inicializa o campo `name`.

A saída será

<!-- idioma: lang-none -->
    Animal's constructor with 1 parameter
    Rex
    Dog's constructor with 1 parameter
    Rex

**Resumo:**

Toda criação de objeto começa na classe base. Na herança, as classes que estão na hierarquia são encadeadas. Como todas as classes derivam de `Object`, o primeiro construtor a ser chamado quando qualquer objeto é criado é o construtor da classe `Object`; Então o próximo construtor na cadeia é chamado e somente depois que todos eles são chamados o objeto é criado

**palavra-chave básica**

1) A palavra-chave base é usada para acessar membros da classe base de dentro de uma classe derivada:
2) Chame um método na classe base que foi substituído por outro método.
Especifique qual construtor de classe base deve ser chamado ao criar instâncias da classe derivada.


[1]: https://dotnetfiddle.net/uOL8cE
[2]: https://dotnetfiddle.net/eRKEjT

## Herdando de uma classe base
Para evitar a duplicação de código, defina métodos e atributos comuns em uma classe geral como base:

    public class Animal 
    {
        public string Name { get; set; }
        // Methods and attributes common to all animals
        public void Eat(Object dinner)
        {
            // ...
        }
        public void Stare()
        {
            // ...
        }
        public void Roll()
        {
            // ...
        }
    }
  
Agora que você tem uma classe que representa `Animal` em geral, você pode definir uma classe que descreva as peculiaridades de animais específicos:
  
    public class Cat : Animal
    {
        public Cat() 
        {
            Name = "Cat";
        }
        // Methods for scratching furniture and ignoring owner
        public void Scratch(Object furniture)
        {
            // ...
        }
    }

A classe Cat obtém acesso não apenas aos métodos descritos em sua definição explicitamente, mas também a todos os métodos definidos na classe base geral `Animal`. Qualquer Animal (fosse ou não um Gato) poderia Comer, Encarar ou Rolar. Um Animal não seria capaz de Arranhar, a menos que também fosse um Gato. Você poderia então definir outras classes descrevendo outros animais. (Como Gopher com um método para destruir jardins de flores e Sloth sem nenhum método extra.)

## Herdando de uma classe e implementando uma interface
    public class Animal 
    {
        public string Name { get; set; }
    }

    public interface INoiseMaker
    {
        string MakeNoise();
    }

    //Note that in C#, the base class name must come before the interface names
    public class Cat : Animal, INoiseMaker
    {
        public Cat() 
        {
            Name = "Cat";
        }

        public string MakeNoise()
        {
            return "Nyan";
        }
    }



## Herdando de uma classe e implementando múltiplas interfaces
    public class LivingBeing
    {
        string Name { get; set; }
    }
    
    public interface IAnimal 
    {
        bool HasHair { get; set; }
    }
    
    public interface INoiseMaker
    {
        string MakeNoise();
    }
    
    //Note that in C#, the base class name must come before the interface names
    public class Cat : LivingBeing, IAnimal, INoiseMaker
    {
        public Cat() 
        {
            Name = "Cat";
            HasHair = true;
        }
    
        public bool HasHair { get; set; }
    
        public string Name { get; set; }

        public string MakeNoise()
        {
            return "Nyan";
        }
    }

## Testando e navegando na herança
    interface BaseInterface {}
    class BaseClass : BaseInterface {}

    interface DerivedInterface {}
    class DerivedClass : BaseClass, DerivedInterface {}
    
    var baseInterfaceType = typeof(BaseInterface);
    var derivedInterfaceType = typeof(DerivedInterface);
    var baseType = typeof(BaseClass);
    var derivedType = typeof(DerivedClass);
    
    var baseInstance = new BaseClass();
    var derivedInstance = new DerivedClass();  
    
    Console.WriteLine(derivedInstance is DerivedClass); //True
    Console.WriteLine(derivedInstance is DerivedInterface); //True
    Console.WriteLine(derivedInstance is BaseClass); //True
    Console.WriteLine(derivedInstance is BaseInterface); //True
    Console.WriteLine(derivedInstance is object); //True
    
    Console.WriteLine(derivedType.BaseType.Name);  //BaseClass
    Console.WriteLine(baseType.BaseType.Name);  //Object
    Console.WriteLine(typeof(object).BaseType);  //null
    
    Console.WriteLine(baseType.IsInstanceOfType(derivedInstance));  //True
    Console.WriteLine(derivedType.IsInstanceOfType(baseInstance));  //False

    Console.WriteLine(
        string.Join(",", 
        derivedType.GetInterfaces().Select(t => t.Name).ToArray()));
    //BaseInterface,DerivedInterface
        
    Console.WriteLine(baseInterfaceType.IsAssignableFrom(derivedType)); //True
    Console.WriteLine(derivedInterfaceType.IsAssignableFrom(derivedType)); //True
    Console.WriteLine(derivedInterfaceType.IsAssignableFrom(baseType)); //False

## Estendendo uma classe base abstrata
Ao contrário das interfaces, que podem ser descritas como contratos para implementação, as classes abstratas atuam como contratos para extensão.

Uma classe abstrata não pode ser instanciada, ela deve ser estendida e a classe resultante (ou classe derivada) pode então ser instanciada.

Classes abstratas são usadas para fornecer implementações genéricas

    public abstract class Car
    {
        public void HonkHorn() {
            // Implementation of horn being honked
        }
    }

    public class Mustang : Car
    {
        // Simply by extending the abstract class Car, the Mustang can HonkHorn()
        // If Car were an interface, the HonkHorn method would need to be included
        // in every class that implemented it.
    }

O exemplo acima mostra como qualquer classe que estende Car receberá automaticamente o método HonkHorn com a implementação. Isso significa que qualquer desenvolvedor que crie um novo carro não precisará se preocupar em como ele irá buzinar.

## Construtores em uma subclasse
Quando você cria uma subclasse de uma classe base, você pode construir a classe base usando `: base` após os parâmetros do construtor da subclasse.

    class Instrument
    {
        string type;
        bool clean;
    
        public Instrument (string type, bool clean)
        {
            this.type = type;
            this.clean = clean;
        }
    }
    
    class Trumpet : Instrument
    {
        bool oiled;
    
        public Trumpet(string type, bool clean, bool oiled) : base(type, clean)
        {
            this.oiled = oiled;
        }
    }

## Antipadrões de herança
# Herança imprópria

Vamos dizer que existem 2 classes classe `Foo` e `Bar`. `Foo` tem dois recursos `Do1` e `Do2`. `Bar` precisa usar `Do1` de `Foo`, mas não precisa de `Do2` ou precisa de recursos equivalentes a `Do2`, mas faz algo completamente diferente.

**Mal caminho**: torne `Do2()` em `Foo` virtual e substitua-o em `Bar` ou apenas `throw Exception` em `Bar` para `Do2()`

    public class Bar : Foo
    {
        public override void Do2()
        {
            //Does something completely different that you would expect Foo to do
            //or simply throws new Exception 
        }
    }

**Bom caminho**

Retire `Do1()` de `Foo` e coloque-o na nova classe `Baz`, então herde `Foo` e `Bar` de `Baz` e implemente `Do2()` separadamente

    public class Baz
    {
        public void Do1()
        {
            // magic
        }
    }

    public class Foo : Baz
    {
        public void Do2()
        {
            // foo way
        }
    }

    public class Bar : Baz
    {
        public void Do2()
        {
            // bar way or not have Do2 at all
        }
    }

Agora porque o primeiro exemplo é ruim e o segundo é bom: Quando o desenvolvedor nr2 tem que fazer uma mudança em `Foo`, as chances são de que ele irá quebrar a implementação de `Bar` porque `Bar` agora é inseparável de `Foo`. Ao fazer isso pelo último exemplo, `Foo` e `Bar` foram movidos para `Baz` e eles não afetam um ao outro (como o should't).


## Métodos de herança
Existem várias maneiras pelas quais os métodos podem ser herdados

    public abstract class Car
    {
        public void HonkHorn() {
            // Implementation of horn being honked
        }

        // virtual methods CAN be overridden in derived classes
        public virtual void ChangeGear() {
            // Implementation of gears being changed
        }

        // abstract methods MUST be overridden in derived classes
        public abstract void Accelerate();
    }

    public class Mustang : Car
    {
        // Before any code is added to the Mustang class, it already contains 
        // implementations of HonkHorn and ChangeGear.

        // In order to compile, it must be given an implementation of Accelerate,
        // this is done using the override keyword
        public override void Accelerate() {
            // Implementation of Mustang accelerating
        }

        // If the Mustang changes gears differently to the implementation in Car
        // this can be overridden using the same override keyword as above
        public override void ChangeGear() {
            // Implementation of Mustang changing gears
        }
    }

## Classe base com especificação de tipo recursivo
Definição única de uma classe base genérica com especificador de tipo recursivo. Cada nó tem um pai e vários filhos.

    /// <summary>
    /// Generic base class for a tree structure
    /// </summary>
    /// <typeparam name="T">The node type of the tree</typeparam>
    public abstract class Tree<T> where T : Tree<T>
    {
        /// <summary>
        /// Constructor sets the parent node and adds this node to the parent's child nodes
        /// </summary>
        /// <param name="parent">The parent node or null if a root</param>
        protected Tree(T parent)
        {
            this.Parent=parent;
            this.Children=new List<T>();
            if(parent!=null)
            {
                parent.Children.Add(this as T);
            }
        }
        public T Parent { get; private set; }
        public List<T> Children { get; private set; }
        public bool IsRoot { get { return Parent==null; } }
        public bool IsLeaf { get { return Children.Count==0; } }
        /// <summary>
        /// Returns the number of hops to the root object
        /// </summary>
        public int Level { get { return IsRoot ? 0 : Parent.Level+1; } }
    }

O acima pode ser reutilizado sempre que uma hierarquia de árvore de objetos precisar ser definida. O objeto nó na árvore tem que herdar da classe base com

    public class MyNode : Tree<MyNode>
    {
        // stuff
    }

cada classe de nó sabe onde está na hierarquia, qual é o objeto pai e quais são os objetos filhos. Vários tipos internos usam uma estrutura de árvore, como `Control` ou `XmlElement` e o `Tree<T>` acima pode ser usado como uma classe base de _any_ tipo em seu código.


----------


Por exemplo, para criar uma hierarquia de peças em que o peso total é calculado a partir do peso de _todos_ os filhos, faça o seguinte:

    public class Part : Tree<Part>
    {
        public static readonly Part Empty = new Part(null) { Weight=0 };
        public Part(Part parent) : base(parent) { }
        public Part Add(float weight)
        {
            return new Part(this) { Weight=weight };
        }
        public float Weight { get; set; }

        public float TotalWeight { get { return Weight+Children.Sum((part) => part.TotalWeight); } }
    }

para ser usado como

    // [Q:2.5] -- [P:4.2] -- [R:0.4]
    //    \
    //      - [Z:0.8]
    var Q = Part.Empty.Add(2.5f);
    var P = Q.Add(4.2f);
    var R = P.Add(0.4f);
    var Z = Q.Add(0.9f);
    
    // 2.5+(4.2+0.4)+0.9 = 8.0
    float weight = Q.TotalWeight;


----------


Outro exemplo seria na definição de quadros de coordenadas relativas. Neste caso, a verdadeira posição do quadro de coordenadas depende das posições de _todos_ os quadros de coordenadas pai.

    public class RelativeCoordinate : Tree<RelativeCoordinate>
    {
        public static readonly RelativeCoordinate Start = new RelativeCoordinate(null, PointF.Empty) { };
        public RelativeCoordinate(RelativeCoordinate parent, PointF local_position)
            : base(parent)
        {
            this.LocalPosition=local_position;
        }
        public PointF LocalPosition { get; set; }
        public PointF GlobalPosition
        {
            get
            {
                if(IsRoot) return LocalPosition;
                var parent_pos = Parent.GlobalPosition;
                return new PointF(parent_pos.X+LocalPosition.X, parent_pos.Y+LocalPosition.Y);
            }
        }
        public float TotalDistance
        {
            get
            {
                float dist = (float)Math.Sqrt(LocalPosition.X*LocalPosition.X+LocalPosition.Y*LocalPosition.Y);
                return IsRoot ? dist : Parent.TotalDistance+dist;
            }
        }
        public RelativeCoordinate Add(PointF local_position)
        {
            return new RelativeCoordinate(this, local_position);
        }
        public RelativeCoordinate Add(float x, float y)
        {
            return Add(new PointF(x, y));
        }
    }

para ser usado como

    // Define the following coordinate system hierarchy
    //
    // o--> [A1] --+--> [B1] -----> [C1]
    //             |     
    //             +--> [B2] --+--> [C2]
    //                         |
    //                         +--> [C3]
    
    var A1 = RelativeCoordinate.Start;
    var B1 = A1.Add(100, 20);
    var B2 = A1.Add(160, 10);
    
    var C1 = B1.Add(120, -40);
    var C2 = B2.Add(80, -20);
    var C3 = B2.Add(60, -30);
    
    double dist1 = C1.TotalDistance;



