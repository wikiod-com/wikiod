---
title: "Herencia"
slug: "herencia"
draft: false
images: []
weight: 9785
type: docs
toc: true
---

## Sintaxis
- clase ClaseDerivada : ClaseBase
- clase DerivedClass: BaseClass, IExampleInterface
- clase DerivedClass: BaseClass, IExampleInterface, IAnotherInterface

Las clases pueden heredar directamente de una sola clase, pero (en su lugar o al mismo tiempo) pueden implementar una o más interfaces.

Las estructuras pueden implementar interfaces pero no pueden heredar explícitamente de ningún tipo. Heredan implícitamente de `System.ValueType`, que a su vez hereda directamente de `System.Object`.

Las clases estáticas [no pueden] [1] implementar interfaces.


[1]: http://stackoverflow.com/a/259079

## Herencia. Secuencia de llamadas de constructores
Considere que tenemos una clase 'Animal' que tiene una clase secundaria 'Perro'

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

Por defecto, cada clase hereda implícitamente la clase `Objeto`.

Esto es lo mismo que el código anterior.

    class Animal : Object
    {
        public Animal()
        {
            Console.WriteLine("In Animal's constructor");
        }
    }

Al crear una instancia de la clase `Dog`, se llamará al **constructor predeterminado de las clases base (sin parámetros) si no hay una llamada explícita a otro constructor en la clase padre**. En nuestro caso, primero se llamará constructor de 'Objeto', luego 'Animal' y al final constructor 'Perro'.

    public class Program
    {
        public static void Main()
        {
            Dog dog = new Dog();
        }
    }

La salida será
>En el constructor de Animal
>En el constructor de Dog

[Ver demostración][1]

**Llamar explícitamente al constructor de los padres.**

En los ejemplos anteriores, nuestro constructor de clase `Perro` llama al constructor **predeterminado** de la clase `Animal`. Si lo desea, puede especificar qué constructor debe llamarse: es posible llamar a cualquier constructor que esté definido en la clase principal.

Considere que tenemos estas dos clases.

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

**¿Qué está pasando aquí?**

Tenemos 2 constructores en cada clase.

**¿Qué significa `base`?**

`base` es una referencia a la clase principal. En nuestro caso, cuando creamos una instancia de la clase `Dog` como esta

    Dog dog = new Dog();

El tiempo de ejecución llama primero a `Dog()`, que es el constructor sin parámetros. Pero su cuerpo no funciona inmediatamente. Después de los paréntesis del constructor tenemos una llamada de este tipo: `base()`, lo que significa que cuando llamamos al constructor predeterminado `Dog`, a su vez llamará al constructor **predeterminado** del padre. Después de que se ejecute el constructor del padre, regresará y luego, finalmente, ejecutará el cuerpo del constructor `Dog()`.

Entonces la salida será así:
>Constructor por defecto de Animal
>Constructor por defecto del perro

[Ver demostración][2]

**Ahora, ¿qué pasa si llamamos al constructor `Dog` con un parámetro?**

    Dog dog = new Dog("Rex");

Sabe que los miembros de la clase principal que no son privados son heredados por la clase secundaria, lo que significa que 'Perro' también tendrá el campo 'nombre'.
En este caso le pasamos un argumento a nuestro constructor. A su vez, pasa el argumento al **constructor de la clase padre con un parámetro**, que inicializa el campo `nombre`.

La salida será

<!-- idioma: lang-none -->
    Animal's constructor with 1 parameter
    Rex
    Dog's constructor with 1 parameter
    Rex

**Resumen:**

Cada creación de objetos comienza desde la clase base. En la herencia se encadenan las clases que están en la jerarquía. Como todas las clases se derivan de `Objeto`, el primer constructor que se llama cuando se crea cualquier objeto es el constructor de la clase `Objeto`; Luego se llama al siguiente constructor en la cadena y solo después de que se llamen a todos, se crea el objeto

**palabra clave base**

1) La palabra clave base se usa para acceder a los miembros de la clase base desde dentro de una clase derivada:
2) Llame a un método en la clase base que ha sido anulado por otro método.
Especifique a qué constructor de clase base se debe llamar al crear instancias de la clase derivada.


[1]: https://dotnetfiddle.net/uOL8cE
[2]: https://dotnetfiddle.net/eRKEjT

## Heredar de una clase base
Para evitar la duplicación de código, defina métodos y atributos comunes en una clase general como base:

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
  
Ahora que tiene una clase que representa `Animal` en general, puede definir una clase que describa las peculiaridades de animales específicos:
  
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

La clase Cat obtiene acceso no solo a los métodos descritos en su definición explícitamente, sino también a todos los métodos definidos en la clase base general `Animal`. Cualquier animal (fuera o no un gato) podía comer, mirar o rodar. Sin embargo, un Animal no podría Rascar, a menos que también fuera un Gato. A continuación, podría definir otras clases que describan otros animales. (Como Gopher con un método para destruir jardines de flores y Sloth sin ningún método adicional).

## Heredar de una clase e implementar una interfaz
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



## Heredar de una clase e implementar múltiples interfaces
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

## Probando y navegando por la herencia
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

## Extendiendo una clase base abstracta
A diferencia de las interfaces, que pueden describirse como contratos de implementación, las clases abstractas actúan como contratos de extensión.

Una clase abstracta no se puede instanciar, se debe extender y luego se puede instanciar la clase resultante (o la clase derivada).

Las clases abstractas se utilizan para proporcionar implementaciones genéricas.

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

El ejemplo anterior muestra cómo cualquier clase que extienda Car recibirá automáticamente el método HonkHorn con la implementación. Esto significa que cualquier desarrollador que cree un nuevo automóvil no tendrá que preocuparse por cómo tocará la bocina.

## Constructores en una subclase
Cuando crea una subclase de una clase base, puede construir la clase base usando `: base` después de los parámetros del constructor de la subclase.

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

## Herencia Anti-patrones
# Herencia impropia

Digamos que hay 2 clases clase `Foo` y `Bar`. `Foo` tiene dos características `Do1` y `Do2`. `Bar` necesita usar `Do1` de `Foo`, pero no necesita `Do2` o necesita una función que sea equivalente a `Do2` pero hace algo completamente diferente.

**Mala manera**: hacer `Do2()` en `Foo` virtual y luego anularlo en `Bar` o simplemente `lanzar Excepción` en `Bar` para `Do2()`

    public class Bar : Foo
    {
        public override void Do2()
        {
            //Does something completely different that you would expect Foo to do
            //or simply throws new Exception 
        }
    }

**Buen camino**

Saque `Do1()` de `Foo` y colóquelo en la nueva clase `Baz`, luego herede tanto `Foo` como `Bar` de `Baz` e implemente `Do2()` por separado

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

Ahora, por qué el primer ejemplo es malo y el segundo es bueno: cuando el desarrollador nr2 tiene que hacer un cambio en `Foo`, es probable que rompa la implementación de `Bar` porque `Bar` ahora es inseparable de `Foo`. Al hacerlo con el último ejemplo, `Foo` y `Bar` se han movido a `Baz` y no se afectan entre sí (como no deberían).


## Heredar métodos
Hay varias formas en que los métodos pueden ser heredados

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

## Clase base con especificación de tipo recursivo
Definición única de una clase base genérica con especificador de tipo recursivo. Cada nodo tiene un padre y varios hijos.

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

Lo anterior se puede reutilizar cada vez que se necesite definir una jerarquía de árbol de objetos. El objeto de nodo en el árbol tiene que heredar de la clase base con

    public class MyNode : Tree<MyNode>
    {
        // stuff
    }

cada clase de nodo sabe dónde se encuentra en la jerarquía, cuál es el objeto principal y cuáles son los objetos secundarios. Varios tipos incorporados usan una estructura de árbol, como `Control` o `XmlElement` y el `Tree<T>` anterior se puede usar como una clase base de _cualquier_ tipo en su código.


----------


Por ejemplo, para crear una jerarquía de partes donde el peso total se calcula a partir del peso de _todos_ los elementos secundarios, haga lo siguiente:

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


Otro ejemplo sería en la definición de marcos de coordenadas relativas. En este caso, la verdadera posición del marco de coordenadas depende de las posiciones de _todos_ los marcos de coordenadas principales.

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

para ser utilizado como

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



