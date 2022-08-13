---
title: "Constructores y Finalizadores"
slug: "constructores-y-finalizadores"
draft: false
images: []
weight: 9573
type: docs
toc: true
---

Los constructores son métodos en una clase que se invocan cuando se crea una instancia de esa clase. Su principal responsabilidad es dejar el nuevo objeto en un estado útil y consistente.

Los destructores/finalizadores son métodos en una clase que se invocan cuando se destruye una instancia de eso. En C#, rara vez se escriben/utilizan explícitamente.

C# en realidad no tiene destructores, sino Finalizadores que utilizan la sintaxis de destructores al estilo de C++. Especificar un destructor anula el método `Object.Finalize()` que no se puede llamar directamente.

A diferencia de otros lenguajes con una sintaxis similar, estos métodos *no* se llaman cuando los objetos quedan fuera del alcance, pero se llaman cuando se ejecuta el Recolector de Basura, lo que ocurre [bajo ciertas condiciones][3]. Como tal, *no* se garantiza que se ejecuten en ningún orden en particular.

Los finalizadores deben ser responsables de limpiar los recursos no administrados **solo** (punteros adquiridos a través de la clase Marshal, recibidos a través de p/Invoke (llamadas al sistema) o punteros sin procesar usados ​​dentro de bloques no seguros). Para limpiar los recursos administrados, revise IDisposable, el patrón Dispose y la instrucción [`using`][1].

(Lectura adicional: [¿Cuándo debo crear un destructor?][2])


[1]: https://www.wikiod.com/es/docs/c%23/38/using-statement
[2]: http://stackoverflow.com/a/4899622
[3]: https://msdn.microsoft.com/en-us/library/ee787088(v=vs.110).aspx#conditions_for_a_garbage_collection

## Constructor estático
Se llama a un constructor estático la primera vez que se inicializa cualquier miembro de un tipo, se llama a un miembro de clase estático oa un método estático.
El constructor estático es seguro para subprocesos.
Un constructor estático se usa comúnmente para:
- Inicializar el estado estático, es decir, el estado que se comparte entre diferentes instancias de la misma clase.
- Crear un singleton

**Ejemplo:**

    class Animal
    {
        // * A static constructor is executed only once,
        //   when a class is first accessed.
        // * A static constructor cannot have any access modifiers
        // * A static constructor cannot have any parameters
        static Animal()
        {
            Console.WriteLine("Animal initialized");
        }

        // Instance constructor, this is executed every time the class is created
        public Animal()
        {
            Console.WriteLine("Animal created");
        }

        public static void Yawn()
        {
            Console.WriteLine("Yawn!");
        }
    }

    var turtle = new Animal();
    var giraffe = new Animal();
**Producción:**
 
> Animal inicializado
> Animal creado
> Animal creado

[Ver demostración][1]

Si la primera llamada es a un método estático, el constructor estático se invoca sin el constructor de instancias. Esto está bien, porque el método estático no puede acceder al estado de la instancia de todos modos.

    Animal.Yawn();

Esto generará:

> Animal inicializado
> ¡Bostezo!

Ver también [Excepciones en constructores estáticos][2] y [Constructores estáticos genéricos][3] .


[1]: https://dotnetfiddle.net/XmExII
[2]: https://www.wikiod.com/es/docs/c%23/25/constructors-finalizers/15007/exceptions-in-static-constructors
[3]: https://www.wikiod.com/es/docs/c%23/25/constructors-finalizers/15003/generic-static-constructors

Ejemplo de singleton:

    public class SessionManager
    {
        public static SessionManager Instance;

        static SessionManager()
        {
            Instance = new SessionManager();
        }
    }

## Patrón constructor singleton
    public class SingletonClass
    {
        public static SingletonClass Instance { get; } = new SingletonClass();

        private SingletonClass()
        {
            // Put custom constructor code here
        }    
    }

Debido a que el constructor es privado, no se pueden crear nuevas instancias de `SingletonClass` mediante el consumo de código. La única forma de acceder a la instancia única de `SingletonClass` es usando la propiedad estática `SingletonClass.Instance`.

La propiedad "Instancia" la asigna un constructor estático que genera el compilador de C#. El tiempo de ejecución de .NET garantiza que el constructor estático se ejecuta como máximo una vez y se ejecuta antes de que se lea por primera vez `Instance`. Por lo tanto, todas las preocupaciones de sincronización e inicialización las lleva a cabo el tiempo de ejecución.

Tenga en cuenta que si el constructor estático falla, la clase `Singleton` se vuelve permanentemente inutilizable durante la vida del AppDomain.

Además, no se garantiza que el constructor estático se ejecute en el momento del primer acceso a `Instancia`. Más bien, se ejecutará *en algún momento antes de eso*. Esto hace que el momento en que ocurre la inicialización no sea determinista. En casos prácticos, el JIT a menudo llama al constructor estático durante la *compilación* (no la ejecución) de un método que hace referencia a `Instancia`. Esta es una optimización del rendimiento.

Consulte la página [Implementaciones de Singleton][1] para conocer otras formas de implementar el patrón de singleton.


[1]: https://www.wikiod.com/es/docs/c%23/1192/singleton-implementation#t=201607231143190778053

## Constructor predeterminado
Cuando un tipo se define sin un constructor:

    public class Animal
    {
    }

luego, el compilador genera un constructor predeterminado equivalente al siguiente:

    public class Animal
    {
        public Animal() {}
    }

La definición de cualquier constructor para el tipo suprimirá la generación del constructor predeterminado. Si el tipo se definiera de la siguiente manera:

    public class Animal
    {
        public Animal(string name) {}
    }

entonces un 'Animal' solo podría crearse llamando al constructor declarado.

    // This is valid
    var myAnimal = new Animal("Fluffy");
    // This fails to compile
    var unnamedAnimal = new Animal();

Para el segundo ejemplo, el compilador mostrará un mensaje de error:
>'Animal' no contiene un constructor que tome 0 argumentos

Si desea que una clase tenga un constructor sin parámetros y un constructor que tome un parámetro, puede hacerlo implementando explícitamente ambos constructores.

    public class Animal
    {
        
        public Animal() {} //Equivalent to a default constructor.
        public Animal(string name) {}
    }

El compilador no podrá generar un constructor predeterminado si la clase extiende otra clase que no tiene un constructor sin parámetros. Por ejemplo, si tuviéramos una clase `Criatura`:

    public class Creature
    {
        public Creature(Genus genus) {}
    }

entonces 'Animal' definido como 'clase Animal: Criatura {}' no compilaría.

## Forzar la llamada a un constructor estático
Si bien los constructores estáticos siempre se llaman antes del primer uso de un tipo, a veces es útil poder forzar su llamada y la clase `RuntimeHelpers` proporciona una ayuda para ello:

    using System.Runtime.CompilerServices;    
    // ...
    RuntimeHelpers.RunClassConstructor(typeof(Foo).TypeHandle);

***Observación*:** Se ejecutará toda la inicialización estática (por ejemplo, inicializadores de campos), no solo el propio constructor.

***Usos potenciales*:** Forzar la inicialización durante la pantalla de inicio en una aplicación de interfaz de usuario o garantizar que un constructor estático no falle en una prueba unitaria.

## Llamar a un constructor desde otro constructor
    public class Animal
    {
        public string Name { get; set; }

        public Animal() : this("Dog")
        {
        }

        public Animal(string name)
        {
            Name = name;
        }
    }

    var dog = new Animal();      // dog.Name will be set to "Dog" by default.
    var cat = new Animal("Cat"); // cat.Name is "Cat", the empty constructor is not called.


## Llamar al constructor de la clase base
Se llama a un constructor de una clase base antes de que se ejecute un constructor de una clase derivada. Por ejemplo, si `Mammal` extiende `Animal`, entonces el código contenido en el constructor de `Animal` se llama primero cuando se crea una instancia de `Mammal`.

Si una clase derivada no especifica explícitamente a qué constructor de la clase base se debe llamar, el compilador asume el constructor sin parámetros.

    public class Animal
    {
        public Animal() { Console.WriteLine("An unknown animal gets born."); }
        public Animal(string name) { Console.WriteLine(name + " gets born"); }
    }

    public class Mammal : Animal
    {
        public Mammal(string name)
        {
            Console.WriteLine(name + " is a mammal.");
        }
    }

En este caso, instanciar un `Mammal` llamando a `new Mammal("George the Cat")` imprimirá

>Nace un animal desconocido.
>George the Cat es un mamífero.

[Ver demostración][1]

La llamada a un constructor diferente de la clase base se realiza colocando `: base(args)` entre la firma del constructor y su cuerpo:

    public class Mammal : Animal
    {
        public Mammal(string name) : base(name)
        {
            Console.WriteLine(name + " is a mammal.");
        }
    }

Llamar a `new Mammal ("George the Cat")` ahora imprimirá:

>George el gato nace.
>George the Cat es un mamífero.

[Ver demostración][2]


[1]: https://dotnetfiddle.net/xb8Vqr
[2]: https://dotnetfiddle.net/gbdERq

## Finalizadores en clases derivadas
Cuando se finaliza un gráfico de objeto, el orden es el inverso al de la construcción. P.ej. el supertipo se finaliza antes que el tipo base, como demuestra el siguiente código:

    class TheBaseClass
    {
        ~TheBaseClass() 
        {
            Console.WriteLine("Base class finalized!");
        }
    }
    
    class TheDerivedClass : TheBaseClass
    {
        ~TheDerivedClass() 
        {
            Console.WriteLine("Derived class finalized!");
        }
    }

    //Don't assign to a variable
    //to make the object unreachable
    new TheDerivedClass();
    
    //Just to make the example work;
    //this is otherwise NOT recommended!
    GC.Collect();

    //Derived class finalized!
    //Base class finalized!

## Excepciones en constructores estáticos
Si un constructor estático lanza una excepción, nunca se vuelve a intentar. El tipo no se puede utilizar durante la vida útil de AppDomain. Cualquier otro uso del tipo generará una `TypeInitializationException` envuelta alrededor de la excepción original.


    public class Animal
    {
        static Animal()
        {
            Console.WriteLine("Static ctor");
            throw new Exception();
        }
    
        public static void Yawn() {}
    }

    try
    {
        Animal.Yawn();
    }
    catch (Exception e)
    {
        Console.WriteLine(e.ToString());
    }

    try
    {
        Animal.Yawn();
    }
    catch (Exception e)
    {
        Console.WriteLine(e.ToString());
    }

Esto generará:

> Factor estático
>
> System.TypeInitializationException: El tipo de inicializador
> para 'Animal' lanzó una excepción. ---> System.Exception: Excepción de
> tipo 'System.Exception' fue lanzada.

[...]

> System.TypeInitializationException: El inicializador de tipo para 'Animal'
> tiró una excepción. ---> System.Exception: Excepción de tipo
> 'System.Exception' fue lanzada.

donde puede ver que el constructor real solo se ejecuta una vez y la excepción se reutiliza.

## Llamar a métodos virtuales en el constructor
A diferencia de C ++ en C #, puede llamar a un método virtual desde el constructor de clases (OK, también puede hacerlo en C ++, pero el comportamiento al principio es sorprendente). Por ejemplo:

    abstract class Base
    {
        protected Base()
        {
            _obj = CreateAnother();
        }
    
        protected virtual AnotherBase CreateAnother()
        {
            return new AnotherBase();
        }
    
        private readonly AnotherBase _obj;
    }
    
    sealed class Derived : Base
    {
        public Derived() { }
    
        protected override AnotherBase CreateAnother()
        {
            return new AnotherDerived();
        }
    }
    
    var test = new Derived();
    // test._obj is AnotherDerived

Si proviene de un entorno C++, esto es sorprendente, ¡el constructor de la clase base ya ve la tabla de métodos virtuales de la clase derivada!

**Tenga cuidado**: es posible que la clase derivada aún no se haya inicializado por completo (su constructor se ejecutará después del constructor de la clase base) y esta técnica es peligrosa (también hay una advertencia de StyleCop para esto). Por lo general, esto se considera una mala práctica.


## Constructores estáticos genéricos
Si el tipo en el que se declara el constructor estático es genérico, el constructor estático se llamará una vez para cada combinación única de argumentos genéricos.

    class Animal<T>
    {
        static Animal()
        {
            Console.WriteLine(typeof(T).FullName);
        }

        public static void Yawn() { }
    }

    Animal<Object>.Yawn();
    Animal<String>.Yawn();

Esto generará:

> Sistema.Objeto
> Sistema.Cadena

Ver también [¿Cómo funcionan los constructores estáticos para tipos genéricos?][1]

[1]: http://stackoverflow.com/q/5629388

## Inicialización de constructores y propiedades
¿Se debe ejecutar la asignación del valor de la propiedad *antes* o *después* del constructor de la clase?

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;
        
        public TestClass() 
        {
            if (TestProperty == 1) 
            {
                Console.WriteLine("Shall this be executed?");
            }

            if (TestProperty == 2) 
            {
                Console.WriteLine("Or shall this be executed");
            }
        }
    }

    var testInstance = new TestClass() { TestProperty = 1 };

En el ejemplo anterior, ¿el valor `TestProperty` será `1` en el constructor de la clase o después del constructor de la clase?

----

Asignando valores de propiedad en la creación de la instancia de esta manera:

    var testInstance = new TestClass() {TestProperty = 1};

Se ejecutará ***después*** de que se ejecute el constructor. Sin embargo, inicializando el valor de la propiedad en la propiedad de la clase en C# 6.0 de esta manera:

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;

        public TestClass() 
        {
        }
    }

se hará ***antes*** de que se ejecute el constructor.

---

Combinando los dos conceptos anteriores en un solo ejemplo:

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;
        
        public TestClass() 
        {
            if (TestProperty == 1) 
            {
                Console.WriteLine("Shall this be executed?");
            }

            if (TestProperty == 2) 
            {
                Console.WriteLine("Or shall this be executed");
            }
        }
    }

    static void Main(string[] args) 
    {
        var testInstance = new TestClass() { TestProperty = 1 };
        Console.WriteLine(testInstance.TestProperty); //resulting in 1
    }

Resultado final:

    "Or shall this be executed"
    "1"

---

**Explicación:**

El valor `TestProperty` se asignará primero como `2`, luego se ejecutará el constructor `TestClass`, lo que dará como resultado la impresión de

    "Or shall this be executed"
    
Y luego `TestProperty` se asignará como `1` debido a `new TestClass() { TestProperty = 1 }`, haciendo que el valor final de `TestProperty` impreso por `Console.WriteLine(testInstance.TestProperty)` sea

    "1"



