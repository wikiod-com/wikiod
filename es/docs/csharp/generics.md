---
title: "Genéricos"
slug: "genericos"
draft: false
images: []
weight: 9216
type: docs
toc: true
---

## Sintaxis
- `public void SomeMethod <T> () { }`
- `vacío público AlgúnMétodo<T, V>() { }`
- `public T SomeMethod<T>(IEnumerable<T> secuencia) { ... }`
- `public void SomeMethod<T>() donde T : new() { }`
- `public void SomeMethod<T, V>() where T : new() where V : struct { }`
- `public void SomeMethod<T>() donde T: IDisposable { }`
- `public void SomeMethod<T>() donde T: Foo { }`
- `public class MyClass<T> { public T Data {get; establecer; } }`

## Parámetros
| Parámetro(s) | Descripción |
|---|---|
| T, V | Escriba marcadores de posición para declaraciones genéricas |



Los genéricos en C# son compatibles hasta el tiempo de ejecución: los tipos genéricos creados con C# conservarán su semántica genérica incluso después de compilarlos en [CIL][1].


Esto significa que, en C#, es posible reflexionar sobre los tipos genéricos y verlos tal como fueron declarados o verificar si un objeto es una instancia de un tipo genérico, por ejemplo. Esto contrasta con [borrado de tipo][2], donde la información de tipo genérico se elimina durante la compilación. También contrasta con el enfoque de plantilla para los genéricos, donde múltiples tipos genéricos concretos se convierten en múltiples tipos no genéricos en tiempo de ejecución, y se pierden los metadatos necesarios para instanciar aún más las definiciones de tipos genéricos originales.

Tenga cuidado, sin embargo, cuando reflexione sobre los tipos genéricos: los nombres de los tipos genéricos se modificarán en la compilación, sustituyendo los corchetes angulares y los nombres de los parámetros de tipo por un acento grave seguido del número de parámetros de tipo genérico. Así, un `Dictionary<TKey, Tvalue>` se traducirá a ``Dictionary`2``.


[1]: https://en.wikipedia.org/wiki/Common_Intermediate_Language
[2]: https://en.wikipedia.org/wiki/Type_erasure

## Inferencia de tipos implícitos (métodos)
Al pasar argumentos formales a un método genérico, los argumentos de tipo genérico relevantes generalmente se pueden inferir implícitamente. Si se pueden inferir todos los tipos genéricos, especificarlos en la sintaxis es opcional.

Considere el siguiente método genérico. Tiene un parámetro formal y un parámetro de tipo genérico. Existe una relación muy obvia entre ellos: el tipo pasado como argumento al parámetro de tipo genérico debe ser el mismo que el tipo de tiempo de compilación del argumento pasado al parámetro formal.

    void M<T>(T obj)
    {
    }

Estas dos llamadas son equivalentes:

    M<object>(new object());
    M(new object());

Estas dos llamadas también son equivalentes:

    M<string>("");
    M("");

Y así son estas tres llamadas:

    M<object>("");
    M((object) "");
    M("" as object);

---

Tenga en cuenta que si al menos un argumento de tipo no se puede inferir, entonces se deben especificar todos.

Considere el siguiente método genérico. El primer argumento de tipo genérico es el mismo que el tipo del argumento formal. Pero no existe tal relación para el segundo argumento de tipo genérico. Por lo tanto, el compilador no tiene forma de inferir el segundo argumento de tipo genérico en ninguna llamada a este método.

    void X<T1, T2>(T1 obj)
    {
    }

Esto ya no funciona:

    X("");

Esto tampoco funciona, porque el compilador no está seguro de si estamos especificando el primer o el segundo parámetro genérico (ambos serían válidos como `objeto`):

    X<object>("");

Estamos obligados a escribir ambos, así:

    X<string, object>("");

## Inferencia de tipo (clases)
Los desarrolladores pueden quedar atrapados por el hecho de que la inferencia de tipo *no funciona* para los constructores:

    class Tuple<T1,T2>
    {
       public Tuple(T1 value1, T2 value2)
       {
       }
    }

    var x = new Tuple(2, "two");              // This WON'T work...
    var y = new Tuple<int, string>(2, "two"); // even though the explicit form will.

La primera forma de crear una instancia sin especificar explícitamente los parámetros de tipo provocará un error de tiempo de compilación que diría:
>Usar el tipo genérico 'Tuple<T1, T2>' requiere 2 argumentos de tipo

Una solución común es agregar un método auxiliar en una clase estática:

    static class Tuple
    {
        public static Tuple<T1, T2> Create<T1, T2>(T1 value1, T2 value2)
        {
             return new Tuple<T1, T2>(value1, value2);
        }
    }

    var x = Tuple.Create(2, "two");  // This WILL work...

## Usar un método genérico con una interfaz como tipo de restricción.
Este es un ejemplo de cómo usar el tipo genérico TFood dentro del método Eat<TFood> en la clase Animal

    public interface IFood
    {
        void EatenBy(Animal animal);
    }
    
    public class Grass: IFood
    {
        public void EatenBy(Animal animal)
        {
            Console.WriteLine("Grass was eaten by: {0}", animal.Name);
        }
    }
    
    public class Animal
    {
        public string Name { get; set; }
    
        public void Eat<TFood>(TFood food)
            where TFood : IFood
        {
            food.EatenBy(this);
        }
    }
    
    public class Carnivore : Animal
    {
        public Carnivore()
        {
            Name = "Carnivore";
        }
    }
    
    public class Herbivore : Animal, IFood
    {
        public Herbivore()
        {
            Name = "Herbivore";
        }
        
        public void EatenBy(Animal animal)
        {
            Console.WriteLine("Herbivore was eaten by: {0}", animal.Name);
        }
    }

Puede llamar al método Eat<TFood> de esta manera:

    var grass = new Grass();        
    var sheep = new Herbivore();
    var lion = new Carnivore();
        
    sheep.Eat(grass);
    //Output: Grass was eaten by: Herbivore

    lion.Eat(sheep);
    //Output: Herbivore was eaten by: Carnivore

En este caso, si intenta llamar:
   
    sheep.Eat(lion);

No será posible porque el objeto león no implementa la interfaz IFood. Intentar realizar la llamada anterior generará un error del compilador: "El tipo 'Carnivore' no se puede usar como parámetro de tipo 'TFoid' en el tipo o método genérico 'Animal.Eat<TFoid>(TFoid)'. No hay una referencia implícita conversión de 'Carnívoro' a 'IFood'".

## Restricciones de tipo (nueva-palabra clave)
Mediante el uso de la restricción `new()`, es posible aplicar parámetros de tipo para definir un constructor vacío (predeterminado).

    class Foo
    {
        public Foo () { }
    }

    class Bar
    {
        public Bar (string s) { ... }
    }

    class Factory<T>
        where T : new()
    {
        public T Create()
        {
            return new T();
        }
    }

    Foo f = new Factory<Foo>().Create(); // Valid.
    Bar b = new Factory<Bar>().Create(); // Invalid, Bar does not define a default/empty constructor.

La segunda llamada a `Crear ()` dará un error de tiempo de compilación con el siguiente mensaje:
>'Bar' debe ser un tipo no abstracto con un constructor público sin parámetros para usarlo como parámetro 'T' en el tipo o método genérico 'Factory<T>'

No hay restricciones para un constructor con parámetros, solo se admiten los constructores sin parámetros.

## Restricciones de tipo (clases e interfaces)
Las restricciones de tipo pueden obligar a un parámetro de tipo a implementar una determinada interfaz o clase.

    interface IType;
    interface IAnotherType;

    // T must be a subtype of IType
    interface IGeneric<T>
        where T : IType
    {
    }

    // T must be a subtype of IType
    class Generic<T>
        where T : IType
    {
    }

    class NonGeneric
    {
        // T must be a subtype of IType
        public void DoSomething<T>(T arg)
            where T : IType
        {
        }
    }

    // Valid definitions and expressions:
    class Type : IType { }
    class Sub : IGeneric<Type> { }
    class Sub : Generic<Type> { }
    new NonGeneric().DoSomething(new Type());

    // Invalid definitions and expressions:
    class AnotherType : IAnotherType { }
    class Sub : IGeneric<AnotherType> { }
    class Sub : Generic<AnotherType> { }
    new NonGeneric().DoSomething(new AnotherType());

Sintaxis para múltiples restricciones:

    class Generic<T, T1>
        where T : IType 
        where T1 : Base, new()
    {
    }

Las restricciones de tipo funcionan de la misma manera que la herencia, ya que es posible especificar múltiples interfaces como restricciones en el tipo genérico, pero solo una clase:

    class A { /* ... */ }
    class B { /* ... */ }

    interface I1 { }
    interface I2 { }

    class Generic<T>
        where T : A, I1, I2
    {
    }

    class Generic2<T>
        where T : A, B //Compilation error
    {
    }

Otra regla es que se debe agregar la clase como primera restricción y luego las interfaces:

    class Generic<T>
        where T : A, I1
    {
    }

    class Generic2<T>
        where T : I1, A //Compilation error
    {
    }

Todas las restricciones declaradas deben cumplirse simultáneamente para que una instancia genérica particular funcione. No hay forma de especificar dos o más conjuntos alternativos de restricciones.

## Reflexionando sobre los parámetros de tipo
El operador `typeof` trabaja en parámetros de tipo.

    class NameGetter<T>
    {
        public string GetTypeName()
        {
            return typeof(T).Name;
        }
    }

## Covarianza
¿Cuándo un `IEnumerable<T>` es un subtipo de un `IEnumerable<T1>` diferente? Cuando `T` es un subtipo de `T1`. `IEnumerable` es _covariante_ en su parámetro `T`, lo que significa que la relación de subtipo de `IEnumerable` va en _la misma dirección_ que `T`.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }

    IEnumerable<Dog> dogs = Enumerable.Empty<Dog>();
    IEnumerable<Animal> animals = dogs;  // IEnumerable<Dog> is a subtype of IEnumerable<Animal>
    // dogs = animals;  // Compilation error - IEnumerable<Animal> is not a subtype of IEnumerable<Dog>

Una instancia de un tipo genérico covariante con un parámetro de tipo determinado se convierte implícitamente en el mismo tipo genérico con un parámetro de tipo menos derivado.

Esta relación se mantiene porque `IEnumerable` _produce_ `T`s pero no los consume. Un objeto que produce 'Perros' puede usarse como si produjera 'Animales'.

Los parámetros de tipo covariante se declaran usando la palabra clave `out`, porque el parámetro debe usarse solo como una _salida_.

    interface IEnumerable<out T> { /* ... */ }

Un parámetro de tipo declarado como covariante puede no aparecer como entrada.

    interface Bad<out T>
    {
        void SetT(T t);  // type error
    }

Aquí hay un ejemplo completo:

    using NUnit.Framework;
    
    namespace ToyStore
    {
       enum Taste { Bitter, Sweet };
    
       interface IWidget
       {
          int Weight { get; }
       }
    
       interface IFactory<out TWidget>
           where TWidget : IWidget
       {
          TWidget Create();
       }
    
       class Toy : IWidget
       {
          public int Weight { get; set; }
          public Taste Taste { get; set; }
       }
    
       class ToyFactory : IFactory<Toy>
       {
          public const int StandardWeight = 100;
          public const Taste StandardTaste = Taste.Sweet;

          public Toy Create() { return new Toy { Weight = StandardWeight, Taste = StandardTaste }; }
       }
    
       [TestFixture]
       public class GivenAToyFactory
       {
          [Test]
          public static void WhenUsingToyFactoryToMakeWidgets()
          {
             var toyFactory = new ToyFactory();
    
             //// Without out keyword, note the verbose explicit cast:
             // IFactory<IWidget> rustBeltFactory = (IFactory<IWidget>)toyFactory;
    
             // covariance: concrete being assigned to abstract (shiny and new)
             IFactory<IWidget> widgetFactory = toyFactory;
             IWidget anotherToy = widgetFactory.Create();
             Assert.That(anotherToy.Weight, Is.EqualTo(ToyFactory.StandardWeight)); // abstract contract
             Assert.That(((Toy)anotherToy).Taste, Is.EqualTo(ToyFactory.StandardTaste)); // concrete contract
          }
       }
    }
    

## Contravarianza
¿Cuándo un `IComparer<T>` es un subtipo de un `IComparer<T1>` diferente? Cuando `T1` es un subtipo de `T`. `IComparer` es _contravariante_ en su parámetro `T`, lo que significa que la relación de subtipo de `IComparer` va en la _dirección opuesta_ como `T`.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }

    IComparer<Animal> animalComparer = /* ... */;
    IComparer<Dog> dogComparer = animalComparer;  // IComparer<Animal> is a subtype of IComparer<Dog>
    // animalComparer = dogComparer;  // Compilation error - IComparer<Dog> is not a subtype of IComparer<Animal>

Una instancia de un tipo genérico contravariante con un parámetro de tipo dado se convierte implícitamente en el mismo tipo genérico con un parámetro de tipo más derivado.

Esta relación se mantiene porque `IComparer` _consume_ `T`s pero no las produce. Un objeto que puede comparar dos 'Animales' cualquiera puede usarse para comparar dos 'Perros'.

Los parámetros de tipo contravariante se declaran usando la palabra clave `in`, porque el parámetro debe usarse solo como una _entrada_.

    interface IComparer<in T> { /* ... */ }

Un parámetro de tipo declarado como contravariante puede no aparecer como salida.

    interface Bad<in T>
    {
        T GetT();  // type error
    }

## Invariancia
`IList<T>` nunca es un subtipo de un `IList<T1>` diferente. `IList` es _invariante_ en su parámetro de tipo.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }
    
    IList<Dog> dogs = new List<Dog>();
    IList<Animal> animals = dogs;  // type error

No existe una relación de subtipo para las listas porque puede poner valores en una lista _y_ sacar valores de una lista.

Si `IList` fuera covariante, podría agregar elementos del _subtipo incorrecto_ a una lista dada.

    IList<Animal> animals = new List<Dog>();  // supposing this were allowed...
    animals.Add(new Giraffe());  // ... then this would also be allowed, which is bad!

Si `IList` fuera contravariante, podría extraer valores del subtipo incorrecto de una lista dada.

    IList<Dog> dogs = new List<Animal> { new Dog(), new Giraffe() };  // if this were allowed...
    Dog dog = dogs[1];  // ... then this would be allowed, which is bad!

Los parámetros de tipo invariable se declaran omitiendo las palabras clave `in` y `out`.

    interface IList<T> { /* ... */ }

## Interfaces variantes
Las interfaces pueden tener parámetros de tipo variante.

    interface IEnumerable<out T>
    {
        // ...
    }
    interface IComparer<in T>
    {
        // ...
    }

pero las clases y estructuras pueden no

    class BadClass<in T1, out T2>  // not allowed
    {
    }
    
    struct BadStruct<in T1, out T2>  // not allowed
    {
    }

ni declaraciones de métodos genéricos

    class MyClass
    {
        public T Bad<out T, in T1>(T1 t1)  // not allowed
        {
            // ...
        }
    }

El siguiente ejemplo muestra múltiples declaraciones de variación en la misma interfaz

    interface IFoo<in T1, out T2, T3>
    //  T1 : Contravariant type
    //  T2 : Covariant type 
    //  T3 : Invariant type
    {
        // ...
    }
    
    IFoo<Animal, Dog, int> foo1 = /* ... */;
    IFoo<Dog, Animal, int> foo2 = foo1;  
    // IFoo<Animal, Dog, int> is a subtype of IFoo<Dog, Animal, int>



## Comprobación de la igualdad de valores genéricos.
Si la lógica de la clase o el método genérico requiere verificar la igualdad de los valores que tienen un tipo genérico, use `EqualityComparer<TType>.Default` [propiedad][1]:


    public void Foo<TBar>(TBar arg1, TBar arg2)
    {
        var comparer = EqualityComparer<TBar>.Default;
        if (comparer.Equals(arg1,arg2)
        {
            ...
        }
    }

Este enfoque es mejor que simplemente llamar al método `Object.Equals()`, porque la implementación del comparador predeterminado comprueba si el tipo `TBar` implementa `IEquatale<TBar>` [interfaz][2] y, en caso afirmativo, llama a `IEquatable<TBar> .Equals(TBar otro)` método. Esto permite evitar el empaquetado/desempaquetado de tipos de valor.


[1]: https://msdn.microsoft.com/en-us/library/ms224763(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx

## Tipo de parámetros (interfaces)
Declaración:

    interface IMyGenericInterface<T1, T2, T3, ...> { ... }

Uso (en herencia):

    class ClassA<T1, T2, T3> : IMyGenericInterface<T1, T2, T3> { ... }

    class ClassB<T1, T2> : IMyGenericInterface<T1, T2, int> { ... }

    class ClassC<T1> : IMyGenericInterface<T1, char, int> { ... }

    class ClassD : IMyGenericInterface<bool, char, int> { ... }

Uso (como el tipo de un parámetro):

    void SomeMethod(IMyGenericInterface<int, char, bool> arg) { ... }

## Delegados variantes
Los delegados pueden tener parámetros de tipo de variante.

    delegate void Action<in T>(T t);    // T is an input
    delegate T Func<out T>();           // T is an output
    delegate T2 Func<in T1, out T2>();  // T1 is an input, T2 is an output

Esto se deriva del [Principio de sustitución de Liskov][1], que establece (entre otras cosas) que un método D puede considerarse más derivado que un método B si:

- D tiene un tipo de retorno igual o más derivado que B
- D tiene tipos de parámetros correspondientes iguales o más generales que B

Por lo tanto, las siguientes asignaciones son seguras para todos los tipos:

    Func<object, string> original = SomeMethod;
    Func<object, object> d1 = original;
    Func<string, string> d2 = original;
    Func<string, object> d3 = original;

[1]: https://en.wikipedia.org/wiki/Liskov_substitution_principle

## Tipos de variantes como parámetros y valores de retorno
Si aparece un tipo covariante como salida, el tipo contenedor es covariante. Producir un productor de 'T's es como producir 'T's.

    interface IReturnCovariant<out T>
    {
        IEnumerable<T> GetTs();
    }

Si aparece un tipo contravariante como salida, el tipo contenedor es contravariante. Producir un consumidor de `T`s es como consumir `T`s.

    interface IReturnContravariant<in T>
    {
        IComparer<T> GetTComparer();
    }

Si aparece un tipo covariante como entrada, el tipo contenedor es contravariante. Consumir un productor de `T`s es como consumir `T`s.

    interface IAcceptCovariant<in T>
    {
        void ProcessTs(IEnumerable<T> ts);
    }

Si aparece un tipo contravariante como entrada, el tipo contenedor es covariante. Consumir un consumidor de `T`s es como producir `T`s.

    interface IAcceptContravariant<out T>
    {
        void CompareTs(IComparer<T> tComparer);
    }

## Tipo de parámetros (clases)
Declaración:

    class MyGenericClass<T1, T2, T3, ...>
    {
        // Do something with the type parameters.
    }

Inicialización:

    var x = new MyGenericClass<int, char, bool>();

Uso (como el tipo de un parámetro):

    void AnotherMethod(MyGenericClass<float, byte, char> arg) { ... }

## Tipo de parámetros (métodos)
Declaración:

    void MyGenericMethod<T1, T2, T3>(T1 a, T2 b, T3 c)
    {
        // Do something with the type parameters.
    }

Invocación:

No hay necesidad de proporcionar argumentos de tipo a un método genérico, porque el compilador puede inferir implícitamente el tipo.
    
    int x =10;
    int y =20;
    string z = "test";
    MyGenericMethod(x,y,z);

Sin embargo, si hay una ambigüedad, los métodos genéricos deben llamarse con argumentos de tipo como

    MyGenericMethod<int, int, string>(x,y,z);



## Restricciones de tipo (clase y estructura)
Es posible especificar si el argumento de tipo debe ser o no un tipo de referencia o un tipo de valor usando las restricciones respectivas 'clase' o 'estructura'. Si se utilizan estas restricciones, *deben* definirse _antes de que_ se puedan listar todas las demás restricciones (por ejemplo, un tipo padre o `nuevo()`).

    // TRef must be a reference type, the use of Int32, Single, etc. is invalid.
    // Interfaces are valid, as they are reference types
    class AcceptsRefType<TRef>
        where TRef : class
    {
        // TStruct must be a value type.
        public void AcceptStruct<TStruct>()
            where TStruct : struct
        {
        }

        // If multiple constraints are used along with class/struct
        // then the class or struct constraint MUST be specified first
        public void Foo<TComparableClass>()
            where TComparableClass : class, IComparable
        {
        }
    }

## Parámetros de tipo explícito
Hay diferentes casos en los que debe especificar explícitamente los parámetros de tipo para un método genérico. En los dos casos siguientes, el compilador no puede inferir todos los parámetros de tipo a partir de los parámetros de método especificados.

Un caso es cuando no hay parámetros:

    public void SomeMethod<T, V>() 
    {
       // No code for simplicity
    }

    SomeMethod(); // doesn't compile
    SomeMethod<int, bool>(); // compiles

El segundo caso es cuando uno (o más) de los parámetros de tipo no es parte de los parámetros del método:

    public K SomeMethod<K, V>(V input)
    {
        return default(K);
    }

    int num1 = SomeMethod(3); // doesn't compile
    int num2 = SomeMethod<int>("3"); // doesn't compile
    int num3 = SomeMethod<int, string>("3"); // compiles.

## Casting de tipos genéricos
        /// <summary>
        /// Converts a data type to another data type.
        /// </summary>
        public static class Cast
        {
            /// <summary>
            /// Converts input to Type of default value or given as typeparam T
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="input">Input that need to be converted to specified type</param>
            /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
            /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
            public static T To<T>(object input, T defaultValue)
            {
                var result = defaultValue;
                try
                {
                    if (input == null || input == DBNull.Value) return result;
                    if (typeof (T).IsEnum)
                    {
                        result = (T) Enum.ToObject(typeof (T), To(input, Convert.ToInt32(defaultValue)));
                    }
                    else
                    {
                        result = (T) Convert.ChangeType(input, typeof (T));
                    }
                }
                catch (Exception ex)
                {
                    Tracer.Current.LogException(ex);
                }
    
                return result;
            }
            
            /// <summary>
            /// Converts input to Type of typeparam T
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="input">Input that need to be converted to specified type</param>
            /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
            public static T To<T>(object input)
            {
                return To(input, default(T));
            }
    
            
    
        }

Usos:

    std.Name = Cast.To<string>(drConnection["Name"]);
    std.Age = Cast.To<int>(drConnection["Age"]);
    std.IsPassed = Cast.To<bool>(drConnection["IsPassed"]);

    
    // Casting type using default value
    //Following both ways are correct
    // Way 1 (In following style input is converted into type of default value)
    std.Name = Cast.To(drConnection["Name"], "");
    std.Marks = Cast.To(drConnection["Marks"], 0);
    // Way 2    
    std.Name = Cast.To<string>(drConnection["Name"], "");
    std.Marks = Cast.To<int>(drConnection["Marks"], 0);

## Lector de configuración con conversión de tipos genéricos
        /// <summary>
        /// Read configuration values from app.config and convert to specified types
        /// </summary>
        public static class ConfigurationReader
        {
            /// <summary>
            /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="strKey">key to find value from AppSettings</param>
            /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
            /// <returns>AppSettings value against key is returned in Type of default value or given as typeparam T</returns>
            public static T GetConfigKeyValue<T>(string strKey, T defaultValue)
            {
                var result = defaultValue;
                try
                {
                    if (ConfigurationManager.AppSettings[strKey] != null)
                        result = (T)Convert.ChangeType(ConfigurationManager.AppSettings[strKey], typeof(T));
                }
                catch (Exception ex)
                {
                    Tracer.Current.LogException(ex);
                }
    
                return result;
            }
            /// <summary>
            /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="strKey">key to find value from AppSettings</param>
            /// <returns>AppSettings value against key is returned in Type given as typeparam T</returns>
            public static T GetConfigKeyValue<T>(string strKey)
            {
                return GetConfigKeyValue(strKey, default(T));
            }
    
        }

Usos:

    var timeOut = ConfigurationReader.GetConfigKeyValue("RequestTimeout", 2000);
    var url = ConfigurationReader.GetConfigKeyValue("URL", "www.someurl.com");
    var enabled = ConfigurationReader.GetConfigKeyValue("IsEnabled", false);

