---
title: "delegados"
slug: "delegados"
draft: false
images: []
weight: 9752
type: docs
toc: true
---

# Resumen

Un **tipo de delegado** es un tipo que representa una firma de método en particular. Una instancia de este tipo se refiere a un método particular con una firma coincidente. Los parámetros de método pueden tener tipos delegados, por lo que este método debe pasar una referencia a otro método, que luego puede invocarse.

# Tipos de delegados integrados: `Action<...>`, `Predicate<T>` y `Func<...,TResult>`

El espacio de nombres `System` contiene los delegados `Action<...>`,`Predicate<T>` y `Func<...,TResult>`, donde "..." representa entre 0 y 16 parámetros de tipo genérico ( para 0 parámetros, `Acción` no es genérica).

`Func` representa métodos con un tipo de retorno que coincide con `TResult`, y `Action` representa métodos sin valor de retorno (void). En ambos casos, los parámetros de tipo genérico adicionales coinciden, en orden, con los parámetros del método.

`Predicate` representa un método con tipo de retorno booleano, T es un parámetro de entrada.

# Tipos de delegados personalizados

Los tipos de delegados con nombre se pueden declarar mediante la palabra clave `delegate`.

# Invocación de delegados

Los delegados se pueden invocar usando la misma sintaxis que los métodos: el nombre de la instancia del delegado, seguido de paréntesis que contengan cualquier parámetro.

# Asignación a delegados

Los delegados se pueden asignar de las siguientes maneras:

- Asignación de un método con nombre
- Asignación de un método anónimo usando una lambda
- Asignar un método con nombre usando la palabra clave `delegate`.

# Combinando delegados

Se pueden asignar varios objetos delegados a una instancia de delegado mediante el operador `+`. El operador `-` se puede usar para eliminar un componente delegado de otro delegado.

## Declarar un tipo de delegado
La siguiente sintaxis crea un tipo `delegado` con el nombre `NumberInOutDelegate`, que representa un método que toma un `int` y devuelve un `int`.

    public delegate int NumberInOutDelegate(int input);

Esto se puede utilizar de la siguiente manera:

    public static class Program
    {
        static void Main()
        {
            NumberInOutDelegate square = MathDelegates.Square;
            int answer1 = square(4); 
            Console.WriteLine(answer1); // Will output 16

            NumberInOutDelegate cube = MathDelegates.Cube;
            int answer2 = cube(4);
            Console.WriteLine(answer2); // Will output 64            
        }
    }
    
    public static class MathDelegates
    {
        static int Square (int x)
        {
            return x*x;
        }

        static int Cube (int x)
        {
            return x*x*x;
        }
    }

La instancia de delegado `example` se ejecuta de la misma manera que el método `Square`. Una instancia delegada actúa literalmente como un delegado para la persona que llama: la persona que llama invoca el
delegado y, a continuación, el delegado llama al método de destino. Esta indirección se desacopla
la persona que llama desde el método de destino.

----------

Puede declarar un tipo de delegado __generic__ y, en ese caso, puede especificar que el tipo sea covariante (`out`) o contravariante (`in`) en algunos de los argumentos de tipo. Por ejemplo:

    public delegate TTo Converter<in TFrom, out TTo>(TFrom input);

Al igual que otros tipos genéricos, los tipos de delegados genéricos pueden tener restricciones, como `where TFrom: struct, IConvertible where TTo: new()`.

Evite la covarianza y la contravariación para los tipos de delegados destinados a ser usados ​​para delegados de multidifusión, como los tipos de controladores de eventos. Esto se debe a que la concatenación (`+`) puede fallar si el tipo de tiempo de ejecución es diferente del tipo de tiempo de compilación debido a la variación. Por ejemplo, evita:

    public delegate void EventHandler<in TEventArgs>(object sender, TEventArgs e);

En su lugar, utilice un tipo genérico invariable:

    public delegate void EventHandler<TEventArgs>(object sender, TEventArgs e);

----------

También se admiten delegados en los que algunos parámetros se modifican mediante `ref` o `out`, como en:

    public delegate bool TryParser<T>(string input, out T result);

(ejemplo de uso `TryParser<decimal> ejemplo = decimal.TryParse;`), o delegados donde el último parámetro tiene el modificador `params`. Los tipos de delegados pueden tener parámetros opcionales (suministrar valores predeterminados). Los tipos de delegados pueden usar tipos de puntero como `int*` o `char*` en sus firmas o tipos de devolución (use la palabra clave `unsafe`). Un tipo de delegado y sus parámetros pueden llevar atributos personalizados.

## Los tipos de delegado Func<T, TResult>, Action<T> y Predicate<T>
El espacio de nombres del sistema contiene tipos de delegado `Func<..., TResult>` con entre 0 y 15 parámetros genéricos, que devuelven el tipo `TResult`.

    private void UseFunc(Func<string> func)
    {
        string output = func(); // Func with a single generic type parameter returns that type
        Console.WriteLine(output);
    }

    private void UseFunc(Func<int, int, string> func)
    {
        string output = func(4, 2); // Func with multiple generic type parameters takes all but the first as parameters of that type
        Console.WriteLine(output);
    }

El espacio de nombres System también contiene tipos de delegado `Action<...>` con diferente número de parámetros genéricos (de 0 a 16). Es similar a `Func<T1, .., Tn>`, pero siempre devuelve `void`.

    private void UseAction(Action action)
    {
        action(); // The non-generic Action has no parameters
    }

    private void UseAction(Action<int, string> action)
    {
        action(4, "two"); // The generic action is invoked with parameters matching its type arguments
    }

`Predicate<T>` también es una forma de `Func` pero siempre devolverá `bool`. Un predicado es una forma de especificar un criterio personalizado. Según el valor de la entrada y la lógica definida dentro del predicado, devolverá "verdadero" o "falso". Por lo tanto, `Predicate<T>` se comporta de la misma manera que `Func<T, bool>` y ambos se pueden inicializar y usar de la misma manera.
    
    Predicate<string> predicate = s => s.StartsWith("a");
    Func<string, bool> func = s => s.StartsWith("a");

    // Both of these return true
    var predicateReturnsTrue = predicate("abc");
    var funcReturnsTrue = func("abc");

    // Both of these return false
    var predicateReturnsFalse = predicate("xyz");
    var funcReturnsFalse = func("xyz");

La elección de usar `Predicate<T>` o `Func<T, bool>` es realmente una cuestión de opinión. Se puede decir que `Predicate<T>` es más expresivo de la intención del autor, mientras que `Func<T, bool>` probablemente sea familiar para una mayor proporción de desarrolladores de C#.

Además de eso, hay algunos casos en los que solo una de las opciones está disponible, especialmente al interactuar con otra API. Por ejemplo, `List<T>` y `Array<T>` generalmente toman `Predicate<T>` como sus métodos, mientras que la mayoría de las extensiones LINQ solo aceptan `Func<T, bool>`.

## Combinar delegados (delegados de multidifusión)
Las operaciones de suma `+` y resta `-` se pueden usar para combinar instancias delegadas. El delegado contiene una lista de los delegados asignados.

    using System;
    using System.Reflection;
    using System.Reflection.Emit;

    namespace DelegatesExample {
        class MainClass {
            private delegate void MyDelegate(int a);

            private static void PrintInt(int a) {
                Console.WriteLine(a);
            }

            private static void PrintType<T>(T a) {
                Console.WriteLine(a.GetType());
            }

            public static void Main (string[] args)
            {
                MyDelegate d1 = PrintInt;
                MyDelegate d2 = PrintType;

                // Output:
                // 1
                d1(1);

                // Output:
                // System.Int32
                d2(1);

                MyDelegate d3 = d1 + d2;
                // Output:
                // 1
                // System.Int32
                d3(1);

                MyDelegate d4 = d3 - d2;
                // Output:
                // 1
                d4(1);

                // Output:
                // True
                Console.WriteLine(d1 == d4);
            }
        }
    }

En este ejemplo, `d3` es una combinación de los delegados `d1` y `d2`, por lo que, cuando se llama, el programa genera las cadenas `1` y `System.Int32`.


----------

Combinación de delegados con tipos de devolución **no nulos**:

Si un delegado de multidifusión tiene un tipo de retorno `nonvoid`, la persona que llama recibe el valor de retorno
desde el último método a ser invocado. Los métodos anteriores todavía se llaman, pero su
los valores devueltos se descartan.

        class Program
        {
            public delegate int Transformer(int x);

            static void Main(string[] args)
            {
                Transformer t = Square;
                t += Cube;
                Console.WriteLine(t(2));  // O/P 8 
            }

            static int Square(int x) { return x * x; }

            static int Cube(int x) { return x*x*x; }
        }

`t(2)` llamará primero a `Square` y luego a `Cube`. El valor de retorno de Square se descarta y se conserva el valor de retorno del último método, es decir, `Cube`.

## Delegado de multidifusión de invocación segura
Alguna vez quiso llamar a un delegado de multidifusión, pero desea que se llame a toda la lista de invocaciones incluso si se produce una excepción en cualquiera de la cadena. Entonces estás de suerte, he creado un método de extensión que hace exactamente eso, arrojando una `AgregateException` solo después de que se complete la ejecución de toda la lista:

    public static class DelegateExtensions
    {
        public static void SafeInvoke(this Delegate del,params object[] args)
        {
            var exceptions = new List<Exception>();

            foreach (var handler in del.GetInvocationList())
            {
                try
                {
                    handler.Method.Invoke(handler.Target, args);
                }
                catch (Exception ex)
                {
                    exceptions.Add(ex);
                }
            }

            if(exceptions.Any())
            {
                throw new AggregateException(exceptions);
            }
        }
    }

    public class Test
    {
        public delegate void SampleDelegate();

        public void Run()
        {
            SampleDelegate delegateInstance = this.Target2;
            delegateInstance += this.Target1;

            try
            {
                delegateInstance.SafeInvoke();
            } 
            catch(AggregateException ex)
            {
                // Do any exception handling here
            }
        }

        private void Target1()
        {
            Console.WriteLine("Target 1 executed");
        }

        private void Target2()
        {
            Console.WriteLine("Target 2 executed");
            throw new Exception();
        }
    }

Esto genera:

    Target 2 executed
    Target 1 executed

Invocar directamente, sin `SaveInvoke`, solo ejecutaría Target 2.

## Delegado Igualdad
Llamar a `.Equals()` en un delegado compara por igualdad de referencia:

    Action action1 = () => Console.WriteLine("Hello delegates");
    Action action2 = () => Console.WriteLine("Hello delegates");
    Action action1Again = action1;

    Console.WriteLine(action1.Equals(action1)) // True
    Console.WriteLine(action1.Equals(action2)) // False
    Console.WriteLine(action1Again.Equals(action1)) // True

Estas reglas también se aplican al hacer `+=` o `-=` en un delegado de multidifusión, por ejemplo, al suscribirse y darse de baja de eventos.

## Referencias subyacentes de delegados de métodos con nombre
Al asignar métodos con nombre a los delegados, se referirán al mismo objeto subyacente si:

- Son el mismo método de instancia, en la misma instancia de una clase
- Son el mismo método estático en una clase.

       public class Greeter
       {
           public void WriteInstance()
           {
               Console.WriteLine("Instance");
           }

           public static void WriteStatic()
           {
               Console.WriteLine("Static");
           }
       }

       // ...

       Greeter greeter1 = new Greeter();
       Greeter greeter2 = new Greeter();

       Action instance1 = greeter1.WriteInstance;
       Action instance2 = greeter2.WriteInstance;
       Action instance1Again = greeter1.WriteInstance;
    
       Console.WriteLine(instance1.Equals(instance2)); // False
       Console.WriteLine(instance1.Equals(instance1Again)); // True

       Action @static = Greeter.WriteStatic;
       Action staticAgain = Greeter.WriteStatic;

       Console.WriteLine(@static.Equals(staticAgain)); // True

## Asignar un método con nombre a un delegado
Los métodos con nombre se pueden asignar a delegados con firmas coincidentes:

    public static class Example
    {
        public static int AddOne(int input)
        {
            return input + 1;
        }
    }


    Func<int,int> addOne = Example.AddOne

`Example.AddOne` toma un `int` y devuelve un `int`, su firma coincide con el delegado `Func<int,int>`. `Ejemplo.AddOne` se puede asignar directamente a `addOne` porque tienen firmas coincidentes.

## Asignación a un delegado por lambda
Lambdas se puede usar para crear métodos anónimos para asignar a un delegado:

    Func<int,int> addOne = x => x+1;

Tenga en cuenta que se requiere la declaración explícita de tipo al crear una variable de esta manera:

    var addOne = x => x+1; // Does not work

## Pasar delegados como parámetros
Los delegados se pueden usar como punteros de función escritos:

    class FuncAsParameters
    {
      public void Run()
      {
        DoSomething(ErrorHandler1);
        DoSomething(ErrorHandler2);
      }
    
      public bool ErrorHandler1(string message)
      {
        Console.WriteLine(message);
        var shouldWeContinue = ...  
        return shouldWeContinue;
      }
    
      public bool ErrorHandler2(string message)
      {
        // ...Write message to file...
        var shouldWeContinue = ...  
        return shouldWeContinue;
      }
    
      public void DoSomething(Func<string, bool> errorHandler)
      {
        // In here, we don't care what handler we got passed!
        ...
        if (...error...)
        {
          if (!errorHandler("Some error occurred!"))
          {
            // The handler decided we can't continue
            return;
          }
        }
      }
    }

## Cierre dentro de un delegado


## Encapsulando transformaciones en funciones


