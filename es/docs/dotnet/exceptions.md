---
title: "Excepciones"
slug: "excepciones"
draft: false
images: []
weight: 9760
type: docs
toc: true
---

Relacionado:

* [MSDN: Excepciones y manejo de excepciones (Guía de programación de C#)](https://msdn.microsoft.com/en-us/library/ms173160.aspx)
* [MSDN: manejo y generación de excepciones](https://msdn.microsoft.com/en-us/library/5b2yeyab.aspx)
* [MSDN: CA1031: No detectar tipos de excepciones generales](https://msdn.microsoft.com/en-us/library/ms182137.aspx)
* [MSDN: Try-catch (Referencia de C#)](https://msdn.microsoft.com/en-us/library/0yd65esw.aspx)

## Atrapar y volver a lanzar excepciones atrapadas
Cuando desea detectar una excepción y hacer algo, pero no puede continuar con la ejecución del bloque de código actual debido a la excepción, es posible que desee volver a generar la excepción al siguiente controlador de excepciones en la pila de llamadas. Hay buenas y malas maneras de hacer esto.

    private static void AskTheUltimateQuestion()
    {
        try
        {
            var x = 42;
            var y = x / (x - x); // will throw a DivideByZeroException

            // IMPORTANT NOTE: the error in following string format IS intentional
            // and exists to throw an exception to the FormatException catch, below
            Console.WriteLine("The secret to life, the universe, and everything is {1}", y); 
        }
        catch (DivideByZeroException)
        {
            // we do not need a reference to the exception
            Console.WriteLine("Dividing by zero would destroy the universe.");

            // do this to preserve the stack trace:
            throw;
        }
        catch (FormatException ex)
        {
            // only do this if you need to change the type of the Exception to be thrown 
            // and wrap the inner Exception

            // remember that the stack trace of the outer Exception will point to the
            // next line

            // you'll need to examine the InnerException property to get the stack trace
            // to the line that actually started the problem

            throw new InvalidOperationException("Watch your format string indexes.", ex);
        }
        catch (Exception ex)
        {
            Console.WriteLine("Something else horrible happened. The exception: " + ex.Message);

            // do not do this, because the stack trace will be changed to point to
            // this location instead of the location where the exception
            // was originally thrown:
            throw ex; 
        }
    }

    static void Main()
    {
        try
        {
            AskTheUltimateQuestion();
        }
        catch
        {
            // choose this kind of catch if you don't need any information about 
            // the exception that was caught

            // this block "eats" all exceptions instead of rethrowing them
        }
    }

Puede filtrar por tipo de excepción e incluso por propiedades de excepción (nuevo en C# 6.0, un poco más disponible en VB.NET (cita requerida)):

[Documentación/C#/nuevas características][1]


[1]: https://www.wikiod.com/es/docs/c%23/24/c-sharp-6-0-features/46/exception-filters

## Usar un bloque finalmente
El bloque `finally { ... }` de `try-finally` o `try-catch-finally` siempre se ejecutará, independientemente de si se produjo o no una excepción (excepto cuando se haya lanzado una `StackOverflowException` o se haya realizado una llamada). hecho a `Environment.FailFast()`).

Se puede utilizar para liberar o limpiar los recursos adquiridos en el bloque `try { ... }` de forma segura.

    Console.Write("Please enter a filename: ");
    string filename = Console.ReadLine();

    Stream fileStream = null;

    try
    {
        fileStream = File.Open(filename);
    }
    catch (FileNotFoundException)
    {
        Console.WriteLine("File '{0}' could not be found.", filename);
    }
    finally
    {
        if (fileStream != null)
        {
            fileStream.Dispose();
        }
    }


## Filtros de excepción
Desde C# 6.0, las excepciones se pueden filtrar mediante el operador `when`.

Esto es similar a usar un `si` simple pero no desenrolla la pila si no se cumple la condición dentro del `cuando`.

__Ejemplo__
    
    try
    { 
      // ...
    }
    catch (Exception e) when (e.InnerException != null) // Any condition can go in here.
    {
      // ...
    }

La misma información se puede encontrar en las [Características de C# 6.0][1] aquí: [Filtros de excepción][2]


[1]: https://www.wikiod.com/es/docs/c%23/24/c-sharp-6-0-features
[2]: https://www.wikiod.com/es/docs/c%23/24/c-sharp-6-0-features/46/exception-filters#t=201607211048375185447

## Volver a lanzar una excepción dentro de un bloque catch
Dentro de un bloque `catch`, la palabra clave `throw` se puede usar sola, sin especificar un valor de excepción, para *volver a lanzar* la excepción que acaba de ser capturada. Volver a lanzar una excepción permite que la excepción original continúe en la cadena de manejo de excepciones, preservando su pila de llamadas o datos asociados:
 
    try {...}
    catch (Exception ex) {
      // Note: the ex variable is *not* used
      throw;
    }

Un antipatrón común es, en cambio, "lanzar ex", lo que tiene el efecto de limitar la vista del seguimiento de la pila del siguiente controlador de excepciones:

    try {...}
    catch (Exception ex) {
      // Note: the ex variable is thrown
      //  future stack traces of the exception will not see prior calls
      throw ex;  
    }

En general, usar `throw ex` no es deseable, ya que los futuros controladores de excepciones que inspeccionan el seguimiento de la pila solo podrán ver las llamadas tan atrás como `throw ex`. Al omitir la variable `ex` y usar solo la palabra clave `throw`, la excepción original ["burbujeará"][1].

[1]: http://stackoverflow.com/questions/4065893/about-throw-and-exception-bubbling

## Lanzar una excepción desde un método diferente conservando su información
Ocasionalmente, deseará capturar una excepción y lanzarla desde un subproceso o método diferente mientras conserva la pila de excepciones original. Esto se puede hacer con `ExceptionDispatchInfo`:

    using System.Runtime.ExceptionServices;

    void Main()
    {
        ExceptionDispatchInfo capturedException = null;
        try
        {
            throw new Exception();
        }
        catch (Exception ex)
        {
            capturedException = ExceptionDispatchInfo.Capture(ex);
        }
        
        Foo(capturedException);
    }
    
    void Foo(ExceptionDispatchInfo exceptionDispatchInfo)
    {
        // Do stuff
    
        if (capturedException != null)
        {
            // Exception stack trace will show it was thrown from Main() and not from Foo()
            exceptionDispatchInfo.Throw();
        }
    }

## Capturar una excepción
El código puede y debe generar excepciones en circunstancias excepcionales. Ejemplos de esto incluyen:

- Intentar [leer más allá del final de una secuencia][1]
- [No tener los permisos necesarios][2] para acceder a un archivo
- Intentar realizar una operación no válida, como [dividir por cero][3]
- [Se produce un tiempo de espera][4] al descargar un archivo de Internet

La persona que llama puede manejar estas excepciones "atrapándolas", y solo debe hacerlo cuando:

- Puede resolver efectivamente la circunstancia excepcional o recuperarla adecuadamente, o;
- Puede proporcionar un contexto adicional a la excepción que sería útil si es necesario volver a lanzar la excepción (los controladores de excepciones capturan las excepciones que se vuelven a lanzar más arriba en la pila de llamadas)

Cabe señalar que elegir *no* para detectar una excepción es perfectamente válido si la intención es que se maneje en un nivel superior.

La captura de una excepción se realiza envolviendo el código potencialmente arrojable en un bloque `try { ... }` de la siguiente manera, y capturando las excepciones que puede manejar en un bloque `catch (ExceptionType) { ... }`:

    Console.Write("Please enter a filename: ");
    string filename = Console.ReadLine();

    Stream fileStream;

    try
    {
        fileStream = File.Open(filename);
    }
    catch (FileNotFoundException)
    {
        Console.WriteLine("File '{0}' could not be found.", filename);
    }


[1]: https://msdn.microsoft.com/en-us/library/system.io.endofstreamexception(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.unauthorizedaccessexception(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.dividebyzeroexception(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.net.webexception.aspx

