---
title: "Declaración de uso"
slug: "declaracion-de-uso"
draft: false
images: []
weight: 9015
type: docs
toc: true
---

Proporciona una sintaxis conveniente que garantiza el uso correcto de los objetos [IDisposable](https://docs.microsoft.com/en-us/dotnet/api/system.idisposable?view=netframework-4.7).



## Sintaxis
- usando (desechable) { }
- usando (IDisposable desechable = new MyDisposable()) { }


El objeto en la instrucción `using` debe implementar la interfaz `IDisposable`.

    using(var obj = new MyObject())
    {
    }
    
    class MyObject : IDisposable
    {
        public void Dispose()
        {
            // Cleanup
        }
    }

Se pueden encontrar ejemplos más completos para la implementación de `IDisposable` en los [documentos de MSDN][1].


[1]: https://msdn.microsoft.com/en-us/library/fs2xkftw(v=vs.110).aspx
  

## Gotcha: devolver el recurso que está desechando
La siguiente es una mala idea porque eliminaría la variable `db` antes de devolverla.

    public IDBContext GetDBContext()
    {
        using (var db = new DBContext())
        {
            return db;
        }
    }

Esto también puede crear errores más sutiles:

    public IEnumerable<Person> GetPeople(int age)
    {
        using (var db = new DBContext())
        {
            return db.Persons.Where(p => p.Age == age);
        }
    }

Esto parece estar bien, pero el problema es que la evaluación de la expresión LINQ es perezosa y posiblemente solo se ejecutará más tarde cuando el 'DBContext' subyacente ya se haya eliminado.

En resumen, la expresión no se evalúa antes de salir de `using`. Una posible solución a este problema, que todavía hace uso de `using`, es hacer que la expresión se evalúe inmediatamente llamando a un método que enumerará el resultado. Por ejemplo, `ToList()`, `ToArray()`, etc. Si está utilizando la versión más reciente de Entity Framework, puede usar las contrapartes `async` como `ToListAsync()` o `ToArrayAsync()`.

A continuación encontrará el ejemplo en acción:

    public IEnumerable<Person> GetPeople(int age)
    {
        using (var db = new DBContext())
        {
            return db.Persons.Where(p => p.Age == age).ToList();
        }
    }

Sin embargo, es importante tener en cuenta que al llamar a `ToList()` o `ToArray()`, la expresión se evaluará con entusiasmo, lo que significa que todas las personas con la edad especificada se cargarán en la memoria incluso si no itera. en ellos.

## Uso de los fundamentos de las declaraciones
`using` es azúcar sintáctico que le permite garantizar que un recurso se limpie sin necesidad de un bloque explícito `try-finally`. Esto significa que su código será mucho más limpio y no perderá recursos no administrados.

Patrón de limpieza `Dispose` estándar, para objetos que implementan la interfaz `IDisposable` (que la clase base `Stream` de `FileStream` hace en .NET):

    int Foo()
    {
        var fileName = "file.txt";

        {
            FileStream disposable = null;

            try
            {
                disposable = File.Open(fileName, FileMode.Open);

                return disposable.ReadByte();
            }
            finally
            {
                // finally blocks are always run
                if (disposable != null) disposable.Dispose();
            }
        }
    }

`using` simplifica tu sintaxis al ocultar el `try-finally` explícito:

    int Foo()
    {
        var fileName = "file.txt";

        using (var disposable = File.Open(fileName, FileMode.Open))
        {
            return disposable.ReadByte();
        }
        // disposable.Dispose is called even if we return earlier
    }

Al igual que los bloques `finally` siempre se ejecutan independientemente de los errores o devoluciones, `using` siempre llama a `Dispose()`, incluso en caso de error:

    int Foo()
    {
        var fileName = "file.txt";

        using (var disposable = File.Open(fileName, FileMode.Open))
        {
            throw new InvalidOperationException();
        }
        // disposable.Dispose is called even if we throw an exception earlier
    }
**Nota:**
Dado que se garantiza que `Dispose` se llamará independientemente del flujo de código, es una buena idea asegurarse de que `Dispose` nunca arroje una excepción cuando implemente `IDisposable`. De lo contrario, una excepción real sería anulada por la nueva excepción, lo que resultaría en una pesadilla de depuración.

Volviendo de usar bloque
---

    using ( var disposable = new DisposableItem() )
    {
        return disposable.SomeProperty;
    }

Debido a la semántica de `try..finally` a la que se traduce el bloque `using`, la instrucción `return` funciona como se esperaba: el valor devuelto se evalúa antes de que se ejecute el bloque `finally` y se elimine el valor. El orden de evaluación es el siguiente:

1. Evaluar el cuerpo `try`
2. Evaluar y almacenar en caché el valor devuelto
3. Ejecutar bloque finalmente
4. Devolver el valor de retorno almacenado en caché

Sin embargo, no puede devolver la variable "desechable" en sí misma, ya que contendría una referencia desechada no válida; consulte [ejemplo relacionado][1].


[1]: https://www.wikiod.com/es/docs/c%23/38/using-statement/327/gotcha-returning-the-resource-which-you-are-disposing#t=201608220847304515557

## Múltiples instrucciones de uso con un bloque
Es posible utilizar múltiples sentencias `using` anidadas sin agregar múltiples niveles de llaves anidadas. Por ejemplo:

    using (var input = File.OpenRead("input.txt"))
    {
        using (var output = File.OpenWrite("output.txt"))
        {
            input.CopyTo(output);
        } // output is disposed here
    } // input is disposed here

Una alternativa es escribir:

    using (var input = File.OpenRead("input.txt"))
    using (var output = File.OpenWrite("output.txt"))
    {
        input.CopyTo(output);
    } // output and then input are disposed here

Lo cual es exactamente equivalente al primer ejemplo.

*Nota:* Las declaraciones `using` anidadas pueden activar la regla de análisis de código de Microsoft [CS2002][1] (consulte [esta respuesta][2] para obtener aclaraciones) y generar una advertencia. Como se explica en la respuesta vinculada, generalmente es seguro anidar las declaraciones `using`.

Cuando los tipos dentro de la instrucción `using` son del mismo tipo, puede delimitarlos con comas y especificar el tipo solo una vez (aunque esto es poco común):

    using (FileStream file = File.Open("MyFile.txt"), file2 = File.Open("MyFile2.txt"))
    {
    }

Esto también se puede usar cuando los tipos tienen una jerarquía compartida:

    using (Stream file = File.Open("MyFile.txt"), data = new MemoryStream())
    {
    }

La palabra clave `var` *no* se puede usar en el ejemplo anterior. Se produciría un error de compilación. Incluso la declaración separada por comas no funcionará cuando las variables declaradas tengan tipos de diferentes jerarquías.

[1]: https://msdn.microsoft.com/en-us/library/ms182334.aspx
[2]: http://stackoverflow.com/a/22323027/501011

## Gotcha: excepción en el método Dispose que enmascara otros errores en el uso de bloques
Considere el siguiente bloque de código.

    try
    {
        using (var disposable = new MyDisposable())
        {
            throw new Exception("Couldn't perform operation.");
        }
    }
    catch (Exception ex)
    {
        Console.WriteLine(ex.Message);
    }

    class MyDisposable : IDisposable
    {
        public void Dispose()
        {
            throw new Exception("Couldn't dispose successfully.");
        }
    }

Es posible que espere ver "No se pudo realizar la operación" impreso en la Consola, pero en realidad verá "No se pudo desechar correctamente". ya que el método Dispose todavía se llama incluso después de que se lanza la primera excepción.

Vale la pena ser consciente de esta sutileza, ya que puede estar enmascarando el error real que impidió que el objeto se desechara y que sea más difícil de depurar.



## Las declaraciones de uso son nulas seguras
No tienes que comprobar el objeto `IDisposable` para `null`. `using` no lanzará una excepción y no se llamará a `Dispose()`:

    DisposableObject TryOpenFile()
    {
        return null;
    }

    // disposable is null here, but this does not throw an exception 
    using (var disposable = TryOpenFile())
    {
        // this will throw a NullReferenceException because disposable is null
        disposable.DoSomething(); 

        if(disposable != null)
        {
            // here we are safe because disposable has been checked for null
            disposable.DoSomething();
        }
    }

## Uso de Dispose Syntax para definir un alcance personalizado
Para algunos casos de uso, puede usar la sintaxis `using` para ayudar a definir un alcance personalizado. Por ejemplo, puede definir la siguiente clase para ejecutar código en una referencia cultural específica.

    public class CultureContext : IDisposable
    {
        private readonly CultureInfo originalCulture;

        public CultureContext(string culture)
        {
            originalCulture = CultureInfo.CurrentCulture;
            Thread.CurrentThread.CurrentCulture = new CultureInfo(culture);
        }

        public void Dispose()
        {
            Thread.CurrentThread.CurrentCulture = originalCulture;
        }
    }

A continuación, puede usar esta clase para definir bloques de código que se ejecutan en una referencia cultural específica.

    Thread.CurrentThread.CurrentCulture = new CultureInfo("en-US");

    using (new CultureContext("nl-NL"))
    {
        // Code in this block uses the "nl-NL" culture
        Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 25-12-2016 00:00:00
    }

    using (new CultureContext("es-ES"))
    {        
        // Code in this block uses the "es-ES" culture
        Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 25/12/2016 0:00:00
    }

    // Reverted back to the original culture
    Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 12/25/2016 12:00:00 AM

Nota: como no usamos la instancia `CultureContext` que creamos, no le asignamos una variable.

Esta técnica es utilizada por ``BeginForm`` [ayudante][1] en ASP.NET MVC.

[1]: https://msdn.microsoft.com/en-us/library/dd410596%28v=vs.100%29.aspx

## Uso de declaraciones y conexiones de base de datos
La palabra clave `using` asegura que el recurso definido dentro de la declaración solo existe dentro del alcance de la declaración misma. Cualquier recurso definido dentro de la declaración debe implementar la interfaz `IDisposable`.

Estos son increíblemente importantes cuando se trata de cualquier conexión que implemente la interfaz `IDisposable`, ya que puede garantizar que las conexiones no solo se cierren correctamente, sino que sus recursos se liberen después de que la declaración `using` esté fuera del alcance.


**Clases de datos `IDisposable` comunes**
---

Muchas de las siguientes son clases relacionadas con datos que implementan la interfaz `IDisposable` y son candidatas perfectas para una declaración `using`:

- `SqlConnection`,`SqlCommand`,`SqlDataReader`, etc.
- `OleDbConnection`, `OleDbCommand`, `OleDbDataReader`, etc.
- `MySqlConnection`, `MySqlCommand`, `MySqlDbDataReader`, etc.
- `ContextoDb`

Todos estos se usan comúnmente para acceder a datos a través de C# y se encontrarán comúnmente en la creación de aplicaciones centradas en datos. Se puede esperar que muchas otras clases que no se mencionan que implementan las mismas clases `FooConnection`,`FooCommand`,`FooDataReader` se comporten de la misma manera.

**Patrón de acceso común para conexiones ADO.NET**
----

Un patrón común que se puede usar al acceder a sus datos a través de una conexión ADO.NET podría tener el siguiente aspecto:

    // This scopes the connection (your specific class may vary)
    using(var connection = new SqlConnection("{your-connection-string}")
    {
        // Build your query
        var query = "SELECT * FROM YourTable WHERE Property = @property");
        // Scope your command to execute
        using(var command = new SqlCommand(query, connection))
        {
             // Open your connection
             connection.Open();

             // Add your parameters here if necessary

             // Execute your query as a reader (again scoped with a using statement)
             using(var reader = command.ExecuteReader())
             {
                   // Iterate through your results here
             }
        }
    }

O si solo estuviera realizando una actualización simple y no necesitara un lector, se aplicaría el mismo concepto básico:

    using(var connection = new SqlConnection("{your-connection-string}"))
    {
         var query = "UPDATE YourTable SET Property = Value WHERE Foo = @foo";
         using(var command = new SqlCommand(query,connection))
         {
              connection.Open();
              
              // Add parameters here
              
              // Perform your update
              command.ExecuteNonQuery();
         }
    }

**Uso de declaraciones con contextos de datos**
---

Muchos ORM, como Entity Framework, exponen clases de abstracción que se utilizan para interactuar con bases de datos subyacentes en forma de clases como `DbContext`. Estos contextos generalmente también implementan la interfaz 'IDisposable' y deben aprovechar esto a través de declaraciones 'usando' cuando sea posible:

    using(var context = new YourDbContext())
    {
          // Access your context and perform your query
          var data = context.Widgets.ToList();
    }




## Ejecutar código en contexto de restricción
Si tiene un código (una *rutina*) que desea ejecutar en un contexto específico (restricción), puede usar la inyección de dependencia.

El siguiente ejemplo muestra la restricción de ejecutar bajo una conexión SSL abierta. Esta primera parte estaría en su biblioteca o marco, que no expondrá al código del cliente.

    public static class SSLContext
    {
        // define the delegate to inject
        public delegate void TunnelRoutine(BinaryReader sslReader, BinaryWriter sslWriter);

        // this allows the routine to be executed under SSL
        public static void ClientTunnel(TcpClient tcpClient, TunnelRoutine routine)
        {
            using (SslStream sslStream = new SslStream(tcpClient.GetStream(), true, _validate))
            {
                sslStream.AuthenticateAsClient(HOSTNAME, null, SslProtocols.Tls, false);

                if (!sslStream.IsAuthenticated)
                {
                    throw new SecurityException("SSL tunnel not authenticated");
                }

                if (!sslStream.IsEncrypted)
                {
                    throw new SecurityException("SSL tunnel not encrypted");
                }

                using (BinaryReader sslReader = new BinaryReader(sslStream))
                using (BinaryWriter sslWriter = new BinaryWriter(sslStream))
                {
                    routine(sslReader, sslWriter);
                }
            }
        }
    }

Ahora el código de cliente que quiere hacer algo bajo SSL pero no quiere manejar todos los detalles de SSL. Ahora puede hacer lo que quiera dentro del túnel SSL, por ejemplo, intercambiar una clave simétrica:

    public void ExchangeSymmetricKey(BinaryReader sslReader, BinaryWriter sslWriter)
    {
        byte[] bytes = new byte[8];
        (new RNGCryptoServiceProvider()).GetNonZeroBytes(bytes);
        sslWriter.Write(BitConverter.ToUInt64(bytes, 0));
    }

Esta rutina se ejecuta de la siguiente manera:

    SSLContext.ClientTunnel(tcpClient, this.ExchangeSymmetricKey);

Para hacer esto, necesita la cláusula `using()` porque es la única forma (aparte de un bloque `try..finally`) de garantizar que el código del cliente (`ExchangeSymmetricKey`) nunca salga sin desechar correctamente los elementos desechables. recursos. Sin la cláusula `using()`, nunca sabría si una rutina podría romper la restricción del contexto para deshacerse de esos recursos.


