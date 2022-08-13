---
title: "Características de C# 6.0"
slug: "caracteristicas-de-c-60"
draft: false
images: []
weight: 994
type: docs
toc: true
---

Esta sexta iteración del lenguaje C# la proporciona el compilador Roslyn. Este compilador salió con la versión 4.6 de .NET Framework, sin embargo, puede generar código de manera compatible con versiones anteriores para permitir apuntar a versiones anteriores del marco. El código de la versión 6 de C# se puede compilar de manera totalmente compatible con versiones anteriores de .NET 4.0. También se puede usar para marcos anteriores, sin embargo, algunas características que requieren soporte de marco adicional pueden no funcionar correctamente.

La sexta versión de C# se lanzó en julio de 2015 junto con Visual Studio 2015 y .NET 4.6.

Además de agregar algunas características nuevas del lenguaje, incluye una reescritura completa del compilador. Anteriormente, `csc.exe` era una aplicación Win32 nativa escrita en C++, con C# 6 ahora es una aplicación administrada por .NET escrita en C#. Esta reescritura se conoció como proyecto "Roslyn" y el código ahora es de código abierto y está disponible en [GitHub][1].


[1]: https://github.com/dotnet/roslyn

## Filtros de excepción
<!-- idioma-todo: lang-cs -->
[Los filtros de excepción][1] brindan a los desarrolladores la capacidad de agregar una condición (en forma de una expresión 'booleana') a un bloque [catch][2], lo que permite que 'catch' se ejecute solo si la condición se evalúa como ' cierto`.

Los filtros de excepción permiten la propagación de la información de depuración en la excepción original, mientras que el uso de una instrucción `if` dentro de un bloque `catch` y volver a lanzar la excepción detiene la propagación de la información de depuración en la excepción original. Con los filtros de excepción, la excepción continúa propagándose hacia arriba en la pila de llamadas *a menos que* se cumpla la condición. Como resultado, los filtros de excepción facilitan mucho la experiencia de depuración. En lugar de detenerse en la declaración `throw`, el depurador se detendrá en la declaración que arroja la excepción, conservando el estado actual y todas las variables locales. Los volcados de memoria se ven afectados de manera similar.

>Los filtros de excepción han sido compatibles con [**CLR**][3] desde el principio y han sido accesibles desde VB.NET y F# durante más de una década al exponer una parte del modelo de manejo de excepciones de CLR. Solo después del lanzamiento de C# 6.0, la funcionalidad también estuvo disponible para los desarrolladores de C#.
---

Uso de filtros de excepción
-

Los filtros de excepción se utilizan agregando una cláusula `when` a la expresión `catch`. Es posible utilizar cualquier expresión que devuelva un `bool` en una cláusula `when` (excepto [await][4]). Se puede acceder a la variable de excepción declarada `ex` desde la cláusula `when`:

    var SqlErrorToIgnore = 123;
    try
    {
        DoSQLOperations();
    }
    catch (SqlException ex) when (ex.Number != SqlErrorToIgnore)
    {
        throw new Exception("An error occurred accessing the database", ex);
    }

Se pueden combinar varios bloques `catch` con cláusulas `when`. La primera cláusula `when` que devuelve `true` hará que se detecte la excepción. Su bloque `catch` será ingresado, mientras que las otras cláusulas `catch` serán ignoradas (sus cláusulas `when` no serán evaluadas). Por ejemplo:

    try
    { ... }
    catch (Exception ex) when (someCondition) //If someCondition evaluates to true,
                                              //the rest of the catches are ignored.
    { ... }
    catch (NotImplementedException ex) when (someMethod()) //someMethod() will only run if
                                                           //someCondition evaluates to false
    { ... }
    catch(Exception ex) // If both when clauses evaluate to false
    { ... }

---
Cláusula arriesgada cuando
-

>**Precaución**
>
>Puede ser arriesgado usar filtros de excepción: cuando se lanza una `Excepción` desde dentro de la cláusula `when`, la `Exception` de la cláusula `when` se ignora y se trata como `falsa`. Este enfoque permite a los desarrolladores escribir la cláusula `when` sin ocuparse de los casos inválidos.

El siguiente ejemplo ilustra tal escenario:

    public static void Main()
    {
        int a = 7;
        int b = 0;
        try
        {
            DoSomethingThatMightFail();
        }
        catch (Exception ex) when (a / b == 0)
        {
            // This block is never reached because a / b throws an ignored
            // DivideByZeroException which is treated as false.
        }
        catch (Exception ex)
        {
            // This block is reached since the DivideByZeroException in the 
            // previous when clause is ignored.
        }
    }

    public static void DoSomethingThatMightFail()
    {
        // This will always throw an ArgumentNullException.
        Type.GetType(null);
    }

[Ver demostración][5]

Tenga en cuenta que los filtros de excepción evitan los confusos problemas de número de línea asociados con el uso de `throw` cuando el código fallido está dentro de la misma función. Por ejemplo, en este caso, el número de línea se notifica como 6 en lugar de 3:

    1. int a = 0, b = 0;
    2. try {
    3.     int c = a / b;
    4. }
    5. catch (DivideByZeroException) {
    6.     throw;
    7. }

El número de línea de excepción se informa como 6 porque el error se detectó y se volvió a generar con la instrucción `throw` en la línea 6.

No ocurre lo mismo con los filtros de excepción:

    1. int a = 0, b = 0;
    2. try {
    3.     int c = a / b;
    4. }
    5. catch (DivideByZeroException) when (a != 0) {
    6.     throw;
    7. }

En este ejemplo, `a` es 0, luego se ignora la cláusula `catch` pero se informa 3 como número de línea. Esto se debe a que **no desenrollan la pila**. Más específicamente, la excepción *no se detecta* en la línea 5 porque `a` de hecho es igual a `0` y, por lo tanto, no hay posibilidad de que la excepción se vuelva a lanzar en la línea 6 porque la línea 6 no se ejecuta.

---

Registro como efecto secundario
-

Las llamadas a métodos en la condición pueden causar efectos secundarios, por lo que los filtros de excepción se pueden usar para ejecutar código en excepciones sin detectarlas. Un ejemplo común que aprovecha esto es un método `Log` que siempre devuelve `falso`. Esto permite rastrear la información de registro durante la depuración sin necesidad de volver a generar la excepción.

>**Tenga en cuenta que** si bien esta parece ser una forma cómoda de iniciar sesión, puede ser riesgosa, especialmente si se utilizan ensamblajes de registro de terceros. Estos pueden arrojar excepciones durante el inicio de sesión en situaciones no obvias que pueden no detectarse fácilmente (consulte la cláusula **Arriesgada `cuando (...)`** anterior).

<pre><código>intentar
{
    DoSomethingThatMightFail(s);
}
catch (Excepción, por ejemplo) <b>cuando</b> (Registro (por ejemplo, "Se produjo un error"))
{
    // This catch block will never be reached
}

// ...

Registro booleano estático (Excepción ex, mensaje de cadena, objeto de parámetros [] argumentos)
{
    Debug.Print(message, args);
    return false;
}</código></pre>

[Ver demostración][6]

El enfoque común en versiones anteriores de C# era registrar y volver a generar la excepción.

<!-- si la versión [lt 6.0] -->
    try
    {
        DoSomethingThatMightFail(s);
    }
    catch (Exception ex)
    {
         Log(ex, "An error occurred");
         throw;
    }

    // ...

    static void Log(Exception ex, string message, params object[] args)
    {
        Debug.Print(message, args);
    }

[Ver demostración][7]
<!-- versión final si -->

---

El bloque `finalmente`
=

El bloque [`finally`][8] se ejecuta cada vez que se lanza la excepción o no. Una sutileza con las expresiones en `when` es que los filtros de excepción se ejecutan más arriba en la pila *antes* de ingresar a los bloques `finally` internos. Esto puede causar resultados y comportamientos inesperados cuando el código intenta modificar el estado global (como el usuario o la cultura del subproceso actual) y lo vuelve a configurar en un bloque "finalmente".

Ejemplo: bloque `finalmente`
-

    private static bool Flag = false;

    static void Main(string[] args)
    {
        Console.WriteLine("Start");
        try
        {
            SomeOperation();
        }
        catch (Exception) when (EvaluatesTo())
        {
            Console.WriteLine("Catch");
        }
        finally
        {
            Console.WriteLine("Outer Finally");
        }
    }

    private static bool EvaluatesTo()
    {
        Console.WriteLine($"EvaluatesTo: {Flag}");
        return true;
    }

    private static void SomeOperation()
    {
        try
        {
            Flag = true;
            throw new Exception("Boom");
        }
        finally
        {
            Flag = false;
            Console.WriteLine("Inner Finally");
        }
    }

Salida producida:

>Empezar
Evalúa a: Verdadero
interior finalmente
Captura
exterior finalmente

[Ver demostración][9]

En el ejemplo anterior, si el método `SomeOperation` no desea "filtrar" los cambios de estado global a las cláusulas `when` de la persona que llama, también debe contener un bloque `catch` para modificar el estado. Por ejemplo:

    private static void SomeOperation()
    {
        try
        {
            Flag = true;
            throw new Exception("Boom");
        }
        catch
        {
           Flag = false;
           throw;
        }
        finally
        {
            Flag = false;
            Console.WriteLine("Inner Finally");
        }
    }

También es común ver clases auxiliares [`IDisposable`][10] que aprovechan la semántica de [usar][11] bloques para lograr el mismo objetivo, ya que siempre se llamará a `IDisposable.Dispose` antes de que se llame una excepción dentro de un ` el bloque using` comienza a burbujear en la pila.


[1]: https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6#exception-filters
[2]: https://www.wikiod.com/es/docs/c%23/26/keywords/148/try-catch-finally-throw
[3]: https://msdn.microsoft.com/en-us/library/8bs2ecf4(v=vs.110).aspx
[4]: https://www.wikiod.com/es/docs/c%23/26/keywords/5993/async-await
[5]: https://dotnetfiddle.net/Iex6DP
[6]: https://dotnetfiddle.net/pqPc7B
[7]: https://dotnetfiddle.net/kEWLue
[8]: https://www.wikiod.com/es/docs/c%23/40/exception-handling/172/finally-block
[9]: https://ideone.com/gxfBA8
[10]: https://www.wikiod.com/es/docs/c%23/1795/idisposable-interface
[11]: https://www.wikiod.com/es/docs/c%23/26/keywords/5062/using

## Interpolación de cadenas
La interpolación de cadenas permite al desarrollador combinar "variables" y texto para formar una cadena.
___

# Ejemplo básico

Se crean dos variables `int`: `foo` y `bar`.

    int foo = 34;
    int bar = 42;
    
    string resultString = $"The foo is {foo}, and the bar is {bar}.";

    Console.WriteLine(resultString);

**Producción**:
>El foo es 34, y el bar es 42.

[Ver demostración][3]

Todavía se pueden usar llaves dentro de cadenas, así:
<pre><código>var foo = 34;
donde barra = 42;

// Notación de interpolación de cadenas (nuevo estilo)
Console.WriteLine($"El foo es <b>{{foo}}</b>, y la barra es <b>{{bar}}</b>.");</code></pre>

Esto produce la siguiente salida:

>El foo es {foo} y la barra es {bar}.

___

# Uso de la interpolación con literales de cadena textuales

El uso de `@` antes de la cadena hará que la cadena se interprete palabra por palabra. Entonces, p. Los caracteres Unicode o los saltos de línea permanecerán exactamente como se escribieron. Sin embargo, esto no afectará las expresiones en una cadena interpolada como se muestra en el siguiente ejemplo: <pre><code>Console.WriteLine($@"En caso de que no esté claro:
\u00B9
el foo
es <b>{foo}</b>,
y la barra
es <b>{bar}</b>.");</code></pre>
Producción:
>Por si no te quedó claro:
\u00B9
el foo
es 34,
y la barra
es 42

[Ver demostración][4]

___

# Expresiones
Con la interpolación de cadenas, también se pueden evaluar *expresiones* entre llaves `{}`. El resultado se insertará en la ubicación correspondiente dentro de la cadena. Por ejemplo, para calcular el máximo de `foo` y `bar` e insertarlo, use `Math.Max` entre llaves:<pre><code>Console.WriteLine($"Y el mayor es: <b >{ Math.Max(foo, bar) }</b>");</code></pre>

Producción:

>Y el mayor es: 42

*Nota: cualquier espacio en blanco inicial o final (incluidos espacios, tabulaciones y CRLF/nueva línea) entre la llave y la expresión se ignora por completo y no se incluye en la salida*

[Ver demostración][5]

Como otro ejemplo, las variables se pueden formatear como moneda:<pre><code>Console.WriteLine($"Foo formateado como moneda con 4 decimales: <b>{foo:c4}</b>");< /código></pre>

Producción:

>Foo formateado como moneda con 4 decimales: $34.0000

[Ver demostración][6]

O se pueden formatear como fechas:<pre><code>Console.WriteLine($"Hoy es: <b>{DateTime.Today:dddd, MMMM dd - aaaa}</b>");</code>< /pre>

Producción:

>Hoy es: lunes, 20 de julio - 2015

[Ver demostración][7]

Las declaraciones con un [Operador condicional (ternario)][8] también se pueden evaluar dentro de la interpolación. Sin embargo, estos deben estar entre paréntesis, ya que los dos puntos se usan para indicar el formato como se muestra arriba:

<pre><code>Console.WriteLine($"{(foo > bar ? "¡Foo es más grande que bar!" : "¡Bar es más grande que foo!")}");</code></pre>

Producción:
>¡El bar es más grande que foo!

[Ver demostración][9]

Las expresiones condicionales y los especificadores de formato se pueden mezclar:

    Console.WriteLine($"Environment: {(Environment.Is64BitProcess ? 64 : 32):00'-bit'} process");

Producción:

> Entorno: proceso de 32 bits

___

# Secuencias de escape
Escapar de los caracteres de barra invertida (`\`) y comillas (`"`) funciona exactamente igual en cadenas interpoladas que en cadenas no interpoladas, tanto para literales de cadena literales como no literales:
<pre><code>Console.WriteLine($"Foo es: <b>{foo}</b>. En una cadena no literal, necesitamos escapar \" y \\ con barras invertidas.");
Console.WriteLine($@"Foo is: <b>{foo}</b>. En una cadena textual, necesitamos escapar "" con una comilla extra, pero no necesitamos escapar \");
</código></pre>

Producción:
>Foo es 34. En una cadena no literal, necesitamos escapar " y \ con barras invertidas.
Foo es 34. En una cadena textual, necesitamos escapar " con una comilla adicional, pero no necesitamos escapar \

Para incluir una llave `{` o `}` en una cadena interpolada, use dos llaves `{{` o `}}`:<pre><code>$"{{foo}} es: <b>{ foo}</b>"</code></pre>

Producción:
>{foo} es: 34

[Ver demostración][10]
___

# tipo FormattableString
El tipo de una expresión de interpolación de cadena `$"..."` [no siempre][11] es una cadena simple. El compilador decide qué tipo asignar según el contexto: <pre><code>string s = $"hello, <b>{name}</b>";
System.FormattableString s = $"Hola, <b>{nombre}</b>";
System.IFormattable s = $"Hola, <b>{nombre}</b>";</code></pre>

Este es también el orden de preferencia de tipo cuando el compilador necesita elegir qué método sobrecargado se va a llamar.

Un [nuevo tipo][12], `System.FormattableString`, representa una cadena de formato compuesto, junto con los argumentos a formatear. Use esto para escribir aplicaciones que manejen los argumentos de interpolación específicamente:

    public void AddLogItem(FormattableString formattableString)
    {
        foreach (var arg in formattableString.GetArguments())
        {
            // do something to interpolation argument 'arg'
        }

        // use the standard interpolation and the current culture info
        // to get an ordinary String:
        var formatted = formattableString.ToString();

        // ...
    }
Llame al método anterior con:<pre><code>AddLogItem($"El foo es <b>{foo}</b>, y la barra es <b>{bar}</b>.");</ código></pre>
Por ejemplo, se podría optar por no incurrir en el costo de rendimiento de formatear la cadena si el nivel de registro ya iba a filtrar el elemento de registro.<hr>
# Conversiones implícitas
Hay conversiones de tipo implícitas a partir de una cadena interpolada:<pre><code>var s = $"Foo: <b>{foo}</b>";
System.IFormattable s = $"Foo: <b>{foo}</b>";</code></pre>
También puede producir una variable `IFormattable` que le permita convertir la cadena con un contexto invariable:<pre><code>var s = $"Bar: <b>{bar}</b>";
System.FormattableString s = $"Barra: <b>{bar}</b>";</code></pre><hr>
# Métodos de cultivo actuales e invariantes
Si el análisis de código está activado, todas las cadenas interpoladas generarán una advertencia [CA1305][13] (Especifique `IFormatProvider`).
Se puede utilizar un método estático para aplicar la cultura actual.

    public static class Culture
    {
        public static string Current(FormattableString formattableString)
        {
            return formattableString?.ToString(CultureInfo.CurrentCulture);
        }
        public static string Invariant(FormattableString formattableString)
        {
            return formattableString?.ToString(CultureInfo.InvariantCulture);
        }
    }
Luego, para producir una cadena correcta para la cultura actual, simplemente use la expresión: <pre><code>Culture.Current($"interpolated <b>{typeof(string).Name}</b> string").
Culture.Invariant($"interpolated <b>{typeof(string).Name}</b> string.")</code></pre>
**Nota**: `Current` e `Invariant` no se pueden crear como métodos de extensión porque, de forma predeterminada, el compilador asigna el tipo `String` a *expresión de cadena interpolada*, lo que hace que el siguiente código no se pueda compilar:

    $"interpolated {typeof(string).Name} string.".Current();
La clase `FormattableString` ya contiene el método `Invariant()`, por lo que la forma más sencilla de cambiar a la referencia cultural invariable es confiar en `using static`:<pre><code>using static System.FormattableString;

cadena invariante = Invariante($"Ahora = <b>{DateTime.Now}</b>");
cadena actual = $"Ahora = <b>{DateTime.Now}</b>";</code></pre><hr>
# Detrás de escena
Las cadenas interpoladas son solo un azúcar sintáctico para `String.Format()`. El compilador ([Roslyn][14]) lo convertirá en un `String.Format` detrás de escena:

    var text = $"Hello {name + lastName}";
    
Lo anterior se convertirá en algo como esto:

    string text = string.Format("Hello {0}", new object[] {
        name + lastName
    });
<hr>

# Interpolación de cadenas y Linq

Es posible usar cadenas interpoladas en declaraciones de Linq para aumentar aún más la legibilidad.

    var fooBar = (from DataRow x in fooBarTable.Rows
              select string.Format("{0}{1}", x["foo"], x["bar"])).ToList();

Se puede reescribir como:

    var fooBar = (from DataRow x in fooBarTable.Rows
              select $"{x["foo"]}{x["bar"]}").ToList();

# Cuerdas interpoladas reutilizables
Con `string.Format`, puede crear cadenas de formato reutilizables:

    public const string ErrorFormat = "Exception caught:\r\n{0}";

    // ...

    Logger.Log(string.Format(ErrorFormat, ex));

Sin embargo, las cadenas interpoladas no se compilarán con marcadores de posición que hagan referencia a variables inexistentes. Lo siguiente no compilará:

    public const string ErrorFormat = $"Exception caught:\r\n{error}";
    // CS0103: The name 'error' does not exist in the current context

En su lugar, crea una `Func<>` que consuma variables y devuelva una `String`:

    public static Func<Exception, string> FormatError =
        error => $"Exception caught:\r\n{error}";

    // ...

    Logger.Log(FormatError(ex));
<hr>

# Interpolación y localización de cadenas

Si está localizando su aplicación, es posible que se pregunte si es posible utilizar la interpolación de cadenas junto con la localización. De hecho, sería bueno tener la posibilidad de almacenar en archivos de recursos `String`s como: <pre><code>"Mi nombre es <b>{name} {middlename} {surname}</b>"</ código></pre>
en lugar del mucho menos legible:

    "My name is {0} {1} {2}"
El proceso de interpolación `String` ocurre *en tiempo de compilación*, a diferencia del formato de cadena con `string.Format` que ocurre *en tiempo de ejecución*. Las expresiones en una cadena interpolada deben hacer referencia a nombres en el contexto actual y deben almacenarse en archivos de recursos. Eso significa que si desea utilizar la localización, debe hacerlo así:

    var FirstName = "John";
    
    // method using different resource file "strings"
    // for French ("strings.fr.resx"), German ("strings.de.resx"), 
    // and English ("strings.en.resx")
    void ShowMyNameLocalized(string name, string middlename = "", string surname = "")
    {
        // get localized string
        var localizedMyNameIs = Properties.strings.Hello;
        // insert spaces where necessary
        name = (string.IsNullOrWhiteSpace(name) ? "" : name + " ");
        middlename = (string.IsNullOrWhiteSpace(middlename) ? "" : middlename + " ");
        surname = (string.IsNullOrWhiteSpace(surname) ? "" : surname + " ");
        // display it
        Console.WriteLine($"{localizedMyNameIs} {name}{middlename}{surname}".Trim());
    }

    // switch to French and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("fr-FR");
    ShowMyNameLocalized(FirstName);

    // switch to German and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("de-DE");
    ShowMyNameLocalized(FirstName);

    // switch to US English and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("en-US");
    ShowMyNameLocalized(FirstName);

Si las cadenas de recursos para los idiomas utilizados anteriormente se almacenan correctamente en los archivos de recursos individuales, debería obtener el siguiente resultado:
> Hola, mi nombre es Juan<br/>
> Hola, mi nombre es Juan<br/>
> Hola, mi nombre es Juan<br/>

**Tenga en cuenta** que esto implica que el nombre sigue a la cadena localizada en todos los idiomas. Si ese no es el caso, debe agregar marcadores de posición a las cadenas de recursos y modificar la función anterior o debe consultar la información cultural en la función y proporcionar una declaración de cambio de caso que contenga los diferentes casos.
Para obtener más detalles sobre los archivos de recursos, consulte [Cómo usar la localización en C#](https://stackoverflow.com/a/1142840/1016343).

Es una buena práctica usar un idioma alternativo predeterminado que la mayoría de la gente entienda, en caso de que no haya una traducción disponible. Sugiero usar el inglés como idioma alternativo predeterminado.

# Interpolación recursiva

Aunque no es muy útil, se permite usar una `cadena` interpolada recursivamente dentro de las llaves de otro:

    Console.WriteLine($"String has {$"My class is called {nameof(MyClass)}.".Length} chars:");
    Console.WriteLine($"My class is called {nameof(MyClass)}.");

Producción:

> String tiene 27 caracteres:

> Mi clase se llama MyClass.

[1]: https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6#string-interpolation
[2]: https://dotnetfiddle.net/0JjwL5
[3]: https://ideone.com/bRFOaV
[4]: https://dotnetfiddle.net/FLs4Ae
[5]: https://ideone.com/qY1Y4B
[6]: https://ideone.com/CPB8UJ
[7]: https://ideone.com/PkjA6k
[8]: https://msdn.microsoft.com/en-us/library/ty67wk28.aspx
[9]: https://ideone.com/sX6tO3
[10]: https://dotnetfiddle.net/BuudHP
[11]: http://stackoverflow.com/questions/38119074
[12]: https://msdn.microsoft.com/en-us/library/system.formattablestring(v=vs.110).aspx
[13]: https://msdn.microsoft.com/en-us/library/ms182190.aspx
[14]: https://github.com/dotnet/roslyn


## Inicializadores de propiedades automáticas
# Introducción

Las propiedades se pueden inicializar con el operador `=` después del cierre `}`. La clase `Coordenada` a continuación muestra las opciones disponibles para inicializar una propiedad:


<!-- si la versión [gte 6.0] -->
    public class Coordinate
    { 
        public int X { get; set; } = 34; // get or set auto-property with initializer
   
        public int Y { get; } = 89;      // read-only auto-property with initializer              
    }
<!-- versión final si -->

---

## Accesorios con diferente visibilidad

Puede inicializar propiedades automáticas que tienen una visibilidad diferente en sus accesos. Aquí hay un ejemplo con un setter protegido:

        public string Name { get; protected set; } = "Cheeze";

El acceso también puede ser `interno`, `interno protegido` o `privado`.

---

## Propiedades de solo lectura

Además de la flexibilidad con la visibilidad, también puede inicializar propiedades automáticas de solo lectura. Aquí hay un ejemplo:

        public List<string> Ingredients { get; } = 
            new List<string> { "dough", "sauce", "cheese" };

Este ejemplo también muestra cómo inicializar una propiedad con un tipo complejo. Además, las propiedades automáticas no pueden ser de solo escritura, lo que también impide la inicialización de solo escritura.

---

# Estilo antiguo (antes de C# 6.0)

Antes de C# 6, esto requería un código mucho más detallado. Estábamos usando una variable adicional llamada propiedad de respaldo para que la propiedad diera un valor predeterminado o para inicializar la propiedad pública como se muestra a continuación,

<!-- si la versión [lt 6.0] -->
    public class Coordinate
    {
        private int _x = 34;
        public int X { get { return _x; } set { _x = value; } }
   
        private readonly int _y = 89;
        public int Y { get { return _y; } }
        
        private readonly int _z;
        public int Z { get { return _z; } }
    
        public Coordinate()
        {
            _z = 42;
        }
    }

***Nota:** Antes de C# 6.0, aún podía inicializar lectura y escritura [**propiedades implementadas automáticamente**][2] (propiedades con un getter y un setter) desde dentro del constructor, pero no podía inicializar el propiedad en línea con su declaración*

[Ver demostración][3]
<!-- versión final si -->

---

# Uso

Los inicializadores deben evaluar expresiones estáticas, al igual que los inicializadores de campo. Si necesita hacer referencia a miembros no estáticos, puede inicializar propiedades en constructores como antes o usar propiedades con cuerpo de expresión. Las expresiones no estáticas, como la siguiente (comentada), generarán un error de compilación:

    
    // public decimal X { get; set; } = InitMe();  // generates compiler error

    decimal InitMe() { return 4m; }

Pero los métodos estáticos **pueden** usarse para inicializar propiedades automáticas:

    public class Rectangle
    {
        public double Length { get; set; } = 1;
        public double Width { get; set; } = 1;
        public double Area { get; set; } = CalculateArea(1, 1);

        public static double CalculateArea(double length, double width)
        {
            return length * width;
        }
    }

Este método también se puede aplicar a propiedades con diferentes niveles de accesores:

    public short Type { get; private set; } = 15;

El inicializador automático de propiedades permite la asignación de propiedades directamente dentro de su declaración. Para las propiedades de solo lectura, se ocupa de todos los requisitos necesarios para garantizar que la propiedad sea inmutable. Considere, por ejemplo, la clase `FingerPrint` en el siguiente ejemplo:

    public class FingerPrint
    {
      public DateTime TimeStamp { get; } = DateTime.UtcNow;

      public string User { get; } =
        System.Security.Principal.WindowsPrincipal.Current.Identity.Name;

      public string Process { get; } =
        System.Diagnostics.Process.GetCurrentProcess().ProcessName;
    }

[Ver demostración][4]

---

# Notas de precaución

Tenga cuidado de no confundir las propiedades automáticas o los inicializadores de campo con [métodos de cuerpo de expresión][5] de apariencia similar que usan `=>` en lugar de `=`, y campos que no incluyen `{ get; }`.

Por ejemplo, cada una de las siguientes declaraciones son diferentes.

    public class UserGroupDto
    {
        // Read-only auto-property with initializer:       
        public ICollection<UserDto> Users1 { get; } = new HashSet<UserDto>();
        
        // Read-write field with initializer:
        public ICollection<UserDto> Users2 = new HashSet<UserDto>();

        // Read-only auto-property with expression body:
        public ICollection<UserDto> Users3 => new HashSet<UserDto>();
    }

Falta `{ obtener; }` en la declaración de propiedad da como resultado un campo público. Tanto la propiedad automática de solo lectura `Users1` como el campo de lectura y escritura `Users2` se inicializan solo una vez, pero un campo público permite cambiar la instancia de colección desde fuera de la clase, lo que generalmente no es deseable. Cambiar una propiedad automática de solo lectura con cuerpo de expresión a una propiedad de solo lectura con inicializador requiere no solo eliminar `>` de `=>`, sino también agregar `{ get; }`.

El símbolo diferente (`=>` en lugar de `=`) en `Users3` da como resultado que cada acceso a la propiedad devuelva una nueva instancia de `HashSet<UserDto>` que, si bien es válido en C# (desde el punto de vista del compilador) es poco probable que sea el comportamiento deseado cuando se usa para un miembro de la colección.

El código anterior es equivalente a:

    public class UserGroupDto
    {
        // This is a property returning the same instance
        // which was created when the UserGroupDto was instantiated.
        private ICollection<UserDto> _users1 = new HashSet<UserDto>();
        public ICollection<UserDto> Users1 { get { return _users1; } }

        // This is a field returning the same instance
        // which was created when the UserGroupDto was instantiated.
        public virtual ICollection<UserDto> Users2 = new HashSet<UserDto>();

        // This is a property which returns a new HashSet<UserDto> as
        // an ICollection<UserDto> on each call to it.
        public ICollection<UserDto> Users3 { get { return new HashSet<UserDto>(); } }
    }


[2]: https://www.wikiod.com/es/docs/c%23/49/properties/3365/auto-implemented-properties#t=201608062134378589394
[3]: http://ideone.com/2OgrPQ
[4]: http://ideone.com/qjDRmx
[5]: https://www.wikiod.com/es/docs/c%23/24/c-sharp-6-0-features/44/expression-bodied-function-members

## Propagación nula
El operador `?.` y el operador `?[...]` se denominan [operador condicional nulo][1]. A veces también se le conoce con otros nombres, como [operador de navegación segura][2].

Esto es útil, porque si el operador `.` (miembro de acceso) se aplica a una expresión que se evalúa como `null`, el programa lanzará una `NullReferenceException`. Si, en cambio, el desarrollador usa el operador `?.` (condicional nulo), la expresión se evaluará como nula en lugar de generar una excepción.

Tenga en cuenta que si se usa el operador `?.` y la expresión no es nula, `?.` y `.` son equivalentes.

---

# Conceptos básicos

    var teacherName = classroom.GetTeacher().Name;
    // throws NullReferenceException if GetTeacher() returns null

[Ver demostración][3]

Si el `aula` no tiene un profesor, `GetTeacher()` puede devolver `null`. Cuando es `null` y se accede a la propiedad `Name`, se lanzará una `NullReferenceException`.

Si modificamos esta declaración para usar la sintaxis `?.`, el resultado de la expresión completa será `nulo`:

    var teacherName = classroom.GetTeacher()?.Name;
    // teacherName is null if GetTeacher() returns null

[Ver demostración][4]

Posteriormente, si `aula` también pudiera ser `null`, también podríamos escribir esta declaración como:

    var teacherName = classroom?.GetTeacher()?.Name;
    // teacherName is null if GetTeacher() returns null OR classroom is null

[Ver demostración][5]

Este es un ejemplo de cortocircuito: cuando cualquier operación de acceso condicional que utiliza el operador condicional nulo se evalúa como nulo, la expresión completa se evalúa como nula inmediatamente, sin procesar el resto de la cadena.

Cuando el miembro terminal de una expresión que contiene el operador condicional nulo es de un tipo de valor, la expresión se evalúa como `Nullable<T>` de ese tipo y, por lo tanto, no se puede usar como reemplazo directo de la expresión sin `?.` .

    bool hasCertification = classroom.GetTeacher().HasCertification;
    // compiles without error but may throw a NullReferenceException at runtime

    bool hasCertification = classroom?.GetTeacher()?.HasCertification;
    // compile time error: implicit conversion from bool? to bool not allowed

    bool? hasCertification = classroom?.GetTeacher()?.HasCertification;
    // works just fine, hasCertification will be null if any part of the chain is null

    bool hasCertification = classroom?.GetTeacher()?.HasCertification.GetValueOrDefault();
    // must extract value from nullable to assign to a value type variable

---

# Usar con el operador de fusión nula (??)

Puede combinar el operador condicional nulo con el [Operador coalescente nulo][6] (`??`) para devolver un valor predeterminado si la expresión se resuelve como `null`. Usando nuestro ejemplo anterior:

    var teacherName = classroom?.GetTeacher()?.Name ?? "No Name";
    // teacherName will be "No Name" when GetTeacher() 
    // returns null OR classroom is null OR Name is null

---

# Uso con indexadores

El operador condicional nulo se puede usar con [indexadores][7]:

    var firstStudentName = classroom?.Students?[0]?.Name;

En el ejemplo anterior:

* El primer `?.` asegura que `aula` no sea `null`.
* El segundo `?` asegura que toda la colección `Students` no sea `nula`.
* El tercer `?.` después del indexador asegura que el indexador `[0]` no devolvió un objeto `nulo`. Debe tenerse en cuenta que esta operación puede **todavía** generar una `IndexOutOfRangeException`.

---

# Usar con funciones nulas

El operador condicional nulo también se puede usar con funciones `void`. Sin embargo, en este caso, la declaración no se evaluará como `null`. Simplemente evitará una `NullReferenceException`.

    List<string> list = null;
    list?.Add("hi");          // Does not evaluate to null


---

# Uso con invocación de eventos

Suponiendo la siguiente definición de evento:

    private event EventArgs OnCompleted;

Al invocar un evento, tradicionalmente, es una buena práctica verificar si el evento es "nulo" en caso de que no haya suscriptores presentes:

    var handler = OnCompleted;
    if (handler != null)
    {
        handler(EventArgs.Empty);
    }

Dado que se ha introducido el operador condicional nulo, la invocación se puede reducir a una sola línea:

    OnCompleted?.Invoke(EventArgs.Empty);

---

# Limitaciones

El operador condicional nulo produce rvalue, no lvalue, es decir, no se puede usar para la asignación de propiedades, suscripción de eventos, etc. Por ejemplo, el siguiente código no funcionará:

    // Error: The left-hand side of an assignment must be a variable, property or indexer
    Process.GetProcessById(1337)?.EnableRaisingEvents = true;
    // Error: The event can only appear on the left hand side of += or -=
    Process.GetProcessById(1337)?.Exited += OnProcessExited;

---

# errores

Tenga en cuenta que:

    int? nameLength = person?.Name.Length;    // safe if 'person' is null

__no__ es lo mismo que:

    int? nameLength = (person?.Name).Length;  // avoid this

porque lo primero corresponde a:

    int? nameLength = person != null ? (int?)person.Name.Length : null;

y este último corresponde a:

    int? nameLength = (person != null ? person.Name : null).Length;

A pesar de que aquí se usa el operador ternario `?:` para explicar la diferencia entre dos casos, estos operadores no son equivalentes. Esto se puede demostrar fácilmente con el siguiente ejemplo:

    void Main()
    {
        var foo = new Foo();
        Console.WriteLine("Null propagation");
        Console.WriteLine(foo.Bar?.Length);

        Console.WriteLine("Ternary");
        Console.WriteLine(foo.Bar != null ? foo.Bar.Length : (int?)null);
    }
    
    class Foo
    {
        public string Bar
        {
            get
            {
                Console.WriteLine("I was read");
                return string.Empty;
            }
        }
    }

Qué salidas:

>Propagación nula
>me leyeron
>0
>Ternario
>me leyeron
>me leyeron
>0

[Ver demostración][8]

Para evitar invocaciones múltiples, el equivalente sería:

    var interimResult = foo.Bar;
    Console.WriteLine(interimResult != null ? interimResult.Length : (int?)null);

Y esta diferencia explica un poco por qué el operador de propagación nula [aún no es compatible][9] en los árboles de expresión.


[1]: https://msdn.microsoft.com/en-us/library/dn986595.aspx
[2]: https://en.wikipedia.org/wiki/Safe_navigation_operator
[3]: http://ideone.com/p8OGBB
[4]: http://ideone.com/3aqGlE
[5]: http://ideone.com/voljZh
[6]: https://msdn.microsoft.com/en-us/library/ms173224.aspx
[7]: https://msdn.microsoft.com/en-us/library/6x16t2tx.aspx
[8]: https://dotnetfiddle.net/BytXEz
[9]: https://roslyn.codeplex.com/discusiones/571077


## Miembros de funciones con cuerpo de expresión
Los miembros de función con cuerpo de expresión permiten el uso de expresiones lambda como cuerpos de miembros. Para miembros simples, puede resultar en un código más limpio y legible.

Las funciones con cuerpo de expresión se pueden usar para propiedades, indexadores, métodos y operadores.

---

# Propiedades

    public decimal TotalPrice => BasePrice + Taxes;

Es equivalente a:

    public decimal TotalPrice
    {
        get
        {
            return BasePrice + Taxes;
        }
    }

Cuando se usa una función con cuerpo de expresión con una propiedad, la propiedad se implementa como una propiedad de solo captador.

[Ver demostración][1]

---

# indexadores

    public object this[string key] => dictionary[key];

Es equivalente a:

    public object this[string key]
    {
        get
        {
            return dictionary[key];
        }
    }

---

# Métodos

    static int Multiply(int a, int b) => a * b;

Es equivalente a:

    static int Multiply(int a, int b)
    {
        return a * b;
    }

Que también se puede usar con métodos `void`:

    public void Dispose() => resource?.Dispose();

Se podría agregar una anulación de `ToString` a la clase `Pair<T>`:

    public override string ToString() => $"{First}, {Second}";

Además, este enfoque simplista funciona con la palabra clave `override`:

    public class Foo
    {
        public int Bar { get; }
    
        public string override ToString() => $"Bar: {Bar}";
    }

---

# Operadores

Esto también puede ser utilizado por los operadores:

    public class Land
    {
        public double Area { get; set; }

        public static Land operator +(Land first, Land second) =>
            new Land { Area = first.Area + second.Area };
    }

---

# Limitaciones

Los miembros de función con cuerpo de expresión tienen algunas limitaciones. No pueden contener sentencias de bloque ni ninguna otra sentencia que contenga bloques: `if`, `switch`, `for`, `foreach`, `while`, `do`, `try`, etc.

Algunas declaraciones `if` se pueden reemplazar con operadores ternarios. Algunas declaraciones `for` y `foreach` se pueden convertir en consultas LINQ, por ejemplo:

    IEnumerable<string> Digits
    {
        get
        {
            for (int i = 0; i < 10; i++)
                yield return i.ToString();
        }
    }

<!---->

    IEnumerable<string> Digits => Enumerable.Range(0, 10).Select(i => i.ToString());

En todos los demás casos, se puede usar la sintaxis anterior para los miembros de funciones.

Los miembros de funciones con cuerpo de expresión pueden contener `async`/`await`, pero a menudo es redundante:

    async Task<int> Foo() => await Bar();  

Se puede reemplazar con:

    Task<int> Foo() => Bar();

[1]: https://dotnetfiddle.net/djFd7O


## Nombre del operador de
El operador `nameof` devuelve el nombre de un elemento de código como una `cadena`. Esto es útil cuando se lanzan excepciones relacionadas con argumentos de métodos y también cuando se implementa `INotifyPropertyChanged`.

    public string SayHello(string greeted)
    {
        if (greeted == null)
            throw new ArgumentNullException(nameof(greeted));
        
        Console.WriteLine("Hello, " + greeted);
    }

El operador `nameof` se evalúa en tiempo de compilación y cambia la expresión a una cadena literal. Esto también es útil para las cadenas que llevan el nombre del miembro que las expone. Considera lo siguiente:

    public static class Strings
    {
        public const string Foo = nameof(Foo); // Rather than Foo = "Foo"
        public const string Bar = nameof(Bar); // Rather than Bar = "Bar"
    }

Dado que las expresiones `nameof` son constantes en tiempo de compilación, se pueden usar en atributos, etiquetas `case`, sentencias `switch`, etc.

<hr/>

Es conveniente usar `nameof` con `Enum`s. En vez de:

    Console.WriteLine(Enum.One.ToString());

es posible utilizar:

    Console.WriteLine(nameof(Enum.One))

La salida será `Uno` en ambos casos.

<hr/>

El operador `nameof` puede acceder a miembros no estáticos usando una sintaxis similar a la estática. En lugar de hacer:

    string foo = "Foo";
    string lengthName = nameof(foo.Length);

Se puede reemplazar con:

    string lengthName = nameof(string.Length);

La salida será `Length` en ambos ejemplos. Sin embargo, este último evita la creación de instancias innecesarias.

<hr/>

Aunque el operador `nameof` funciona con la mayoría de las construcciones del lenguaje, existen algunas limitaciones. Por ejemplo, no puede usar el operador `nameof` en tipos genéricos abiertos o valores devueltos por métodos:

    public static int Main()
    {   
        Console.WriteLine(nameof(List<>)); // Compile-time error
        Console.WriteLine(nameof(Main())); // Compile-time error
    }

Además, si lo aplica a un tipo genérico, el parámetro de tipo genérico se ignorará:

    Console.WriteLine(nameof(List<int>));  // "List"
    Console.WriteLine(nameof(List<bool>)); // "List"

Para obtener más ejemplos, consulte [este tema][1] dedicado a `nameof`.

<hr/>

# Solución para versiones anteriores ([más detalles][2])

Aunque el operador `nameof` no existe en C# para versiones anteriores a la 6.0, se puede tener una funcionalidad similar usando `MemberExpression` como se muestra a continuación:

<!-- si la versión [lt 6.0] -->
Expresión:

    public static string NameOf<T>(Expression<Func<T>> propExp)
    {
        var memberExpression = propExp.Body as MemberExpression;
        return memberExpression != null ? memberExpression.Member.Name : null;
    }

    public static string NameOf<TObj, T>(Expression<Func<TObj, T>> propExp)
    {
        var memberExpression = propExp.Body as MemberExpression;
        return memberExpression != null ? memberExpression.Member.Name : null;
    }

Uso:

    string variableName = NameOf(() => variable);
    string propertyName = NameOf((Foo o) => o.Bar);

<!-- versión final si -->

Tenga en cuenta que este enfoque hace que se cree un árbol de expresión en cada llamada, por lo que el rendimiento es mucho peor en comparación con el operador `nameof`, que se evalúa en tiempo de compilación y no tiene gastos generales en tiempo de ejecución.


[1]: https://www.wikiod.com/es/docs/c%23/80/nameof-operator#t=201608031424500177545
[2]: https://www.wikiod.com/es/docs/c%23/80/nameof-operator/26157/name-of-extension-support-added-for-before-c-sharp-6-version#t= 201612071107472552734

## Usando tipo estático
La directiva `using static [Namespace.Type]` permite importar miembros estáticos de tipos y valores de enumeración. Los métodos de extensión se importan como métodos de extensión (de un solo tipo), no en el ámbito de nivel superior.

<!-- si la versión [gte 6.0] -->

    using static System.Console;
    using static System.ConsoleColor;
    using static System.Math;
    
    class Program
    {
        static void Main()
        {
            BackgroundColor = DarkBlue;
            WriteLine(Sqrt(2));
        }
    }

[Violín de demostración en vivo][1]
<!-- versión final si -->

<!-- si la versión [lt 6.0] -->

    using System;
    
    class Program
    {
        static void Main()
        {
            Console.BackgroundColor = ConsoleColor.DarkBlue;
            Console.WriteLine(Math.Sqrt(2));
        }
    }

<!-- versión final si -->


[1]: https://dotnetfiddle.net/7Ll3XN

## Inicializadores de índice
Los inicializadores de índices permiten crear e inicializar objetos con índices al mismo tiempo.

Esto hace que la inicialización de diccionarios sea muy fácil:

    var dict = new Dictionary<string, int>()
    {
        ["foo"] = 34,
        ["bar"] = 42
    };


Cualquier objeto que tenga un getter o setter indexado se puede usar con esta sintaxis:

    class Program
    {
        public class MyClassWithIndexer
        {
            public int this[string index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
        }

        public static void Main()
        {
            var x = new MyClassWithIndexer()
            {
                ["foo"] = 34,
                ["bar"] = 42
            };

            Console.ReadKey();
        }
    }

Producción:
>Índice: foo, valor: 34
>Índice: barra, valor: 42


[Ver demostración][1]

Si la clase tiene múltiples indexadores, es posible asignarlos todos en un solo grupo de declaraciones:

    class Program
    {
        public class MyClassWithIndexer
        {
            public int this[string index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
            public string this[int index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
        }

        public static void Main()
        {
            var x = new MyClassWithIndexer()
            {
                ["foo"] = 34,
                ["bar"] = 42,
                [10] = "Ten",
                [42] = "Meaning of life"
            };
        }
    }

Producción:
>Índice: foo, valor: 34
>Índice: barra, valor: 42
>Índice: 10, valor: Diez
>Índice: 42, valor: Sentido de la vida

Debe tenerse en cuenta que el descriptor de acceso `set` podría comportarse de manera diferente en comparación con un método `Add` (utilizado en los inicializadores de colección).

Por ejemplo:

    var d = new Dictionary<string, int>
    {
        ["foo"] = 34,
        ["foo"] = 42,
    }; // does not throw, second value overwrites the first one

versus:

    var d = new Dictionary<string, int>
    {
        { "foo", 34 },
        { "foo", 42 },
    }; // run-time ArgumentException: An item with the same key has already been added.


[1]: https://dotnetfiddle.net/Evs4Qx

## Resolución de sobrecarga mejorada
El siguiente fragmento muestra un ejemplo de paso de un grupo de métodos (a diferencia de una lambda) cuando se espera un delegado. La resolución de sobrecarga ahora resolverá esto en lugar de generar un error de sobrecarga ambiguo debido a la capacidad de **C# 6** para verificar el tipo de devolución del método que se pasó.

    using System;
    public class Program
    {
        public static void Main()
        {
            Overloaded(DoSomething);
        }
    
        static void Overloaded(Action action)
        {
           Console.WriteLine("overload with action called");
        }
    
        static void Overloaded(Func<int> function)
        {
           Console.WriteLine("overload with Func<int> called");
        }
    
        static int DoSomething()
        {
            Console.WriteLine(0);
            return 0;
        }
    }

Resultados:

<!-- si la versión [eq 6.0] -->
**Producción**
> sobrecarga con Func\<int\> llamado

[Ver demostración][1]
<!-- versión final si -->

<!-- si la versión [eq 5.0] -->
**Error**
> error CS0121: La llamada es ambigua entre los siguientes métodos o propiedades:
     'Program.Overloaded(System.Action)' and 'Program.Overloaded(System.Func)'

<!-- versión final si -->

**C# 6** también puede manejar bien el siguiente caso de coincidencia exacta para expresiones lambda que habría resultado en un error en **C# 5**.

    using System;

    class Program
    {
        static void Foo(Func<Func<long>> func) {}
        static void Foo(Func<Func<int>> func) {}

        static void Main()
        {
            Foo(() => () => 7);
        }
    }


[1]: https://dotnetfiddle.net/Vnudqy

## Espera en catch y finalmente
Es posible usar la expresión `await` para aplicar [await operator][1] a [Tasks][2] o [Task(Of TResult)][3] en los bloques `catch` y `finally` en C#6 .

No era posible usar la expresión `await` en los bloques `catch` y `finally` en versiones anteriores debido a las limitaciones del compilador. C#6 facilita mucho la espera de tareas asíncronas al permitir la expresión `await`.

    try
    {
        //since C#5
        await service.InitializeAsync();
    } 
    catch (Exception e)
    {
        //since C#6
        await logger.LogAsync(e);
    }
    finally
    {
        //since C#6
        await service.CloseAsync();
    }

En C# 5 se requería usar un `bool` o declarar una `Exception` fuera de try catch para realizar operaciones asíncronas. Este método se muestra en el siguiente ejemplo:
    
    bool error = false;
    Exception ex = null;

    try
    {
        // Since C#5
        await service.InitializeAsync();
    } 
    catch (Exception e)
    {
        // Declare bool or place exception inside variable
        error = true;
        ex = e;
    }

    // If you don't use the exception
    if (error)
    {
        // Handle async task
    }

    // If want to use information from the exception
    if (ex != null)
    {
        await logger.LogAsync(e);
    }    

    // Close the service, since this isn't possible in the finally
    await service.CloseAsync();


[1]: https://msdn.microsoft.com/en-us/library/hh156528.aspx
[2]: https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.aspx
[3]: https://msdn.microsoft.com/en-us/library/dd321424.aspx

## Cambios menores y correcciones de errores
Los paréntesis ahora están prohibidos alrededor de los parámetros con nombre. Lo siguiente se compila en C#5, pero no en C#6

<!-- si la versión [lte 5.0] -->

    Console.WriteLine((value: 23));

<!-- versión final si -->

Ya no se permite que los operandos `is` y `as` sean grupos de métodos. Lo siguiente se compila en C#5, pero no en C#6

<!-- si la versión [lte 5.0] -->

    var result = "".Any is byte;

> El compilador nativo permitió esto (aunque mostró una advertencia), y de hecho ni siquiera verificó la compatibilidad del método de extensión, permitiendo locuras como `1.Any is string` o `IDisposable.Dispose is object`.

<!-- versión final si -->

Consulte [esta referencia][1] para obtener actualizaciones sobre los cambios.


[1]: http://blog.slaks.net/2014-05-28/exploring-roslyn-part-3-breaking-changes/

## Usando un método de extensión para la inicialización de la colección
La sintaxis de inicialización de la colección se puede usar al instanciar cualquier clase que implemente `IEnumerable` y tenga un método llamado `Add` que toma un solo parámetro.

En versiones anteriores, este método `Add` tenía que ser un método de **instancia** en la clase que se estaba inicializando. En C#6, también puede ser un método de extensión.

    public class CollectionWithAdd : IEnumerable
    {
        public void Add<T>(T item)
        {
            Console.WriteLine("Item added with instance add method: " + item);
        }

        public IEnumerator GetEnumerator()
        {
            // Some implementation here
        }
    }
    
    public class CollectionWithoutAdd : IEnumerable
    {
        public IEnumerator GetEnumerator()
        {
            // Some implementation here
        }
    }
    
    public static class Extensions
    {
        public static void Add<T>(this CollectionWithoutAdd collection, T item)
        {
            Console.WriteLine("Item added with extension add method: " + item);
        }
    }
    
    public class Program
    {
        public static void Main()
        {
            var collection1 = new CollectionWithAdd{1,2,3}; // Valid in all C# versions
            var collection2 = new CollectionWithoutAdd{4,5,6}; // Valid only since C# 6
        }
    }


Esto generará:

>Elemento agregado con el método de agregar instancia: 1
>Elemento agregado con el método de agregar instancia: 2
>Elemento agregado con el método de agregar instancia: 3
>Elemento agregado con el método de agregar extensión: 4
>Elemento agregado con el método de agregar extensión: 5
>Elemento agregado con el método de agregar extensión: 6

## Deshabilitar mejoras de advertencias
En C# 5.0 y versiones anteriores, el desarrollador solo podía suprimir las advertencias por número. Con la introducción de Roslyn Analyzers, C# necesita una forma de deshabilitar las advertencias emitidas desde bibliotecas específicas. Con C# 6.0, la directiva pragma puede suprimir las advertencias por nombre.

Antes:

    #pragma warning disable 0501

C# 6.0:

    #pragma warning disable CS0501

