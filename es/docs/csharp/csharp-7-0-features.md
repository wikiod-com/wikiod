---
title: "Características de C# 7.0"
slug: "caracteristicas-de-c-70"
draft: false
images: []
weight: 1717
type: docs
toc: true
---

C# 7.0 es la séptima versión de C#. Esta versión contiene algunas características nuevas: soporte de idioma para tuplas, funciones locales, declaraciones `out var`, separadores de dígitos, literales binarios, coincidencia de patrones, expresiones throw, `ref return` y `ref local` y lista de miembros con cuerpo de expresión extendida.

Referencia oficial: [Novedades de C# 7](https://docs.microsoft.com/en-us/dotnet/articles/csharp/csharp-7)

## Soporte de idioma para tuplas
# Conceptos básicos

Una **tupla** es una lista ordenada y finita de elementos. Las tuplas se usan comúnmente en programación como un medio para trabajar con una sola entidad colectivamente en lugar de trabajar individualmente con cada uno de los elementos de la tupla, y para representar filas individuales (es decir, "registros") en una base de datos relacional.

En C# 7.0, los métodos pueden tener varios valores de retorno. Detrás de escena, el compilador utilizará la nueva estructura [ValueTuple][1].

    public (int sum, int count) GetTallies() 
    {
        return (1, 2);
    }

_Nota al margen_: para que esto funcione en Visual Studio 2017, debe obtener el paquete ```System.ValueTuple```.

Si el resultado de un método de retorno de tupla se asigna a una sola variable, puede acceder a los miembros por sus nombres definidos en la firma del método:

    var result = GetTallies();
    // > result.sum
    // 1
    // > result.count
    // 2

# Deconstrucción de tuplas

La deconstrucción de tuplas separa una tupla en sus partes.

Por ejemplo, invocar `GetTallies` y asignar el valor de retorno a dos variables separadas deconstruye la tupla en esas dos variables:

    (int tallyOne, int tallyTwo) = GetTallies();

`var` también funciona:

    (var s, var c) = GetTallies();

También puede usar una sintaxis más corta, con `var` fuera de `()`:

    var (s, c) = GetTallies();

También puede deconstruir en variables existentes:

    int s, c;
    (s, c) = GetTallies();

El intercambio ahora es mucho más simple (no se necesita una variable temporal):

    (b, a) = (a, b);

Curiosamente, cualquier objeto se puede deconstruir definiendo un método `Deconstruct` en la clase:

    class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }

        public void Deconstruct(out string firstName, out string lastName)
        {
            firstName = FirstName;
            lastName = LastName;
        }
    }

    var person = new Person { FirstName = "John", LastName = "Smith" };
    var (localFirstName, localLastName) = person;

En este caso, la sintaxis `(localFirstName, localLastName) = person` está invocando `Deconstruct` en `person`.

La deconstrucción puede incluso definirse en un método de extensión. Esto es equivalente a lo anterior:

    public static class PersonExtensions
    {
        public static void Deconstruct(this Person person, out string firstName, out string lastName)
        {
            firstName = person.FirstName;
            lastName = person.LastName;
        }
    }
    
    var (localFirstName, localLastName) = person;

Un enfoque alternativo para la clase 'Persona' es definir el propio 'Nombre' como una 'Tupla'. Considera lo siguiente:

    class Person
    {
        public (string First, string Last) Name { get; }

        public Person((string FirstName, string LastName) name)
        {
            Name = name;
        }
    }

Luego puede crear una instancia de una persona así (donde podemos tomar una tupla como argumento):

    var person = new Person(("Jane", "Smith"));

    var firstName = person.Name.First; // "Jane"
    var lastName = person.Name.Last;   // "Smith"

# Inicialización de tuplas
También puede crear arbitrariamente tuplas en el código:

    var name = ("John", "Smith");
    Console.WriteLine(name.Item1);
    // Outputs John

    Console.WriteLine(name.Item2);
    // Outputs Smith

# 

Al crear una tupla, puede asignar nombres de elementos ad-hoc a los miembros de la tupla:

    var name = (first: "John", middle: "Q", last: "Smith");
    Console.WriteLine(name.first);
    // Outputs John

# Tipo de inferencia

Varias tuplas definidas con la misma firma (tipos coincidentes y recuento) se inferirán como tipos coincidentes. Por ejemplo:

    public (int sum, double average) Measure(List<int> items)
    {
        var stats = (sum: 0, average: 0d);
        stats.sum = items.Sum();
        stats.average = items.Average();
        return stats;
    }

Se puede devolver `stats` ya que la declaración de la variable `stats` y la firma de retorno del método coinciden.

# Nombres de campo de reflexión y tupla
Los nombres de miembros no existen en tiempo de ejecución. Reflection considerará las tuplas con el mismo número y tipos de miembros incluso si los nombres de los miembros no coinciden. Convertir una tupla en un `objeto` y luego en una tupla con los mismos tipos de miembros, pero con diferentes nombres, tampoco provocará una excepción.

Si bien la clase ValueTuple en sí misma no conserva la información de los nombres de los miembros, la información está disponible a través de la reflexión en un TupleElementNamesAttribute. Este atributo no se aplica a la tupla en sí, sino a los parámetros del método, valores devueltos, propiedades y campos. Esto permite que los nombres de elementos de tupla se conserven entre ensamblajes, es decir, si un método devuelve (nombre de cadena, recuento int), el nombre y el recuento de nombres estarán disponibles para las personas que llaman al método en otro ensamblaje porque el valor devuelto se marcará con TupleElementNameAttribute que contiene los valores. "nombre" y "contar".

# Usar con genéricos y `async`

Las nuevas funciones de tupla (que utilizan el tipo `ValueTuple` subyacente) son totalmente compatibles con los genéricos y se pueden utilizar como parámetro de tipo genérico. Eso hace posible usarlos con el patrón `async`/`await`:

    public async Task<(string value, int count)> GetValueAsync()
    {
        string fooBar = await _stackoverflow.GetStringAsync();
        int num = await _stackoverflow.GetIntAsync();

        return (fooBar, num);
    }

# Usar con colecciones

Puede ser beneficioso tener una colección de tuplas (como ejemplo) en un escenario en el que intenta encontrar una tupla coincidente en función de las condiciones para evitar la bifurcación del código.

Ejemplo:

    private readonly List<Tuple<string, string, string>> labels = new List<Tuple<string, string, string>>()
    {
        new Tuple<string, string, string>("test1", "test2", "Value"),
        new Tuple<string, string, string>("test1", "test1", "Value2"),
        new Tuple<string, string, string>("test2", "test2", "Value3"),
    };

    public string FindMatchingValue(string firstElement, string secondElement)
    {
        var result = labels
            .Where(w => w.Item1 == firstElement && w.Item2 == secondElement)
            .FirstOrDefault();

        if (result == null)
            throw new ArgumentException("combo not found");

        return result.Item3;
    }

Con las nuevas tuplas pueden convertirse en:

    private readonly List<(string firstThingy, string secondThingyLabel, string foundValue)> labels = new List<(string firstThingy, string secondThingyLabel, string foundValue)>()
    {
        ("test1", "test2", "Value"),
        ("test1", "test1", "Value2"),
        ("test2", "test2", "Value3"),
    }

    public string FindMatchingValue(string firstElement, string secondElement)
    {
        var result = labels
            .Where(w => w.firstThingy == firstElement && w.secondThingyLabel == secondElement)
            .FirstOrDefault();

        if (result == null)
            throw new ArgumentException("combo not found");

        return result.foundValue;
    }

Aunque la denominación de la tupla de ejemplo anterior es bastante genérica, la idea de las etiquetas relevantes permite una comprensión más profunda de lo que se intenta en el código sobre las referencias a "elemento1", "elemento2" y "elemento3".

# Diferencias entre ValueTuple y Tuple

La razón principal para la introducción de `ValueTuple` es el rendimiento.

| Escriba el nombre | `TuplaValor` | `Tupla` |
|---|---|---|
| Clase o estructura | `estructura` | `clase` |
| Mutabilidad (valores cambiantes después de la creación) | mutable | inmutable |
| Nombramiento de miembros y soporte de otros idiomas | si | no ([TBD][2]) |

# Referencias

- [Propuesta de función de idioma original de Tuples en GitHub][3]
- [Una solución VS 15 ejecutable para funciones de C# 7.0][4]
- [Paquete de tupla NuGet][5]


[1]: https://github.com/dotnet/corefx/blob/master/src/System.ValueTuple/src/System/ValueTuple/ValueTuple.cs
[2]: https://github.com/dotnet/roslyn/issues/11031
[3]: https://github.com/dotnet/roslyn/issues/347
[4]: https://code.msdn.microsoft.com/Introduce-new-C-70-features-c639ed88
[5]: https://www.nuget.org/packages/System.ValueTuple/

## Funciones locales
Las funciones locales se definen dentro de un método y no están disponibles fuera de él. Tienen acceso a todas las variables locales y soportan iteradores, `async`/`await` y sintaxis lambda. De esta manera, se pueden funcionalizar repeticiones específicas de una función sin abarrotar la clase. Como efecto secundario, esto mejora el rendimiento de las sugerencias de intellisense.

# Ejemplo

    double GetCylinderVolume(double radius, double height)
    {
        return getVolume();
  
        double getVolume()
        {
            // You can declare inner-local functions in a local function 
            double GetCircleArea(double r) => Math.PI * r * r;

            // ALL parents' variables are accessible even though parent doesn't have any input. 
            return GetCircleArea(radius) * height;
        }
    }

Las funciones locales simplifican considerablemente el código para los operadores de LINQ, en los que normalmente tiene que separar las comprobaciones de argumentos de la lógica real para que las comprobaciones de argumentos sean instantáneas, no retrasadas hasta después de iniciada la iteración.

# Ejemplo

    public static IEnumerable<TSource> Where<TSource>(
        this IEnumerable<TSource> source, 
        Func<TSource, bool> predicate)
    {
        if (source == null) throw new ArgumentNullException(nameof(source));
        if (predicate == null) throw new ArgumentNullException(nameof(predicate));
    
        return iterator();

        IEnumerable<TSource> iterator()
        {
            foreach (TSource element in source)
                if (predicate(element))
                    yield return element;
        }
    }

Las funciones locales también admiten las palabras clave `async` y `await`.

# Ejemplo

    async Task WriteEmailsAsync()
    {
        var emailRegex = new Regex(@"(?i)[a-z0-9_.+-]+@[a-z0-9-]+\.[a-z0-9-.]+");
        IEnumerable<string> emails1 = await getEmailsFromFileAsync("input1.txt");
        IEnumerable<string> emails2 = await getEmailsFromFileAsync("input2.txt");
        await writeLinesToFileAsync(emails1.Concat(emails2), "output.txt");

        async Task<IEnumerable<string>> getEmailsFromFileAsync(string fileName)
        {
            string text;

            using (StreamReader reader = File.OpenText(fileName))
            {
                text = await reader.ReadToEndAsync();
            }

            return from Match emailMatch in emailRegex.Matches(text) select emailMatch.Value;
        }

        async Task writeLinesToFileAsync(IEnumerable<string> lines, string fileName)
        {
            using (StreamWriter writer = File.CreateText(fileName))
            {
                foreach (string line in lines)
                {
                    await writer.WriteLineAsync(line);
                }
            }
        }
    }

Una cosa importante que puede haber notado es que las funciones locales se pueden definir bajo la instrucción `return`, **no** necesitan definirse encima de ella. Además, las funciones locales suelen seguir la convención de nomenclatura "lowerCamelCase" para diferenciarse más fácilmente de las funciones de ámbito de clase.

## fuera de la declaración de var
Un patrón común en C# es usar `bool TryParse(entrada de objeto, valor de objeto de salida)` para analizar objetos de forma segura.

La declaración `out var` es una característica simple para mejorar la legibilidad. Permite declarar una variable al mismo tiempo que se pasa como parámetro de salida.

Una variable declarada de esta manera se limita al resto del cuerpo en el punto en el que se declara.

# Ejemplo

Al usar `TryParse` antes de C# 7.0, debe declarar una variable para recibir el valor antes de llamar a la función:

<!-- si la versión [lt 7.0] -->
    int value;
    if (int.TryParse(input, out value)) 
    {
        Foo(value); // ok
    }
    else
    {
        Foo(value); // value is zero
    }

    Foo(value); // ok
<!-- versión final si -->

En C# 7.0, puede alinear la declaración de la variable pasada al parámetro `out`, eliminando la necesidad de una declaración de variable separada:

<!-- si la versión [gte 7.0] -->
    if (int.TryParse(input, out var value)) 
    {
        Foo(value); // ok
    }
    else
    {
        Foo(value); // value is zero
    }

    Foo(value); // still ok, the value in scope within the remainder of the body
<!-- versión final si -->

Si algunos de los parámetros que devuelve una función en `out` no son necesarios, puede utilizar el operador _discard_ `_`.

    p.GetCoordinates(out var x, out _); // I only care about x

Una declaración `out var` se puede usar con cualquier función existente que ya tenga parámetros `out`. La sintaxis de la declaración de la función sigue siendo la misma y no se necesitan requisitos adicionales para que la función sea compatible con una declaración `out var`. Esta característica es simplemente azúcar sintáctica.

Otra característica de la declaración `out var` es que se puede usar con tipos anónimos.

<!-- si la versión [gte 7.0] -->
    var a = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    var groupedByMod2 = a.Select(x => new
                                      {
                                          Source = x,
                                          Mod2 = x % 2
                                      })
                         .GroupBy(x => x.Mod2)
                         .ToDictionary(g => g.Key, g => g.ToArray());
    if (groupedByMod2.TryGetValue(1, out var oddElements))
    {
        Console.WriteLine(oddElements.Length);
    }
<!-- versión final si -->
    
En este código creamos un `Diccionario` con la clave `int` y una matriz de valor de tipo anónimo. En la versión anterior de C#, era imposible usar el método `TryGetValue` aquí, ya que requería que declarara la variable `out` (¡que es de tipo anónimo!). Sin embargo, con `out var` no necesitamos especificar explícitamente el tipo de la variable `out`.

# Limitaciones

Tenga en cuenta que las declaraciones out var tienen un uso limitado en las consultas LINQ, ya que las expresiones se interpretan como cuerpos lambda de expresión, por lo que el alcance de las variables introducidas se limita a estas expresiones lambda. Por ejemplo, el siguiente código no funcionará:

    var nums = 
        from item in seq
        let success = int.TryParse(item, out var tmp)
        select success ? tmp : 0; // Error: The name 'tmp' does not exist in the current context



# Referencias

* [Propuesta de declaración original de var en GitHub](https://github.com/dotnet/roslyn/issues/6183)

## La coincidencia de patrones
Las extensiones de coincidencia de patrones para C# permiten muchos de los beneficios de la coincidencia de patrones de los lenguajes funcionales, pero de una manera que se integra sin problemas con la sensación del lenguaje subyacente.

**expresión `cambiar`**
-----
La coincidencia de patrones amplía la instrucción `switch` para activar tipos:

    class Geometry {} 

    class Triangle : Geometry
    {
        public int Width { get; set; }
        public int Height { get; set; }
        public int Base { get; set; }
    }

    class Rectangle : Geometry
    {
        public int Width { get; set; }
        public int Height { get; set; }
    }

    class Square : Geometry
    {
        public int Width { get; set; }
    }

    public static void PatternMatching()
    {
        Geometry g = new Square { Width = 5 }; 
        
        switch (g)
        {
            case Triangle t:
                Console.WriteLine($"{t.Width} {t.Height} {t.Base}");
                break;
            case Rectangle sq when sq.Width == sq.Height:
                Console.WriteLine($"Square rectangle: {sq.Width} {sq.Height}");
                break;
            case Rectangle r:
                Console.WriteLine($"{r.Width} {r.Height}");
                break;
            case Square s:
                Console.WriteLine($"{s.Width}");
                break;
            default:
                Console.WriteLine("<other>");
                break;
        }
    }


**`es` expresión**
---- 

La coincidencia de patrones amplía el operador `is` para comprobar un tipo y declarar una nueva variable al mismo tiempo.

# Ejemplo

<!-- si la versión [lt 7.0] -->
    string s = o as string;
    if(s != null)
    {
        // do something with s
    }
<!-- versión final si -->

se puede reescribir como:

<!-- si la versión [gte 7.0] -->
    if(o is string s)
    {
        //Do something with s
    };
<!-- versión final si -->

También tenga en cuenta que el alcance de la variable de patrón `s` se extiende fuera del bloque `if` alcanzando el final del alcance adjunto, ejemplo:

    if(someCondition)
    {
       if(o is string s)
       {
          //Do something with s
       }
       else
       {
         // s is unassigned here, but accessible 
       }
    
       // s is unassigned here, but accessible 
    }
    // s is not accessible here

## Separadores de dígitos
El guión bajo `_` se puede utilizar como separador de dígitos. Ser capaz de agrupar dígitos en grandes literales numéricos tiene un impacto significativo en la legibilidad.

El guión bajo puede aparecer en cualquier parte de un literal numérico excepto como se indica a continuación. Diferentes agrupaciones pueden tener sentido en diferentes escenarios o con diferentes bases numéricas.

Cualquier secuencia de dígitos puede estar separada por uno o más guiones bajos. El `_` está permitido tanto en decimales como en exponentes. Los separadores no tienen impacto semántico, simplemente se ignoran.

    int bin = 0b1001_1010_0001_0100;
    int hex = 0x1b_a0_44_fe;
    int dec = 33_554_432;
    int weird = 1_2__3___4____5_____6______7_______8________9;
    double real = 1_000.111_1e-1_000;

**Donde no se puede usar el separador de dígitos `_`:**
- al principio del valor (`_121`)
- al final del valor (`121_` o `121.05_`)
- al lado del decimal (`10_.0`)
- junto al carácter exponente (`1.1e_1`)
- junto al especificador de tipo (`10_f`)
- inmediatamente después de `0x` o `0b` en literales binarios y hexadecimales ([podría cambiarse para permitir, por ejemplo, 0b_1001_1000][1])

[1]: https://github.com/dotnet/roslyn/issues/12680

## Literales binarios
El prefijo **0b** se puede usar para representar literales binarios.

Los literales binarios permiten construir números a partir de ceros y unos, lo que hace mucho más fácil ver qué bits se establecen en la representación binaria de un número. Esto puede ser útil para trabajar con banderas binarias.

Las siguientes son formas equivalentes de especificar un `int` con el valor `34` (=2<sup>5</sup> + 2<sup>1</sup>):

    // Using a binary literal:
    //   bits: 76543210
    int a1 = 0b00100010;          // binary: explicitly specify bits

    // Existing methods:
    int a2 = 0x22;                // hexadecimal: every digit corresponds to 4 bits
    int a3 = 34;                  // decimal: hard to visualise which bits are set
    int a4 = (1 << 5) | (1 << 1); // bitwise arithmetic: combining non-zero bits

# Enumeraciones de banderas

Antes, la especificación de valores de marca para un `enum` solo se podía hacer usando uno de los tres métodos de este ejemplo:

    [Flags]
    public enum DaysOfWeek
    {
        // Previously available methods:
        //          decimal        hex       bit shifting
        Monday    =  1,    //    = 0x01    = 1 << 0
        Tuesday   =  2,    //    = 0x02    = 1 << 1
        Wednesday =  4,    //    = 0x04    = 1 << 2
        Thursday  =  8,    //    = 0x08    = 1 << 3
        Friday    = 16,    //    = 0x10    = 1 << 4
        Saturday  = 32,    //    = 0x20    = 1 << 5
        Sunday    = 64,    //    = 0x40    = 1 << 6
    
        Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
        Weekends = Saturday | Sunday
    }

Con los literales binarios, es más obvio qué bits están configurados, y usarlos no requiere comprender los números hexadecimales y la aritmética bit a bit:

    [Flags]
    public enum DaysOfWeek
    {
        Monday    = 0b00000001,
        Tuesday   = 0b00000010,
        Wednesday = 0b00000100,
        Thursday  = 0b00001000,
        Friday    = 0b00010000,
        Saturday  = 0b00100000,
        Sunday    = 0b01000000,
    
        Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
        Weekends = Saturday | Sunday
    }

## lanzar expresiones
C# 7.0 permite lanzar como una expresión en ciertos lugares:

    class Person
    {
        public string Name { get; }

        public Person(string name) => Name = name ?? throw new ArgumentNullException(nameof(name));

        public string GetFirstName()
        {
            var parts = Name.Split(' ');
            return (parts.Length > 0) ? parts[0] : throw new InvalidOperationException("No name!");
        }

        public string GetLastName() => throw new NotImplementedException();
    }


Antes de C# 7.0, si deseaba generar una excepción desde el cuerpo de una expresión, tenía que hacer lo siguiente:

    var spoons = "dinner,desert,soup".Split(',');

    var spoonsArray = spoons.Length > 0 ? spoons : null;

    if (spoonsArray == null) 
    {
        throw new Exception("There are no spoons");
    }

O

    var spoonsArray = spoons.Length > 0 
        ? spoons 
        : new Func<string[]>(() => 
          {
              throw new Exception("There are no spoons");
          })();

En C# 7.0, lo anterior ahora se simplifica a:

    var spoonsArray = spoons.Length > 0 ? spoons : throw new Exception("There are no spoons");



## Lista de miembros con cuerpo de expresión extendida
C# 7.0 agrega accesores, constructores y finalizadores a la lista de cosas que pueden tener cuerpos de expresión:

    class Person
    {
        private static ConcurrentDictionary<int, string> names = new ConcurrentDictionary<int, string>();

        private int id = GetId();

        public Person(string name) => names.TryAdd(id, name); // constructors

        ~Person() => names.TryRemove(id, out _);              // finalizers

        public string Name
        {
            get => names[id];                                 // getters
            set => names[id] = value;                         // setters
        }
    }

Consulte también la sección [declaración de var][1] para el operador de descarte.

[1]: https://www.wikiod.com/es/docs/c%23/1936/c-sharp-7-0-features/6326/out-var-declaration

## retorno de referencia y referencia local
Las devoluciones de referencia y las referencias locales son útiles para manipular y devolver referencias a bloques de memoria en lugar de copiar la memoria sin recurrir a punteros inseguros.

# Retorno de referencia

    public static ref TValue Choose<TValue>(
        Func<bool> condition, ref TValue left, ref TValue right)
    {
        return condition() ? ref left : ref right;
    }

Con esto, puede pasar dos valores por referencia y uno de ellos se devolverá en función de alguna condición:

    Matrix3D left = …, right = …;
    Choose(chooser, ref left, ref right).M20 = 1.0;


# referencia local

    public static ref int Max(ref int first, ref int second, ref int third)
    {
        ref int max = first > second ? ref first : ref second;
        return max > third ? ref max : ref third;
    }
    …
    int a = 1, b = 2, c = 3;
    Max(ref a, ref b, ref c) = 4;
    Debug.Assert(a == 1); // true
    Debug.Assert(b == 2); // true
    Debug.Assert(c == 4); // true

# Operaciones de referencia no seguras
En `System.Runtime.CompilerServices.Unsafe` se han definido un conjunto de operaciones no seguras que te permiten manipular los valores `ref` como si fueran punteros, básicamente.

Por ejemplo, reinterpretar una dirección de memoria (`ref`) como un tipo diferente:

    byte[] b = new byte[4] { 0x42, 0x42, 0x42, 0x42 };
    
    ref int r = ref Unsafe.As<byte, int>(ref b[0]);
    Assert.Equal(0x42424242, r);
    
    0x0EF00EF0;
    Assert.Equal(0xFE, b[0] | b[1] | b[2] | b[3]);

Sin embargo, tenga cuidado con [endianness][1] al hacer esto, p. marque `BitConverter.IsLittleEndian` si es necesario y manéjelo en consecuencia.

O iterar sobre una matriz de una manera insegura:

    int[] a = new int[] { 0x123, 0x234, 0x345, 0x456 };
    
    ref int r1 = ref Unsafe.Add(ref a[0], 1);
    Assert.Equal(0x234, r1);

    ref int r2 = ref Unsafe.Add(ref r1, 2);
    Assert.Equal(0x456, r2);

    ref int r3 = ref Unsafe.Add(ref r2, -3);
    Assert.Equal(0x123, r3);

O el `Restar` similar:

    string[] a = new string[] { "abc", "def", "ghi", "jkl" };
    
    ref string r1 = ref Unsafe.Subtract(ref a[0], -2);
    Assert.Equal("ghi", r1);
    
    ref string r2 = ref Unsafe.Subtract(ref r1, -1);
    Assert.Equal("jkl", r2);
    
    ref string r3 = ref Unsafe.Subtract(ref r2, 3);
    Assert.Equal("abc", r3);

Además, se puede verificar si dos valores `ref` son iguales, es decir, la misma dirección:

    long[] a = new long[2];
    
    Assert.True(Unsafe.AreSame(ref a[0], ref a[0]));
    Assert.False(Unsafe.AreSame(ref a[0], ref a[1]));

# Enlaces
[Problema de Roslyn Github][2]

[System.Runtime.CompilerServices.Unsafe en github][3]


[1]: https://en.wikipedia.org/wiki/Endianness
[2]: https://github.com/dotnet/roslyn/issues/118
[3]: https://github.com/dotnet/corefx/tree/master/src/System.Runtime.CompilerServices.Unsafe

## Tarea de valor<T>
`Task<T>` es una **clase** y provoca la sobrecarga innecesaria de su asignación cuando el resultado está disponible de inmediato.

`ValueTask<T>` es una **estructura** y se introdujo para evitar la asignación de un objeto `Task` en caso de que el resultado de la operación **asincrónica** ya esté disponible en el momento de la espera.

Entonces `ValueTask<T>` proporciona dos beneficios:

# 1. Aumento del rendimiento

Aquí hay un ejemplo de `Tarea<T>`:
- Requiere asignación de montón
- Toma 120ns con JIT


    async Task<int> TestTask(int d)
    {
        await Task.Delay(d);
        return 10;
    }

Aquí está el ejemplo analógico `ValueTask<T>`:
- No hay asignación de montón si el resultado se conoce de forma síncrona (que no es en este caso debido a `Task.Delay`, pero a menudo ocurre en muchos escenarios `async`/`await` del mundo real)
- Toma 65ns con JIT


    async ValueTask<int> TestValueTask(int d)
    {
        await Task.Delay(d);
        return 10;
    }

# 2. Mayor flexibilidad de implementación

De lo contrario, las implementaciones de una interfaz asíncrona que deseen ser síncronas se verían obligadas a usar `Task.Run` o `Task.FromResult` (lo que resultaría en la penalización de rendimiento discutida anteriormente). Por lo tanto, existe cierta presión contra las implementaciones sincrónicas.

Pero con `ValueTask<T>`, las implementaciones tienen más libertad para elegir entre ser sincrónicas o asincrónicas sin afectar a las personas que llaman.

Por ejemplo, aquí hay una interfaz con un método asíncrono:

    interface IFoo<T>
    {
        ValueTask<T> BarAsync();
    }

... y así es como se podría llamar ese método:

    IFoo<T> thing = getThing();
    var x = await thing.BarAsync();

Con `ValueTask`, el código anterior funcionará con **implementaciones síncronas o asíncronas**:

## Implementación síncrona:

    class SynchronousFoo<T> : IFoo<T>
    {
        public ValueTask<T> BarAsync()
        {
            var value = default(T);
            return new ValueTask<T>(value);
        }
    }

## Implementación asíncrona

    class AsynchronousFoo<T> : IFoo<T>
    {
        public async ValueTask<T> BarAsync()
        {
            var value = default(T);
            await Task.Delay(1);
            return value;
        }
    }

# Notas

Aunque se planeó agregar la estructura `ValueTask` a [C# 7.0][1], se ha mantenido como otra biblioteca por el momento.
https://www.wikiod.com/es/docs/c%23/1936/c-sharp-7-0-features/28612/valuetaskt#
El paquete `System.Threading.Tasks.Extensions` se puede descargar desde [Nuget Gallery](https://www.nuget.org/packages/System.Threading.Tasks.Extensions/)

[1]: https://blogs.msdn.microsoft.com/dotnet/2016/08/24/whats-new-in-csharp-7-0/

