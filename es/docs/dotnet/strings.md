---
title: "Instrumentos de cuerda"
slug: "instrumentos-de-cuerda"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

En las cadenas .NET `System.String` hay una secuencia de caracteres `System.Char`, cada carácter es una unidad de código codificada en UTF-16. Esta distinción es importante porque la definición de carácter en _idioma hablado_ y la definición de carácter en .NET (y muchos otros idiomas) son diferentes.

Un _carácter_, que debería llamarse correctamente [grafema][1], se muestra como un [glifo][2] y está definido por uno o más [puntos de código][3] Unicode. A continuación, cada punto de código se codifica en una secuencia de [unidades de código][4]. Ahora debería quedar claro por qué un solo `System.Char` no siempre representa un grafema, veamos en el mundo real en qué se diferencian:

* Un grafema, debido a [combinar caracteres][5], puede dar como resultado dos o más puntos de código: <kbd>à</kbd> está compuesto por dos puntos de código: _U+0061 LETRA A MINÚSCULA LATINA_ y _U+ 0300 COMBINACIÓN DE ACENTO GRAVE_. Este es el error más común porque `"à".Length == 2` mientras que puede esperar `1`.
* Hay caracteres duplicados, por ejemplo, <kbd>à</kbd> puede ser un solo punto de código _U+00E0 LETRA A MINÚSCULA LATINA CON TUMBA_ o dos puntos de código como se explicó anteriormente. Obviamente deben comparar lo mismo: `"\u00e0" == "\u0061\u0300"` (incluso si `"\u00e0".Length != "\u0061\u0300".Length`). Esto es posible gracias a la _normalización de cadenas_ realizada por el método `String.Normalize()`.
* Una secuencia Unicode puede contener una secuencia compuesta o descompuesta, por ejemplo, el carácter <kbd>한</kbd> _U+D55C HAN CHARACTER_ puede ser un solo punto de código (codificado como una sola unidad de código en UTF-16) o un secuencia descompuesta de sus sílabas <kbd>ᄒ</kbd>, <kbd>ᅡ</kbd> y <kbd>ᆫ</kbd>. Deben ser comparados iguales.
* Un punto de código se puede codificar en más de una unidad de código: el carácter <kbd>𠂊</kbd> _U+2008A HAN CHARACTER_ se codifica como dos `System.Char` (`"\ud840\udc8a"`) incluso si es solo un punto de código: ¡la codificación UTF-16 no tiene un tamaño fijo! Esta es una fuente de innumerables errores (también errores de seguridad graves), si, por ejemplo, su aplicación aplica una longitud máxima y trunca ciegamente la cadena, entonces puede crear una cadena no válida.
* Algunos idiomas tienen [digraph][6] y trigraphs, por ejemplo, en checo <kbd>ch</kbd> es una letra independiente (después de <kbd>h</kbd> y antes de <kbd>i</kbd> luego al ordenar una lista de cadenas, tendrá *fyzika* antes de *chemie*.

Hay muchos más problemas relacionados con el manejo de texto; consulte, por ejemplo, [¿Cómo puedo realizar una comparación de carácter por carácter compatible con Unicode?] [7] para obtener una introducción más amplia y más enlaces a argumentos relacionados.

En general, cuando se trata de texto _internacional_, puede usar esta función simple para enumerar elementos de texto en una cadena (evitando romper los sustitutos y la codificación de Unicode):

    public static class StringExtensions
    {
        public static IEnumerable<string> EnumerateCharacters(this string s)
        {
            if (s == null)
                return Enumerable.Empty<string>();

            var enumerator = StringInfo.GetTextElementEnumerator(s.Normalize());
            while (enumerator.MoveNext())
                yield return (string)enumerator.Value;
        }
    }


[1]: https://en.wikipedia.org/wiki/Grafema
[2]: https://en.wikipedia.org/wiki/Glifo
[3]: https://en.wikipedia.org/wiki/Code_point
[4]: https://en.wikipedia.org/wiki/Character_encoding#Code_unit
[5]: https://en.wikipedia.org/wiki/Combining_character
[6]: https://en.wikipedia.org/wiki/Digraph_(ortografía)
[7]: http://stackoverflow.com/q/27229589/1207195

## Contar caracteres
Si necesita contar _caracteres_ entonces, por las razones explicadas en la sección _Comentarios_, no puede simplemente usar la propiedad Longitud porque es la longitud de la matriz de `System.Char` que no son caracteres sino unidades de código (no código Unicode). puntos ni grafemas). El código correcto es entonces:

    int length = text.EnumerateCharacters().Count();

Una pequeña optimización puede reescribir el método de extensión `EnumerateCharacters()` específicamente para este propósito:

    public static class StringExtensions
    {
        public static int CountCharacters(this string text)
        {
            if (String.IsNullOrEmpty(text))
                return 0;
    
            int count = 0;
            var enumerator = StringInfo.GetTextElementEnumerator(text);
            while (enumerator.MoveNext())
                ++count;
    
            return count;
        }
    }

## Contar caracteres distintos
Si necesita contar caracteres distintos, entonces, por las razones explicadas en la sección *Comentarios*, no puede simplemente usar la propiedad `Longitud` porque es la longitud de la matriz de `System.Char` que no son caracteres sino unidades de código (no puntos de código Unicode ni grafemas). Si, por ejemplo, simplemente escribe `text.Distinct().Count()` obtendrá resultados incorrectos, código correcto:

    int distinctCharactersCount = text.EnumerateCharacters().Count();

Un paso más allá es **contar las ocurrencias de cada carácter**, si el rendimiento no es un problema, simplemente puede hacerlo así (en este ejemplo, independientemente del caso):

    var frequencies = text.EnumerateCharacters()
        .GroupBy(x => x, StringComparer.CurrentCultureIgnoreCase)
        .Select(x => new { Character = x.Key, Count = x.Count() };

## Convertir cadena a/desde otra codificación
Las cadenas .NET contienen `System.Char` (unidades de código UTF-16). Si desea guardar (o administrar) texto con otra codificación, debe trabajar con una matriz de `System.Byte`.

Las conversiones son realizadas por clases derivadas de `System.Text.Encoder` y `System.Text.Decoder` que, juntas, pueden convertir a/desde otra codificación (desde una matriz codificada en bytes _X_ `byte[]` a un UTF-16 codificado `System.String` y viceversa).

Debido a que el codificador/descodificador generalmente funciona muy cerca uno del otro, se agrupan en una clase derivada de `System.Text.Encoding`, las clases derivadas ofrecen conversiones a/desde codificaciones populares (UTF-8, UTF-16, etc. ).

Ejemplos:
=

Convertir una cadena a UTF-8
-
    byte[] data = Encoding.UTF8.GetBytes("This is my text");
---
Convierta datos UTF-8 en una cadena
-
    var text = Encoding.UTF8.GetString(data);

---
Cambiar la codificación de un archivo de texto existente
-

Este código leerá el contenido de un archivo de texto codificado en UTF-8 y lo volverá a guardar codificado como UTF-16. Tenga en cuenta que este código no es óptimo si el archivo es grande porque leerá todo su contenido en la memoria:

    var content = File.ReadAllText(path, Encoding.UTF8);
    File.WriteAllText(content, Encoding.UTF16);

## Comparando cadenas
A pesar de que `String` es un tipo de referencia, el operador `==` compara valores de cadena en lugar de referencias.

Como sabrá, `string` es solo una matriz de caracteres. Pero si cree que la verificación y comparación de la igualdad de cadenas se realiza carácter por carácter, está equivocado. Esta operación es específica de la cultura (consulte las Observaciones a continuación): algunas secuencias de caracteres se pueden tratar como iguales según la [cultura][1].

¡Piénselo dos veces antes de hacer un cortocircuito en la verificación de igualdad comparando `Longitud` [propiedades][2] de dos cadenas!

Utilice sobrecargas de `String.Equals` [método][3] que aceptan valores adicionales de `StringComparison` [enumeración][4], si necesita cambiar el comportamiento predeterminado.


[1]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx
[2]: https://msdn.microsoft.com/library/system.string.length(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/t4411bks(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.stringcomparison(v=vs.110).aspx

## Cuenta las ocurrencias de un personaje
Debido a las razones explicadas en la sección _Comentarios_, no puede simplemente hacer esto (a menos que desee contar las ocurrencias de una unidad de código específica):

    int count = text.Count(x => x == ch);

Necesitas una función más compleja:

    public static int CountOccurrencesOf(this string text, string character)
    {
        return text.EnumerateCharacters()
            .Count(x => String.Equals(x, character, StringComparer.CurrentCulture));
    }

Tenga en cuenta que la comparación de cadenas (a diferencia de la comparación de caracteres, que es invariante de la cultura) siempre debe realizarse de acuerdo con las reglas de una cultura específica.

## Divida la cadena en bloques de longitud fija
No podemos dividir una cadena en puntos arbitrarios (porque un 'System.Char' puede no ser válido solo porque es un carácter combinado o parte de un sustituto), entonces el código debe tener eso en cuenta (tenga en cuenta que con _longitud_ me refiero al número de _grafemas_ no el número de _unidades de código_):

    public static IEnumerable<string> Split(this string value, int desiredLength)
    {
        var characters = StringInfo.GetTextElementEnumerator(value);
        while (characters.MoveNext())
            yield return String.Concat(Take(characters, desiredLength));
    }
    
    private static IEnumerable<string> Take(TextElementEnumerator enumerator, int count)
    {
        for (int i = 0; i < count; ++i)
        {
            yield return (string)enumerator.Current;
    
            if (!enumerator.MoveNext())
                yield break;
        }
    }

## Método virtual Object.ToString()
Todo en .NET es un objeto, por lo que cada tipo tiene `ToString()` [método][1] definido en `Objeto` [clase][2] que se puede anular. La implementación predeterminada de este método solo devuelve el nombre del tipo:

    public class Foo
    {
    }
    
    var foo = new Foo();
    Console.WriteLine(foo); // outputs Foo

`ToString()` se llama implícitamente cuando se concatena valor con una cadena:

    public class Foo
    {
        public override string ToString()
        {
            return "I am Foo";
        }
    }
    
    var foo = new Foo();
    Console.WriteLine("I am bar and "+foo);// outputs I am bar and I am Foo

El resultado de este método también es ampliamente utilizado por las herramientas de depuración. Si, por algún motivo, no desea anular este método, pero desea personalizar la forma en que el depurador muestra el valor de su tipo, use [DebuggerDisplay Attribute][4] ([MSDN][3]):

    // [DebuggerDisplay("Person = FN {FirstName}, LN {LastName}")]
    [DebuggerDisplay("Person = FN {"+nameof(Person.FirstName)+"}, LN {"+nameof(Person.LastName)+"}")]
    public class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set;}
        // ...
    }


[1]: https://msdn.microsoft.com/en-us/library/system.object.tostring(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.object(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.diagnostics.debuggerdisplayattribute(v=vs.110).aspx
[4]: https://www.wikiod.com/es/docs/c%23/1062/attributes/4689/debuggerdisplay-attribute#t=201702221225586559231

## Inmutabilidad de cadenas
Las cadenas son inmutables. Simplemente no puede cambiar la cadena existente. Cualquier operación en la cadena genera una nueva instancia de la cadena que tiene un nuevo valor. Significa que si necesita reemplazar un solo carácter en una cadena muy larga, la memoria se asignará para un nuevo valor.

    string veryLongString = ...
    // memory is allocated
    string newString = veryLongString.Remove(0,1); // removes first character of the string.

Si necesita realizar muchas operaciones con valor de cadena, use `StringBuilder` [clase][1] que está diseñado para la manipulación eficiente de cadenas:

    var sb = new StringBuilder(someInitialString);
    foreach(var str in manyManyStrings)
    {
        sb.Append(str);
    } 
    var finalString = sb.ToString();

[1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

