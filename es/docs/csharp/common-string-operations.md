---
title: "Operaciones comunes de cadenas"
slug: "operaciones-comunes-de-cadenas"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Formatear una cadena
Usa el método `String.Format()` para reemplazar uno o más elementos en la cadena con la representación de cadena de un objeto específico:

    String.Format("Hello {0} Foo {1}", "World", "Bar") //Hello World Foo Bar

## Relleno de una cadena a una longitud fija
    string s = "Foo";
    string paddedLeft = s.PadLeft(5);        // paddedLeft = "  Foo" (pads with spaces by default)
    string paddedRight = s.PadRight(6, '+'); // paddedRight = "Foo+++"
    string noPadded = s.PadLeft(2);          // noPadded = "Foo" (original string is never shortened)


## Invertir correctamente una cadena
La mayoría de las veces, cuando las personas tienen que invertir una cadena, lo hacen más o menos así:

    char[] a = s.ToCharArray();
    System.Array.Reverse(a);
    string r = new string(a);

Sin embargo, lo que estas personas no se dan cuenta es que esto es realmente incorrecto. <br />
Y no me refiero a que falta el cheque NULL.

En realidad, es incorrecto porque un Glyph/GraphemeCluster puede constar de varios puntos de código (también conocidos como caracteres).

Para ver por qué esto es así, primero tenemos que ser conscientes del hecho de lo que realmente significa el término "carácter".

[Referencia 1]
> Carácter es un término sobrecargado que puede significar muchas cosas.
> 
> Un punto de código es la unidad atómica de información. El texto es una secuencia de
> puntos de código. Cada punto de código es un número al que se le da significado por el
> Estándar Unicode.
> 
> Un grafema es una secuencia de uno o más puntos de código que se muestran
> como una sola unidad gráfica que un lector reconoce como una sola
> elemento del sistema de escritura. Por ejemplo, tanto a como ä son
> grafemas, pero pueden consistir en múltiples puntos de código (por ejemplo, ä puede ser
> dos puntos de código, uno para el carácter base a seguido de uno para el
> diáresis; pero también hay un punto de código único heredado alternativo
> representando este grafema). Algunos puntos de código nunca forman parte de ningún
> grafema (por ejemplo, el no ensamblador de ancho cero o las anulaciones direccionales).
> 
> Un glifo es una imagen, generalmente almacenada en una fuente (que es una colección
> de glifos), utilizados para representar grafemas o partes de los mismos. Las fuentes pueden
> componer varios glifos en una sola representación, por ejemplo, si
> la ä anterior es un punto de código único, una fuente puede optar por representarlo como
> dos glifos separados espacialmente superpuestos. Para OTF, el GSUB de la fuente y
> Las tablas GPOS contienen información de sustitución y posicionamiento para hacer
> este trabajo. Una fuente puede contener varios glifos alternativos para el mismo
> grafema, también.

Entonces, en C#, un carácter es en realidad un CodePoint.

Lo que significa que, si inviertes una cadena válida como `Los Miserables`, que puede tener este aspecto

    string s = "Les Mise\u0301rables";

como una secuencia de caracteres, obtendrá:

> selbaŕesiM seL

Como puede ver, el acento está en el carácter R, en lugar del carácter e. <br />
Aunque string.reverse.reverse producirá la cadena original si invierte ambas veces la matriz de caracteres, este tipo de inversión definitivamente NO es el reverso de la cadena original.


Tendrás que invertir solo cada GraphemeCluster. <br />
Entonces, si se hace correctamente, invierte una cadena como esta:


        private static System.Collections.Generic.List<string> GraphemeClusters(string s)
        {
            System.Collections.Generic.List<string> ls = new System.Collections.Generic.List<string>();
    
            System.Globalization.TextElementEnumerator enumerator = System.Globalization.StringInfo.GetTextElementEnumerator(s);
            while (enumerator.MoveNext())
            {
                ls.Add((string)enumerator.Current);
            }
    
            return ls;
        }
    
    
        // this 
        private static string ReverseGraphemeClusters(string s)
        {
            if(string.IsNullOrEmpty(s) || s.Length == 1)
                 return s;
            
            System.Collections.Generic.List<string> ls = GraphemeClusters(s);
            ls.Reverse();
    
            return string.Join("", ls.ToArray());
        }
    
        public static void TestMe()
        {
            string s = "Les Mise\u0301rables";
            // s = "noël";
            string r = ReverseGraphemeClusters(s);
    
            // This would be wrong:
            // char[] a = s.ToCharArray();
            // System.Array.Reverse(a);
            // string r = new string(a);
    
            System.Console.WriteLine(r);
        }

Y, oh alegría, te darás cuenta de que si lo haces correctamente de esta manera, también funcionará para los idiomas asiáticos/del sur de Asia/del este de Asia (y francés/sueco/noruego, etc.)...


[1]: https://stackoverflow.com/questions/27331819/cuál es-la-diferencia-entre-un-carácter-un-código-punto-un-glifo-y-un-grafema

## Obtener x caracteres del lado derecho de una cadena
Visual Basic tiene funciones Left, Right y Mid que devuelven caracteres de Left, Right y Middle de una cadena. Estos métodos no existen en C#, pero se pueden implementar con `Substring()`. Se pueden implementar como métodos de extensión como los siguientes:


       public static class StringExtensions
       {
          /// <summary>
          /// VB Left function
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="numchars"></param>
          /// <returns>Left-most numchars characters</returns>
          public static string Left( this string stringparam, int numchars )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (numchars > stringparam.Length)
                numchars = stringparam.Length;
        
             return stringparam.Substring( 0, numchars );
          }
        
          /// <summary>
          /// VB Right function
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="numchars"></param>
          /// <returns>Right-most numchars characters</returns>
          public static string Right( this string stringparam, int numchars )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (numchars > stringparam.Length)
                numchars = stringparam.Length;
        
             return stringparam.Substring( stringparam.Length - numchars );
          }
        
          /// <summary>
          /// VB Mid function - to end of string
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
          /// <returns>Balance of string beginning at startindex character</returns>
          public static string Mid( this string stringparam, int startindex )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative startindex being passed
             startindex = Math.Abs( startindex );
        
             // Validate numchars parameter        
             if (startindex > stringparam.Length)
                startindex = stringparam.Length;
             
             // C# strings are zero-based, convert passed startindex
             return stringparam.Substring( startindex - 1 );
          }
        
          /// <summary>
          /// VB Mid function - for number of characters
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
          /// <param name="numchars">number of characters to return</param>
          /// <returns>Balance of string beginning at startindex character</returns>
          public static string Mid( this string stringparam, int startindex, int numchars)
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative startindex being passed
             startindex = Math.Abs( startindex );
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (startindex > stringparam.Length)
                startindex = stringparam.Length;
        
             // C# strings are zero-based, convert passed startindex
             return stringparam.Substring( startindex - 1, numchars );
    
           }
        }
Este método de extensión se puede utilizar de la siguiente manera:

    string myLongString = "Hello World!";
    string myShortString = myLongString.Right(6);  // "World!"
    string myLeftString = myLongString.Left(5);    // "Hello"
    string myMidString1 = myLongString.Left(4);    // "lo World"
    string myMidString2 = myLongString.Left(2,3);    // "ell"








## Comprobación de cadenas vacías mediante String.IsNullOrEmpty() y String.IsNullOrWhiteSpace()
    string nullString = null;
    string emptyString = "";
    string whitespaceString = "    ";
    string tabString = "\t";
    string newlineString = "\n";
    string nonEmptyString = "abc123";
    
    bool result;

    result = String.IsNullOrEmpty(nullString);            // true
    result = String.IsNullOrEmpty(emptyString);           // true
    result = String.IsNullOrEmpty(whitespaceString);      // false
    result = String.IsNullOrEmpty(tabString);             // false
    result = String.IsNullOrEmpty(newlineString);         // false
    result = String.IsNullOrEmpty(nonEmptyString);        // false

    result = String.IsNullOrWhiteSpace(nullString);       // true
    result = String.IsNullOrWhiteSpace(emptyString);      // true
    result = String.IsNullOrWhiteSpace(tabString);        // true
    result = String.IsNullOrWhiteSpace(newlineString);    // true
    result = String.IsNullOrWhiteSpace(whitespaceString); // true
    result = String.IsNullOrWhiteSpace(nonEmptyString);   // false

## Recortar caracteres no deseados al principio y/o al final de las cadenas.
`Cadena.Recortar()`
--------

    string x = "   Hello World!    ";
    string y = x.Trim(); // "Hello World!"

    string q = "{(Hi!*";
    string r = q.Trim( '(', '*', '{' ); // "Hi!"


`String.TrimStart()` y `String.TrimEnd()`
--------------------------------------------

    string q = "{(Hi*";
    string r = q.TrimStart( '{' ); // "(Hi*"
    string s = q.TrimEnd( '*' );   // "{(Hi" 


## Construye una cadena a partir de Array
El método `String.Join` nos ayudará a construir una cadena a partir de una matriz/lista de caracteres o cadena. Este método acepta dos parámetros. El primero es el delimitador o el separador que te ayudará a separar cada elemento de la matriz. Y el segundo parámetro es el propio Array.

**Cadena de `matriz de caracteres`:**

    string delimiter=",";
    char[] charArray = new[] { 'a', 'b', 'c' };
    string inputString = String.Join(delimiter, charArray);
**Salida**: `a,b,c` si cambiamos el `delimitador` como `""` entonces la salida se convertirá en `abc`.

**Cadena de `Lista de caracteres`:**

    string delimiter = "|";
    List<char> charList = new List<char>() { 'a', 'b', 'c' };
    string inputString = String.Join(delimiter, charList);

**Salida** : `a|b|c`

**Cadena de `Lista de cadenas`:**

    string delimiter = " ";
    List<string> stringList = new List<string>() { "Ram", "is", "a","boy" };
    string inputString = String.Join(delimiter, stringList);

**Salida**: `Ram es un niño`

**Cadena de `matriz de cadenas`:**

    string delimiter = "_";
    string[] stringArray = new [] { "Ram", "is", "a","boy" };
    string inputString = String.Join(delimiter, stringArray);

**Salida**: `Ram_is_a_boy`


## Formateo usando ToString
Por lo general, usamos el método `String.Format` para fines de formateo, `.ToString` generalmente se usa para convertir otros tipos a cadenas. Podemos especificar el formato junto con el método ToString mientras se realiza la conversión, por lo que podemos evitar un formateo adicional. Déjame explicarte cómo funciona con diferentes tipos;

**Entero a cadena formateada:**

    int intValue = 10;
    string zeroPaddedInteger = intValue.ToString("000"); // Output will be "010"
    string customFormat = intValue.ToString("Input value is 0"); // output will be   "Input value is 10" 
**doble a cadena formateada:**

    double doubleValue = 10.456;
    string roundedDouble = doubleValue.ToString("0.00"); // output 10.46
    string integerPart = doubleValue.ToString("00");    // output 10
    string customFormat = doubleValue.ToString("Input value is 0.0");  // Input value is 10.5

**Formateo de fecha y hora usando ToString**

    DateTime currentDate = DateTime.Now; //  {7/21/2016 7:23:15 PM}
    string dateTimeString = currentDate.ToString("dd-MM-yyyy HH:mm:ss"); // "21-07-2016 19:23:15"
    string dateOnlyString = currentDate.ToString("dd-MM-yyyy"); // "21-07-2016"
    string dateWithMonthInWords = currentDate.ToString("dd-MMMM-yyyy HH:mm:ss"); // "21-July-2016 19:23:15"




## Convertir número decimal a formato binario, octal y hexadecimal
1. Para convertir un número decimal a formato binario, use **base 2**

        Int32 Number = 15;
        Console.WriteLine(Convert.ToString(Number, 2));  //OUTPUT : 1111

2. Para convertir un número decimal a formato octal, use **base 8**

        int Number = 15;
        Console.WriteLine(Convert.ToString(Number, 8));  //OUTPUT : 17

3. Para convertir números decimales a formato hexadecimal, use **base 16**

        var Number = 15;
        Console.WriteLine(Convert.ToString(Number, 16));  //OUTPUT : f



## Dividir una cadena por carácter específico
    string helloWorld = "hello world, how is it going?";
    string[] parts1 = helloWorld.Split(',');

    //parts1: ["hello world", " how is it going?"]

    string[] parts2 = helloWorld.Split(' ');

    //parts2: ["hello", "world,", "how", "is", "it", "going?"]


## Obtener subcadenas de una cadena dada
    string helloWorld = "Hello World!";
    string world = helloWorld.Substring(6); //world = "World!"
    string hello = helloWorld.Substring(0,5); // hello = "Hello"

`Substring` devuelve la cadena desde un índice dado, o entre dos índices (ambos inclusive).

## Determinar si una cadena comienza con una secuencia dada
    string HelloWorld = "Hello World";
    HelloWorld.StartsWith("Hello"); // true
    HelloWorld.StartsWith("Foo"); // false


**Encontrar una cadena dentro de una cadena**

Utilizando el
[`System.String.Contains`][1] puede averiguar si existe una cadena en particular dentro de una cadena. El método devuelve un valor booleano, verdadero si la cadena existe o falso.

    string s = "Hello World";
    bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 


[1]: https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx

## Unir una matriz de cadenas en una nueva
    var parts = new[] { "Foo", "Bar", "Fizz", "Buzz"};
    var joined = string.Join(", ", parts);

    //joined = "Foo, Bar, Fizz, Buzz"

## Obtener un carácter en un índice específico y enumerar la cadena
Puede usar el método `Substring` para obtener cualquier número de caracteres de una cadena en cualquier ubicación determinada. Sin embargo, si solo desea un solo carácter, puede usar el indexador de cadenas para obtener un solo carácter en cualquier índice dado, como lo hace con una matriz:

    string s = "hello";
    char c = s[1]; //Returns 'e'

Tenga en cuenta que el tipo de retorno es `char`, a diferencia del método `Substring` que devuelve un tipo `string`.

También puede usar el indexador para iterar a través de los caracteres de la cadena:

    string s = "hello";
    foreach (char c in s)
        Console.WriteLine(c);
    /********* This will print each character on a new line:
    h
    e
    l
    l
    o
    **********/

## Dividir una cadena por otra cadena
    string str = "this--is--a--complete--sentence";
    string[] tokens = str.Split(new[] { "--" }, StringSplitOptions.None);

Resultado:

>[ "esto", "es", "un", "completo", "frase" ]

## Reemplazar una cadena dentro de una cadena
Con el método [`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx), puede reemplazar parte de una cadena con otra cuerda.

    string s = "Hello World";
     s = s.Replace("World", "Universe"); // s = "Hello Universe"
Se reemplazan todas las apariciones de la cadena de búsqueda.

Este método también se puede usar para eliminar parte de una cadena, usando [`String.Empty`](https://msdn.microsoft.com/en-us/library/system.string.empty(v=vs.110 ).aspx) campo:

    string s = "Hello World";
    s = s.Replace("ell", String.Empty); // s = "Ho World"


## Cambiando el caso de los caracteres dentro de una Cadena
La clase [`System.String`](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) admite varios métodos para convertir entre mayúsculas y minúsculas caracteres en una cadena.

- [`System.String.ToLowerInvariant`](https://msdn.microsoft.com/en-us/library/system.string.tolowerinvariant(v=vs.110).aspx) se usa para devolver un objeto String convertido a minúsculas.


- [`System.String.ToUpperInvariant`](https://msdn.microsoft.com/en-us/library/system.string.toupperinvariant(v=vs.110).aspx) se usa para devolver un objeto String convertido a mayúsculas.

**Nota:** La razón para usar las versiones *invariantes* de estos métodos es evitar la producción de letras específicas de la cultura inesperadas. Esto se explica [aquí en detalle] (http://stackoverflow.com/a/19778131/1379664).

Ejemplo:

    string s = "My String";
    s = s.ToLowerInvariant(); // "my string"
    s = s.ToUpperInvariant(); // "MY STRING"


Tenga en cuenta que *puede* elegir especificar una **[Cultura](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo(v=vs.110).aspx)** al convertir a minúsculas y mayúsculas mediante [String.ToLower(CultureInfo)](https://msdn.microsoft.com/en-us/library/s8z5yt00(v=vs.110).aspx) y [String.ToUpper (CultureInfo)](https://msdn.microsoft.com/en-us/library/24kc78ka(v=vs.110).aspx) métodos en consecuencia.



## Concatenar una matriz de cadenas en una sola cadena
El método [`System.String.Join`](https://msdn.microsoft.com/en-us/library/57a79xd0(v=vs.110).aspx) permite concatenar todos los elementos en una matriz de cadenas, usando un separador especificado entre cada elemento:

    string[] words = {"One", "Two", "Three", "Four"};
    string singleString = String.Join(",", words); // singleString = "One,Two,Three,Four"


## Concatenación de cadenas
La concatenación de cadenas se puede realizar mediante el método [`System.String.Concat`](https://msdn.microsoft.com/en-us/library/system.string.concat(v=vs.110).aspx) , o (mucho más fácil) usando el operador `+`:

    string first = "Hello ";
    string second = "World";

    string concat = first + second; // concat = "Hello World"
    concat = String.Concat(first, second); // concat = "Hello World"

En C# 6 esto se puede hacer de la siguiente manera:

    string concat = $"{first},{second}";



