---
title: "Cadena.Formato"
slug: "cadenaformato"
draft: false
images: []
weight: 9519
type: docs
toc: true
---

Los métodos `Format` son un conjunto de [sobrecargas][1] en la clase [`System.String`][2] que se utilizan para crear cadenas que combinan objetos en representaciones de cadenas específicas. Esta información se puede aplicar a [`String.Format`][1], varios métodos `WriteLine` así como otros métodos en el marco .NET.

[1]: https://msdn.microsoft.com/en-us/library/system.string.format(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx

## Sintaxis
- string.Format(formato de cadena, objeto params[] args)
- string.Format (proveedor de IFormatProvider, formato de cadena, objeto de parámetros [] argumentos)
- $"cadena {texto} blablabla" // Desde C#6

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| formato | Una [cadena de formato compuesto][1], que define la forma en que *args* debe combinarse en una cadena. |
| argumentos | Una secuencia de objetos que se combinarán en una cadena. Dado que esto usa un argumento [`params`][2], puede usar una lista de argumentos separados por comas o una matriz de objetos real. |
| proveedor | Una colección de formas de formatear objetos en cadenas. Los valores típicos incluyen [CultureInfo.InvariantCulture][3] y [CultureInfo.CurrentCulture][4]. |


[1]: https://msdn.microsoft.com/en-us/library/txafckwd(v=vs.110).aspx
[2]: https://www.wikiod.com/es/docs/c%23/26/keywords/2513/params#t=201607212143476676934
[3]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.invariantculture(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx

Notas:

- `String.Format()` maneja argumentos `nulos` sin lanzar una excepción.
- Hay sobrecargas que reemplazan el parámetro `args` con uno, dos o tres parámetros de objeto.



## Desde C# 6.0
<!-- si la versión [gte 6.0] -->

Desde C# 6.0, es posible utilizar la interpolación de cadenas en lugar de `String.Format`.

    string name = "John";
    string lastname = "Doe";
    Console.WriteLine($"Hello {name} {lastname}!");

> ¡Hola John Doe!

<sup>Más ejemplos de esto en el tema Características de C# 6.0: https://www.wikiod.com/es/docs/c%23/24/c-sharp-6-0-features/49/string-interpolation#t=201607220912379524818. </sup>

<!-- versión final si -->
 

## Lugares donde String.Format está 'incrustado' en el marco
Hay varios lugares donde puede usar `String.Format` *indirectamente*: El secreto es buscar la sobrecarga con la firma `formato de cadena, params object[] args`, por ejemplo:

    Console.WriteLine(String.Format("{0} - {1}", name, value));

Se puede reemplazar con una versión más corta:

    Console.WriteLine("{0} - {1}", name, value);

Hay otros métodos que también usan `String.Format`, por ejemplo:

    Debug.WriteLine(); // and Print()
    StringBuilder.AppendFormat();

## Crear un proveedor de formato personalizado
    public class CustomFormat : IFormatProvider, ICustomFormatter
    {
        public string Format(string format, object arg, IFormatProvider formatProvider)
        {
            if (!this.Equals(formatProvider))
            {
                return null;
            }

            if (format == "Reverse")
            {
                return String.Join("", arg.ToString().Reverse());
            }

            return arg.ToString();
        }

        public object GetFormat(Type formatType)
        {
            return formatType==typeof(ICustomFormatter) ? this:null;
        }
    }

Uso:

    String.Format(new CustomFormat(), "-> {0:Reverse} <-", "Hello World");

Producción:

    -> dlroW olleH <-

## Formato de fecha
    DateTime date = new DateTime(2016, 07, 06, 18, 30, 14);
    // Format: year, month, day hours, minutes, seconds

    Console.Write(String.Format("{0:dd}",date)); 

    //Format by Culture info
    String.Format(new System.Globalization.CultureInfo("mn-MN"),"{0:dddd}",date);

<!-- si la versión [gte 6.0] -->
    Console.Write($"{date:ddd}");
<!-- versión final si -->

producción :

    06
    Лхагва
    06

| Especificador| Significado| Muestra| Resultado|
| ------ | ------ | ------ | ------ |
|d| Fecha |`{0:d}`|7/6/2016|
|dd| Día, relleno con ceros |`{0:dd}`|06|
|ddd|Nombre de día corto|`{0:ddd}`|Mié|
|dddd|Nombre completo del día|`{0:dddd}`|Miércoles|
|D|Fecha larga|`{0:D}`|miércoles, 6 de julio de 2016|
|f|Fecha y hora completa, breve|`{0:f}`|miércoles, 6 de julio de 2016 18:30|
|ff|Segundas fracciones, 2 dígitos|`{0:ff}`|20|
|fff|Segundas fracciones, 3 dígitos|`{0:fff}`|201|
|ffff|Segundas fracciones, 4 dígitos|`{0:ffff}`|2016|
|F|Fecha y hora completas, larga|`{0:F}`|miércoles, 6 de julio de 2016 18:30:14|
|g|Fecha y hora predeterminadas|`{0:g}`|7/6/2016 6:30 PM|
|gg|Era|`{0:gg}`|AD|
|hh|Hora (2 dígitos, 12H)|`{0:hh}`|06|
|HH|Hora (2 dígitos, 24H)|`{0:HH}`|18|
|M|Mes y día|`{0:M}`|6 de julio|
|mm|Minutos, relleno con ceros|`{0:mm}`|30|
|MM|Mes, relleno con ceros|`{0:MM}`|07|
|MMM|Nombre del mes de 3 letras|`{0:MMM}`|Jul|
|MMMM|Nombre completo del mes|`{0:MMMM}`|Julio|
|ss|Segundos|`{0:ss}`|14|
|r| RFC1123 fecha|`{0:r}`|miércoles, 06 de julio de 2016 18:30:14 GMT|
|s| Cadena de fecha ordenable|`{0:s}`|2016-07-06T18:30:14|
|t| Tiempo corto |`{0:t}`|6:30 PM|
|T|Mucho tiempo|`{0:T}`|6:30:14 PM|
|ht|AM/PM|`{0:ht}`|PM|
|u|Hora local clasificable universal|`{0:u}`|2016-07-06 18:30:14Z|
|T| Universal GMT|`{0:U}`|Miércoles, 6 de julio de 2016 9:30:14 a.m.|
|S| Mes y año|`{0:Y}`|julio 2016|
|aa|año de 2 dígitos|`{0:aa}`|16|
|yyyy|año de 4 dígitos|`{0:yyyy}`|2016|
|zz|desplazamiento de zona horaria de 2 dígitos|`{0:zz}`|+09|
|zzz|compensación de zona horaria completa|`{0:zzz}`|+09:00|

## Formato de moneda
El especificador de formato "c" (o moneda) convierte un número en una cadena que representa una cantidad de moneda.

    string.Format("{0:c}", 112.236677) // $112.23 - defaults to system

## Precisión ##
El valor predeterminado es 2. Utilice c1, c2, c3, etc. para controlar la precisión.

    string.Format("{0:C1}", 112.236677) //$112.2
    string.Format("{0:C3}", 112.236677) //$112.237
    string.Format("{0:C4}", 112.236677) //$112.2367
    string.Format("{0:C9}", 112.236677) //$112.236677000

## Símbolo de moneda ##

1. Pase la instancia `CultureInfo` para usar el símbolo cultural personalizado.


    string.Format(new CultureInfo("en-US"), "{0:c}", 112.236677); //$112.24
    string.Format(new CultureInfo("de-DE"), "{0:c}", 112.236677); //112,24 €
    string.Format(new CultureInfo("hi-IN"), "{0:c}", 112.236677); //₹ 112.24


2. Use cualquier cadena como símbolo de moneda. Use `NumberFormatInfo` para personalizar el símbolo de moneda.


    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;
    nfi = (NumberFormatInfo) nfi.Clone();
    nfi.CurrencySymbol = "?";
    string.Format(nfi, "{0:C}", 112.236677); //?112.24
    nfi.CurrencySymbol = "?%^&";
    string.Format(nfi, "{0:C}", 112.236677); //?%^&112.24

## Posición del símbolo de moneda ##

Utilice [CurrencyPositivePattern][1] para valores positivos y [CurrencyNegativePattern][2] para valores negativos.

    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
    nfi.CurrencyPositivePattern = 0;
    string.Format(nfi, "{0:C}", 112.236677); //$112.24 - default
    nfi.CurrencyPositivePattern = 1;
    string.Format(nfi, "{0:C}", 112.236677); //112.24$
    nfi.CurrencyPositivePattern = 2;
    string.Format(nfi, "{0:C}", 112.236677); //$ 112.24
    nfi.CurrencyPositivePattern = 3; 
    string.Format(nfi, "{0:C}", 112.236677); //112.24 $

El uso del patrón negativo es el mismo que el patrón positivo. Muchos más casos de uso, consulte el enlace original.

## Separador decimal personalizado ##

    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
    nfi.CurrencyPositivePattern = 0;
    nfi.CurrencyDecimalSeparator = "..";
    string.Format(nfi, "{0:C}", 112.236677); //$112..24

[1]: https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencypositivepattern(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencynegativepattern(v=vs.110).aspx

## Usando un formato de número personalizado
`NumberFormatInfo` se puede utilizar para dar formato a números enteros y flotantes.

    // invariantResult is "1,234,567.89"
    var invarianResult = string.Format(CultureInfo.InvariantCulture, "{0:#,###,##}", 1234567.89);

    // NumberFormatInfo is one of classes that implement IFormatProvider
    var customProvider = new NumberFormatInfo
    {
        NumberDecimalSeparator = "_NS_", // will be used instead of ','
        NumberGroupSeparator = "_GS_", // will be used instead of '.'
    };

    // customResult is "1_GS_234_GS_567_NS_89"
    var customResult = string.Format(customProvider, "{0:#,###.##}", 1234567.89);



## Alinear a la izquierda/derecha, rellenar con espacios
El segundo valor entre llaves dicta la longitud de la cadena de reemplazo.
Al ajustar el segundo valor para que sea positivo o negativo, se puede cambiar la alineación de la cuerda.

    string.Format("LEFT:  string: ->{0,-5}<- int: ->{1,-5}<-", "abc", 123);
    string.Format("RIGHT: string: ->{0,5}<- int: ->{1,5}<-", "abc", 123);

Producción:

    LEFT:  string: ->abc  <- int: ->123  <-
    RIGHT: string: ->  abc<- int: ->  123<-


## Formatos numéricos
    // Integral types as hex
    string.Format("Hexadecimal: byte2: {0:x2}; byte4: {0:X4}; char: {1:x2}", 123, (int)'A');

    // Integers with thousand separators
    string.Format("Integer, thousand sep.: {0:#,#}; fixed length: >{0,10:#,#}<", 1234567);

    // Integer with leading zeroes
    string.Format("Integer, leading zeroes: {0:00}; ", 1);

    // Decimals
    string.Format("Decimal, fixed precision: {0:0.000}; as percents: {0:0.00%}", 0.12);

Producción:

    Hexadecimal: byte2: 7b; byte4: 007B; char: 41
    Integer, thousand sep.: 1,234,567; fixed length: > 1,234,567<
    Integer, leading zeroes: 01; 
    Decimal, fixed precision: 0.120; as percents: 12.00%


## Escapar de corchetes dentro de una expresión String.Format()
    string outsidetext = "I am outside of bracket";
    string.Format("{{I am in brackets!}} {0}", outsidetext);

    //Outputs "{I am in brackets!} I am outside of bracket"

## Encadenar()
El método ToString() está presente en todos los tipos de objetos de referencia. Esto se debe a que todos los tipos de referencia se derivan de Object que tiene el método ToString(). El método ToString() en la clase base del objeto devuelve el nombre del tipo. El siguiente fragmento imprimirá "Usuario" en la consola.

    public class User
    {
        public string Name { get; set; }
        public int Id { get; set; }
    }

    ...

    var user = new User {Name = "User1", Id = 5};
    Console.WriteLine(user.ToString());


Sin embargo, la clase User también puede anular ToString() para modificar la cadena que devuelve. El siguiente fragmento de código imprime "Id: 5, Nombre: Usuario1" en la consola.

    public class User
    {
        public string Name { get; set; }
        public int Id { get; set; }
        public override ToString()
        {
            return string.Format("Id: {0}, Name: {1}", Id, Name);
        }
    }

    ...

    var user = new User {Name = "User1", Id = 5};
    Console.WriteLine(user.ToString());


## Relación con ToString()
Si bien el método `String.Format()` es ciertamente útil para formatear datos como cadenas, a menudo puede ser un poco excesivo, especialmente cuando se trata de un solo objeto, como se ve a continuación:

    String.Format("{0:C}", money);  // yields "$42.00"

Un enfoque más fácil podría ser simplemente usar el método `ToString()` disponible en todos los objetos dentro de C#. Admite todas las mismas [cadenas de formato estándar y personalizadas] (https://msdn.microsoft.com/en-us/library/dwhawy9k(v=vs.110).aspx), pero no requiere el parámetro necesario mapeo ya que solo habrá un único argumento:

    money.ToString("C");  // yields "$42.00"

**Advertencias y restricciones de formato**
---

Si bien este enfoque puede ser más simple en algunos escenarios, el enfoque `ToString()` está limitado con respecto a agregar relleno izquierdo o derecho como lo haría dentro del método `String.Format()`:

    String.Format("{0,10:C}", money);  // yields "    $42.00"

Para lograr este mismo comportamiento con el método `ToString()`, necesitará usar otro método como `PadLeft()` o `PadRight()` respectivamente:

    money.ToString("C").PadLeft(10);  // yields "    $42.00"

