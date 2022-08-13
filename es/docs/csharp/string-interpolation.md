---
title: "Interpolación de cadenas"
slug: "interpolacion-de-cadenas"
draft: false
images: []
weight: 9769
type: docs
toc: true
---

## Sintaxis
- $"contenido {expresión} contenido"
- $"contenido {expresión:formato} contenido"
- $"contenido {expresión} {{contenido entre llaves}} contenido}"
- $"contenido {expresión:formato} {{contenido entre llaves}} contenido}"

La interpolación de cadenas es una forma abreviada del método `string.Format()` que facilita la creación de cadenas con valores de variables y expresiones dentro de ellas.

    var name = "World";
    var oldWay = string.Format("Hello, {0}!", name);  // returns "Hello, World"
    var newWay = $"Hello, {name}!";                   // returns "Hello, World"

## Formatear fechas en cadenas
    var date = new DateTime(2015, 11, 11);
    var str = $"It's {date:MMMM d, yyyy}, make a wish!";
    System.Console.WriteLine(str);

También puede usar el método [`DateTime.ToString`][1] para formatear el objeto `DateTime`. Esto producirá el mismo resultado que el código anterior.

    var date = new DateTime(2015, 11, 11);
    var str = date.ToString("MMMM d, yyyy");
    str = "It's " + str + ", make a wish!";
    Console.WriteLine(str);

**Producción:**
>Es 11 de noviembre de 2015, ¡pide un deseo!

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/DpRwV5)

[Demostración en vivo usando DateTime.ToString](https://dotnetfiddle.net/YnV9J0)

> **Nota:** `MM` significa meses y `mm` minutos. Tenga mucho cuidado al usarlos, ya que los errores pueden introducir errores que pueden ser difíciles de descubrir.


[1]: https://msdn.microsoft.com/en-us/library/zdtaw1bw(v=vs.110).aspx

## Rellenando la salida
La cadena se puede formatear para aceptar un parámetro de relleno que especificará cuántas posiciones de caracteres usará la cadena insertada:

    ${value, padding}

> **NOTA:** Los valores de relleno positivos indican relleno izquierdo y negativo
> los valores de relleno indican el relleno correcto.

**Relleno izquierdo**
----

Un relleno izquierdo de 5 (agrega 3 espacios antes del valor del número, por lo que ocupa un total de 5 posiciones de caracteres en la cadena resultante).

    var number = 42;
    var str = $"The answer to life, the universe and everything is {number, 5}.";
    //str is "The answer to life, the universe and everything is    42.";
    //                                                           ^^^^^
    System.Console.WriteLine(str);
    
**Producción:**
       
    The answer to life, the universe and everything is    42.
[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/PpZXmk)

**Relleno derecho**
----

El relleno derecho, que utiliza un valor de relleno negativo, agregará espacios al final del valor actual.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, -5}.";
    //str is "The answer to life, the universe and everything is 42   .";
    //                                                           ^^^^^
    System.Console.WriteLine(str);

**Producción:**

    The answer to life, the universe and everything is 42   .

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/QtKjGF)

**Relleno con especificadores de formato**
----

También puede usar los especificadores de formato existentes junto con el relleno.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, 5:f1}";
    //str is "The answer to life, the universe and everything is 42.1 ";
    //                                                           ^^^^^

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/34ZxP0)



## Expresiones
Las expresiones completas también se pueden usar en cadenas interpoladas.

    var StrWithMathExpression = $"1 + 2 = {1 + 2}"; // -> "1 + 2 = 3"
    
    string world = "world";
    var StrWithFunctionCall = $"Hello, {world.ToUpper()}!"; // -> "Hello, WORLD!"


[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/u9lzeg)



## Uso sencillo
    var name = "World";
    var str = $"Hello, {name}!";
    //str now contains: "Hello, World!";

## Detrás de escena

Internamente esto

    $"Hello, {name}!" 

Se compilará a algo como esto:

    string.Format("Hello, {0}!", name);

    


## Formatear números en cadenas
Puede usar dos puntos y la [sintaxis de formato numérico estándar](https://msdn.microsoft.com/en-us/library/dwhawy9k.aspx) para controlar el formato de los números.

    var decimalValue = 120.5;

    var asCurrency = $"It costs {decimalValue:C}";
    // String value is "It costs $120.50" (depending on your local currency settings)

    var withThreeDecimalPlaces = $"Exactly {decimalValue:F3}";
    // String value is "Exactly 120.500"

    var integerValue = 57;

    var prefixedIfNecessary = $"{integerValue:D5}";
    // String value is "00057"


[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/z2XbG7)

