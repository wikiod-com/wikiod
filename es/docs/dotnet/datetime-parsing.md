---
title: "Análisis de fecha y hora"
slug: "analisis-de-fecha-y-hora"
draft: false
images: []
weight: 9816
type: docs
toc: true
---

## Análisis exacto
    var dateString = "2015-11-24";

    var date = DateTime.ParseExact(dateString, "yyyy-MM-dd", null);
    Console.WriteLine(date);

> 24/11/2015 00:00:00

Tenga en cuenta que pasar `CultureInfo.CurrentCulture` como tercer parámetro es idéntico a pasar `null`. O bien, puede pasar una cultura específica.

**Cadenas de formato**

*La cadena de entrada puede estar en cualquier formato que coincida con la cadena de formato*

    var date = DateTime.ParseExact("24|201511", "dd|yyyyMM", null);
    Console.WriteLine(date);

> 24/11/2015 00:00:00

*Los caracteres que no son especificadores de formato se tratan como literales*

    var date = DateTime.ParseExact("2015|11|24", "yyyy|MM|dd", null);
    Console.WriteLine(date);

> 24/11/2015 00:00:00

*El caso importa para los especificadores de formato*

    var date = DateTime.ParseExact("2015-01-24 11:11:30", "yyyy-mm-dd hh:MM:ss", null);
    Console.WriteLine(date);

> 24/11/2015 11:01:30

Tenga en cuenta que los valores de mes y minuto se analizaron en los destinos incorrectos.

*Las cadenas de formato de un solo carácter deben ser uno de los formatos estándar*

    var date = DateTime.ParseExact("11/24/2015", "d", new CultureInfo("en-US"));
    var date = DateTime.ParseExact("2015-11-24T10:15:45", "s", null);
    var date = DateTime.ParseExact("2015-11-24 10:15:45Z", "u", null);

**Excepciones**

*ArgumentNullException*

    var date = DateTime.ParseExact(null, "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", null, null);

*Excepción de formato*

    var date = DateTime.ParseExact("", "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", "", null);
    var date = DateTime.ParseExact("2015-0C-24", "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", "yyyy-QQ-dd", null);

    // Single-character format strings must be one of the standard formats
    var date = DateTime.ParseExact("2015-11-24", "q", null);

    // Format strings must match the input exactly* (see next section)
    var date = DateTime.ParseExact("2015-11-24", "d", null); // Expects 11/24/2015 or 24/11/2015 for most cultures

**Manejo de múltiples formatos posibles**

    var date = DateTime.ParseExact("2015-11-24T10:15:45", 
      new [] { "s", "t", "u", "yyyy-MM-dd" }, // Will succeed as long as input matches one of these
      CultureInfo.CurrentCulture, DateTimeStyles.None);

**Manejar las diferencias culturales**

    var dateString = "10/11/2015";
    var date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-US"));
    Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

> Día: 11; Mes: 10

    date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-GB"));
    Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

> Día: 10; Mes: 11


## Intenta analizar
Este método acepta una cadena como entrada, intenta analizarla en un `DateTime` y devuelve un resultado booleano que indica éxito o fracaso. Si la llamada tiene éxito, la variable pasada como el parámetro `out` se completa con el resultado analizado.

Si el análisis falla, la variable pasada como el parámetro `out` se establece en el valor predeterminado, `DateTime.MinValue`.

**TryParse(cadena, salida DateTime)**

    DateTime parsedValue;

    if (DateTime.TryParse("monkey", out parsedValue))
    {
       Console.WriteLine("Apparently, 'monkey' is a date/time value. Who knew?");
    }

Este método intenta analizar la cadena de entrada según la configuración regional del sistema y los formatos conocidos, como ISO 8601 y otros formatos comunes.

    DateTime.TryParse("11/24/2015 14:28:42", out parsedValue); // true
    DateTime.TryParse("2015-11-24 14:28:42", out parsedValue); // true
    DateTime.TryParse("2015-11-24T14:28:42", out parsedValue); // true
    DateTime.TryParse("Sat, 24 Nov 2015 14:28:42", out parsedValue); // true

Dado que este método no acepta información cultural, utiliza la configuración regional del sistema. Esto puede conducir a resultados inesperados.

    // System set to en-US culture
    bool result = DateTime.TryParse("24/11/2015", out parsedValue);
    Console.WriteLine(result);

> Falso

    // System set to en-GB culture
    bool result = DateTime.TryParse("11/24/2015", out parsedValue);
    Console.WriteLine(result);

> Falso

    // System set to en-GB culture
    bool result = DateTime.TryParse("10/11/2015", out parsedValue);
    Console.WriteLine(result);

> Verdadero

Tenga en cuenta que si se encuentra en los EE. UU., es posible que se sorprenda de que el resultado analizado sea el 10 de noviembre, no el 11 de octubre.

**TryParse(cadena, IFormatProvider, DateTimeStyles, out DateTime)**

    if (DateTime.TryParse(" monkey ", new CultureInfo("en-GB"),
        DateTimeStyles.AllowLeadingWhite | DateTimeStyles.AllowTrailingWhite, out parsedValue)
    {
        Console.WriteLine("Apparently, ' monkey ' is a date/time value. Who knew?");
    }

A diferencia de su método hermano, esta sobrecarga permite especificar una cultura y un estilo específicos. Pasar `null` para el parámetro `IFormatProvider` utiliza la referencia cultural del sistema.

*Excepciones*

Tenga en cuenta que es posible que este método arroje una excepción bajo ciertas condiciones. Estos se relacionan con los parámetros introducidos para esta sobrecarga: `IFormatProvider` y `DateTimeStyles`.

* `NotSupportedException`: `IFormatProvider` especifica una cultura neutral
* `ArgumentException`: `DateTimeStyles` no es una opción válida o contiene marcas incompatibles como `AssumeLocal` y `AssumeUniversal`.

## TryParseExact
Este método se comporta como una combinación de `TryParse` y `ParseExact`: permite especificar formatos personalizados y devuelve un resultado booleano que indica el éxito o el fracaso en lugar de lanzar una excepción si el análisis falla.

**TryParseExact(cadena, cadena, IFormatProvider, DateTimeStyles, out DateTime)**

Esta sobrecarga intenta analizar la cadena de entrada contra un formato específico. La cadena de entrada debe coincidir con ese formato para poder analizarse.

    DateTime.TryParseExact("11242015", "MMddyyyy", null, DateTimeStyles.None, out parsedValue); // true

**TryParseExact(string, string[], IFormatProvider, DateTimeStyles, out DateTime)**

Esta sobrecarga intenta analizar la cadena de entrada contra una matriz de formatos. La cadena de entrada debe coincidir con al menos un formato para poder analizarse.

    DateTime.TryParseExact("11242015", new [] { "yyyy-MM-dd", "MMddyyyy" }, null, DateTimeStyles.None, out parsedValue); // true


