---
title: "Concatenar cadenas"
slug: "concatenar-cadenas"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Si está creando una cadena dinámica, es una buena práctica optar por la clase `StringBuilder` en lugar de unir cadenas usando el método + o `Concat` ya que cada +/`Concat` crea un nuevo objeto de cadena cada vez que se ejecuta.

## + Operador
    string s1 = "string1";
    string s2 = "string2";
    
    string s3 = s1 + s2; // "string1string2"

## Concatenar cadenas usando System.Text.StringBuilder
La concatenación de cadenas usando un [StringBuilder][1] puede ofrecer ventajas de rendimiento sobre la concatenación simple de cadenas usando `+`. Esto se debe a la forma en que se asigna la memoria. Las cadenas se reasignan con cada concatenación, los StringBuilders asignan memoria en bloques y solo se reasignan cuando se agota el bloque actual. Esto puede marcar una gran diferencia cuando se realizan muchas concatenaciones pequeñas.

    StringBuilder sb = new StringBuilder();
    for (int i = 1; i <= 5; i++)
    {
        sb.Append(i);
        sb.Append(" ");
    }
    Console.WriteLine(sb.ToString()); // "1 2 3 4 5 "

Las llamadas a `Append()` se pueden conectar en cadena, porque devuelve una referencia a `StringBuilder`:

    StringBuilder sb = new StringBuilder();
    sb.Append("some string ")
      .Append("another string");


[1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

## Concatenar elementos de matriz de cadenas usando String.Join
El método `String.Join` se puede utilizar para concatenar varios elementos de una matriz de cadenas.

    string[] value = {"apple", "orange", "grape", "pear"};
    string separator = ", ";

    string result = String.Join(separator, value, 1, 2);
    Console.WriteLine(result);

> Produce el siguiente resultado: "naranja, uva"

Este ejemplo utiliza la sobrecarga `String.Join(String, String[], Int32, Int32)`, que especifica el índice de inicio y el recuento además del separador y el valor.

Si no desea utilizar startIndex y contar sobrecargas, puede unir todas las cadenas proporcionadas. Como esto:
    
    string[] value = {"apple", "orange", "grape", "pear"};
    string separator = ", ";
    string result = String.Join(separator, value);
    Console.WriteLine(result);

que producirá;

> manzana, naranja, uva, pera

## Concatenación de dos cadenas usando $
$ proporciona un método fácil y conciso para concatenar varias cadenas.

    var str1 = "text1";
    var str2 = " ";
    var str3 = "text3";
    string result2 = $"{str1}{str2}{str3}"; //"text1 text3"

