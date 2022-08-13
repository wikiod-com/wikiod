---
title: "Cadenas textuales"
slug: "cadenas-textuales"
draft: false
images: []
weight: 9434
type: docs
toc: true
---

## Sintaxis
- @"Las cadenas textuales son cadenas cuyo contenido no se escapa, por lo que en este caso \n no representa el carácter de nueva línea sino dos caracteres individuales: \ y n. Las cadenas textuales se crean anteponiendo el contenido de la cadena con el carácter @"

- @"Para escapar de las comillas, se utilizan ""comillas dobles""."

Para concatenar literales de cadena, use el símbolo @ al comienzo de cada cadena.

    var combinedString = @"\t means a tab" + @" and \n means a newline";

## Cadenas textuales interpoladas
Las cadenas textuales se pueden combinar con las nuevas funciones de https://www.wikiod.com/es/docs/c%23/24/c-sharp-6-0-features/49/string-interpolation que se encuentran en C#6.

    Console.WriteLine($@"Testing \n 1 2 {5 - 2}
    New line");

**Producción:**

> Prueba \n 1 2 3
> Nueva línea

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/cWyQE2)

Como se esperaba de una cadena textual, las barras invertidas se ignoran como caracteres de escape. Y como se esperaba de una cadena interpolada, cualquier expresión entre llaves se evalúa antes de insertarse en la cadena en esa posición.


## Escape de comillas dobles
Las comillas dobles dentro de cadenas textuales se pueden escapar usando 2 comillas dobles secuenciales `""` para representar una comilla doble `"` en la cadena resultante.

    var str = @"""I don't think so,"" he said.";
    Console.WriteLine(str);

**Producción:**
> "No lo creo", dijo.

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/c4OJoq)

## Las cadenas textuales indican al compilador que no use escapes de caracteres
En una cadena normal, el carácter de barra invertida es el carácter de escape, que indica al compilador que mire los siguientes caracteres para determinar el carácter real de la cadena. ([Lista completa de escapes de personajes][1])

En cadenas textuales, no hay caracteres de escape (excepto `""` que se convierte en `"`).
Para usar una cadena textual, simplemente anteponga un `@` antes de las comillas iniciales.

Esta cadena textual

    var filename = @"c:\temp\newfile.txt"

**Producción:**

>c:\temp\nuevoarchivo.txt

A diferencia de usar una cadena ordinaria (no textual):

    var filename = "c:\temp\newfile.txt"

eso generará:

    c:    emp
    ewfile.txt

utilizando el carácter de escape. (El `\t` se reemplaza con un carácter de tabulación y el `\n` se reemplaza con una nueva línea).

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/7kslXQ)






[1]: https://www.wikiod.com/es/docs/c%23/39/string-escape-sequences#t=201607172257361795538&a=syntax

## Cadenas multilínea
    var multiLine = @"This is a 

    multiline paragraph";

**Producción:**
>Este es un
>
> párrafo de varias líneas

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/kfOUcH)

Las cadenas de varias líneas que contienen comillas dobles también se pueden escapar como si estuvieran en una sola línea, porque son cadenas textuales.
 
    var multilineWithDoubleQuotes = @"I went to a city named

                            ""San Diego""

                          during summer vacation.";

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/0hwJpf)

*Cabe señalar que los espacios/tabulaciones al inicio de las líneas 2 y 3 aquí sí están presentes en el valor de la variable; verifique [esta pregunta] (http://stackoverflow.com/questions/7178136/multiline-formatting-for-verbatim-strings-in-c-sharp-prefix-with) para posibles soluciones.*


