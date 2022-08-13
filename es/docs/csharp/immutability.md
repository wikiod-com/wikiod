---
title: "Inmutabilidad"
slug: "inmutabilidad"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Clase System.String


## Cadenas e inmutabilidad
Los tipos inmutables son tipos que, cuando se modifican, crean una nueva versión del objeto en la memoria, en lugar de cambiar el objeto existente en la memoria. El ejemplo más simple de esto es el tipo `string` integrado.

Tomando el siguiente código, que agrega "mundo" a la palabra "Hola"

    string myString = "hello";
    myString += " world";

Lo que sucede en la memoria en este caso es que se crea un nuevo objeto cuando se agrega a la `cadena` en la segunda línea. Si hace esto como parte de un bucle grande, existe la posibilidad de que cause problemas de rendimiento en su aplicación.

El equivalente mutable de una `cadena` es un `StringBuilder`

Tomando el siguiente código

    StringBuilder myStringBuilder = new StringBuilder("hello");
    myStringBuilder.append(" world");

Cuando ejecuta esto, está modificando el objeto `StringBuilder` en la memoria.

