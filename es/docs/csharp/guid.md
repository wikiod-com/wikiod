---
title: "guía"
slug: "guia"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

GUID (o UUID) es un acrónimo de 'Globally Unique Identifier' (o 'Universally Unique Identifier'). Es un número entero de 128 bits que se utiliza para identificar recursos.

`Guid`s son *Identificadores únicos globales*, también conocidos como *UUID*, *Identificadores únicos universales*.

Son valores pseudoaleatorios de 128 bits. Hay tantos `Guid`s válidos (alrededor de 10^18 `Guid`s para cada célula de cada persona en la Tierra) que si son generados por un buen algoritmo pseudoaleatorio, pueden ser considerados únicos en todo el universo por todas las prácticas. medio.

Los 'Guid' se usan con mayor frecuencia como claves principales en las bases de datos. Su ventaja es que no tiene que llamar a la base de datos para obtener una nueva identificación que (casi) garantiza que es única.

## Obtener la representación de cadena de un Guid
Se puede obtener una representación de cadena de un Guid utilizando el método integrado `ToString`

    string myGuidString = myGuid.ToString();

Según sus necesidades, también puede formatear el Guid agregando un argumento de tipo de formato a la llamada `ToString`.

    var guid = new Guid("7febf16f-651b-43b0-a5e3-0da8da49e90d");

    // None          "7febf16f651b43b0a5e30da8da49e90d"
    Console.WriteLine(guid.ToString("N"));

    // Hyphens       "7febf16f-651b-43b0-a5e3-0da8da49e90d"
    Console.WriteLine(guid.ToString("D"));

    // Braces        "{7febf16f-651b-43b0-a5e3-0da8da49e90d}"
    Console.WriteLine(guid.ToString("B"));

    // Parentheses   "(7febf16f-651b-43b0-a5e3-0da8da49e90d)"
    Console.WriteLine(guid.ToString("P"));

    // Hex           "{0x7febf16f,0x651b,0x43b0{0xa5,0xe3,0x0d,0xa8,0xda,0x49,0xe9,0x0d}}"
    Console.WriteLine(guid.ToString("X"));


## Creando un Guid
Estas son las formas más comunes de crear una instancia de Guid:

- Crear un GUID vacío (`00000000-0000-0000-0000-000000000000`):


    Guid g = Guid.Empty;
    Guid g2 = new Guid();

- Creando un nuevo Guid (pseudoaleatorio):


    Guid g = Guid.NewGuid();

- Creación de Guids con un valor específico:


    Guid g = new Guid("0b214de7-8958-4956-8eed-28f9ba2c47c6");
    Guid g2 = new Guid("0b214de7895849568eed28f9ba2c47c6");
    Guid g3 = Guid.Parse("0b214de7-8958-4956-8eed-28f9ba2c47c6");


## Declarar un GUID anulable
Al igual que otros tipos de valores, GUID también tiene un tipo que acepta valores NULL que puede tomar valores NULL.

Declaración :

    Guid? myGuidVar = null;

Esto es particularmente útil cuando se recuperan datos de la base de datos cuando existe la posibilidad de que el valor de una tabla sea NULL.

