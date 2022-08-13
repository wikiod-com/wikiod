---
title: "literales"
slug: "literales"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Sintaxis
- **bool:** verdadero o falso
- **byte:** Ninguno, literal entero convertido implícitamente de int
- **sbyte:** Ninguno, literal entero convertido implícitamente de int
- **char:** Envuelve el valor con comillas simples
- **decimal:** M o m
- **doble:** D, d, o un número real
- **flotante:** F o f
- **int:** Ninguno, predeterminado para valores integrales dentro del rango de int
- **uint:** U, u o valores integrales dentro del rango de uint
- **largo:** L, l o valores integrales dentro del rango de largo
- **Ulong:** UL, UL, Ul, uL, LU, lu, Lu, lU, o valores integrales dentro del rango de ulong
- **corto:** Ninguno, literal entero convertido implícitamente de int
- **ushort:** Ninguno, literal entero convertido implícitamente de int
- **cadena:** Envuelve el valor con comillas dobles, opcionalmente antepuesto con `@`
- **null**: El literal `null`

## uint literales
Los literales `uint` se definen usando el sufijo `U` o `u`, o usando valores integrales dentro del rango de `uint`:

    uint ui = 5U;

## literales int
Los literales `int` se definen simplemente usando valores integrales dentro del rango de `int`:

    int i = 5;

## literales de sbytes
El tipo `sbyte` no tiene sufijo literal. Los literales enteros se convierten implícitamente de `int`:

    sbyte sb = 127;

## literales decimales
Los literales `decimales` se definen usando el sufijo M o m en un número real:

    decimal m = 30.5M;

## literales dobles
Los literales `dobles` se definen usando el sufijo D o d, o usando un número real:

    double d = 30.5D;


## literales flotantes
Los literales `flotantes` se definen usando el sufijo F o f, o usando un número real:

    float f = 30.5F;


## literales largos
Los literales `largos` se definen usando el sufijo `L` o `l`, o usando valores integrales dentro del rango de `largo`:

    long l = 5L;

## encabezado literal
Los literales `ulong` se definen usando los sufijos `UL`, `ul`, `Ul`, `uL`, `LU`, `lu`, `Lu` o `lU`, o usando valores integrales ​dentro del rango de `cabeza`:

    ulong ul = 5UL;

## literales de cadena
Los literales `string` se definen envolviendo el valor con comillas dobles `"`:

    string s = "hello, this is a string literal";

Los literales de cadena pueden contener secuencias de escape. Consulte [Secuencias de escape de cadenas][1]

Además, C# admite literales de cadenas textuales (consulte [Cadenas textuales][2]). Estos se definen envolviendo el valor con comillas dobles `"` y anteponiéndolo con `@`. Las secuencias de escape se ignoran en los literales de cadena textuales y se incluyen todos los caracteres de espacio en blanco:

    string s = @"The path is:
    C:\Windows\System32";
    //The backslashes and newline are included in the string


[1]: https://www.wikiod.com/es/docs/c%23/39/string-escape-sequences
[2]: https://www.wikiod.com/es/docs/c%23/16/verbatim-strings

## literales de caracteres
Los literales `char` se definen envolviendo el valor con comillas simples `'`:

    char c = 'h';

Los caracteres literales pueden contener secuencias de escape. Consulte [Secuencias de escape de cadenas][1]

Un carácter literal debe tener exactamente un carácter de largo (después de que se hayan evaluado todas las secuencias de escape). Los literales de caracteres vacíos no son válidos. El carácter predeterminado (devuelto por `default(char)` o `new char()`) es `'\0'`, o el carácter NULL (que no debe confundirse con el literal `null` y las referencias nulas).

[1]: https://www.wikiod.com/es/docs/c%23/39/string-escape-sequences

## literales de bytes
El tipo `byte` no tiene sufijo literal. Los literales enteros se convierten implícitamente de `int`:

    byte b = 127;

## literal corto
El tipo `short` no tiene literal. Los literales enteros se convierten implícitamente de `int`:

    short s = 127;

## uliteral corto
El tipo `ushort` no tiene sufijo literal. Los literales enteros se convierten implícitamente de `int`:

    ushort us = 127;

## literales booleanos
Los literales `bool` son `verdadero` o `falso`;

    bool b = true;

