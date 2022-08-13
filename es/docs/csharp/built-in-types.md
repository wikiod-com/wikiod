---
title: "Tipos incorporados"
slug: "tipos-incorporados"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Conversión de tipos de valores en caja
[En caja](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx) los tipos de valor solo se pueden desempaquetar en su 'Tipo' original, incluso si una conversión de los dos 'Tipo' es válida , p.ej.:

    object boxedInt = (int)1; // int boxed in an object

    long unboxedInt1 = (long)boxedInt; // invalid cast

Esto se puede evitar desempaquetando primero en el 'Tipo' original, por ejemplo:

    long unboxedInt2 = (long)(int)boxedInt; // valid

## Tipo de referencia inmutable - cadena
    // assign string from a string literal
    string s = "hello";

    // assign string from an array of characters
    char[] chars = new char[] { 'h', 'e', 'l', 'l', 'o' };
    string s = new string(chars, 0, chars.Length);

    // assign string from a char pointer, derived from a string
    string s;
    unsafe
    {
        fixed (char* charPointer = "hello")
        {
            s = new string(charPointer);
        }
    }


## Tipo de valor - char
    // single character s
    char c = 's';

    // character s: casted from integer value
    char c = (char)115;

    // unicode character: single character s
    char c = '\u0073';

    // unicode character: smiley face
    char c = '\u263a';

## Tipo de valor: corto, int, largo (enteros de 16 bits, 32 bits, 64 bits con signo)
    // assigning a signed short to its minimum value
    short s = -32768;
    
    // assigning a signed short to its maximum value
    short s = 32767;
    
    // assigning a signed int to its minimum value
    int i = -2147483648;
    
    // assigning a signed int to its maximum value
    int i = 2147483647;
    
    // assigning a signed long to its minimum value (note the long postfix)
    long l = -9223372036854775808L;
    
    // assigning a signed long to its maximum value (note the long postfix)
    long l = 9223372036854775807L;

También es posible hacer que estos tipos admitan valores nulos, lo que significa que, además de los valores habituales, también se pueden asignar valores nulos. Si una variable de tipo anulable no se inicializa, será nula en lugar de 0. Los tipos anulables se marcan agregando un signo de interrogación (?) después del tipo.

    int a; //This is now 0.
    int? b; //This is now null.

## Tipo de valor: ushort, uint, ulong (enteros sin signo de 16 bits, 32 bits, 64 bits)
    // assigning an unsigned short to its minimum value
    ushort s = 0;
    
    // assigning an unsigned short to its maximum value
    ushort s = 65535;
    
    // assigning an unsigned int to its minimum value
    uint i = 0;
    
    // assigning an unsigned int to its maximum value
    uint i = 4294967295;
    
    // assigning an unsigned long to its minimum value (note the unsigned long postfix)
    ulong l = 0UL;
    
    // assigning an unsigned long to its maximum value (note the unsigned long postfix)
    ulong l = 18446744073709551615UL;

También es posible hacer que estos tipos admitan valores nulos, lo que significa que, además de los valores habituales, también se pueden asignar valores nulos. Si una variable de tipo anulable no se inicializa, será nula en lugar de 0. Los tipos anulables se marcan agregando un signo de interrogación (?) después del tipo.

    uint a; //This is now 0.
    uint? b; //This is now null.

## Tipo de valor - bool
    // default value of boolean is false
    bool b;
    //default value of nullable boolean is null
    bool? z;
    b = true;
    if(b) {
        Console.WriteLine("Boolean has true value");
    }

La palabra clave bool es un alias de System.Boolean. Se utiliza para declarar variables para almacenar los valores booleanos, `verdadero` y `falso`.

## Comparaciones con tipos de valores en caja
Si los tipos de valor se asignan a variables de tipo "objeto", se [*encuadran*](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx): el valor se almacena en una instancia de un `Sistema.Objeto`. Esto puede tener consecuencias no deseadas al comparar valores con `==`, por ejemplo:

    object left = (int)1;  // int in an object box
    object right = (int)1; // int in an object box

    var comparison1 = left == right;      // false

Esto se puede evitar utilizando el método `Equals` sobrecargado, que dará el resultado esperado.

    var comparison2 = left.Equals(right); // true

Alternativamente, se podría hacer lo mismo desempaquetando las variables `izquierda` y `derecha` para que se comparen los valores `int`:

    var comparison3 = (int)left == (int)right; // true

