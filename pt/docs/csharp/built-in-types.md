---
title: "Tipos integrados"
slug: "tipos-integrados"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Conversão de tipos de valor em caixa
Os tipos de valor [Boxed](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx) só podem ser desembalados em seu `Type` original, mesmo se uma conversão dos dois `Type`s for válida , por exemplo.:

    object boxedInt = (int)1; // int boxed in an object

    long unboxedInt1 = (long)boxedInt; // invalid cast

Isso pode ser evitado primeiro unboxing no 'Type' original, por exemplo:

    long unboxedInt2 = (long)(int)boxedInt; // valid

## Tipo de referência imutável - string
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

## Tipo de valor - short, int, long (inteiros de 16 bits, 32 bits, 64 bits assinados)
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

Também é possível tornar esses tipos anuláveis, o que significa que, além dos valores usuais, null também pode ser atribuído. Se uma variável de um tipo anulável não for inicializada, ela será nula em vez de 0. Os tipos anuláveis ​​são marcados pela adição de um ponto de interrogação (?) após o tipo.

    int a; //This is now 0.
    int? b; //This is now null.

## Tipo de valor - ushort, uint, ulong (inteiros não assinados de 16 bits, 32 bits, 64 bits)
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

Também é possível tornar esses tipos anuláveis, o que significa que, além dos valores usuais, null também pode ser atribuído. Se uma variável de um tipo anulável não for inicializada, ela será nula em vez de 0. Os tipos anuláveis ​​são marcados pela adição de um ponto de interrogação (?) após o tipo.

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

A palavra-chave bool é um alias de System.Boolean. Ele é usado para declarar variáveis ​​para armazenar os valores booleanos, `true` e `false`.

## Comparações com tipos de valor em caixa
Se os tipos de valor são atribuídos a variáveis ​​do tipo `object`, eles são [*boxed*](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx) - o valor é armazenado em uma instância de um `System.Object`. Isso pode levar a consequências não intencionais ao comparar valores com `==`, por exemplo:

    object left = (int)1;  // int in an object box
    object right = (int)1; // int in an object box

    var comparison1 = left == right;      // false

Isso pode ser evitado usando o método sobrecarregado `Equals`, que dará o resultado esperado.

    var comparison2 = left.Equals(right); // true

Alternativamente, o mesmo pode ser feito desembalando as variáveis ​​`left` e `right` para que os valores `int` sejam comparados:

    var comparison3 = (int)left == (int)right; // true

