---
title: "Literais"
slug: "literais"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Sintaxe
- **bool:** verdadeiro ou falso
- **byte:** Nenhum, inteiro literal convertido implicitamente de int
- **sbyte:** Nenhum, literal inteiro convertido implicitamente de int
- **char:** Coloque o valor entre aspas simples
- **decimal:** M ou m
- **double:** D, d ou um número real
- **float:** F ou f
- **int:** Nenhum, padrão para valores integrais dentro do intervalo de int
- **uint:** U, u ou valores integrais dentro do intervalo de uint
- **longo:** L, l ou valores integrais dentro do intervalo de longo
- **Ulong:** UL, UL, Ul, uL, LU, lu, Lu, lU ou valores integrais dentro do intervalo de ulong
- **curto:** Nenhum, literal inteiro convertido implicitamente de int
- **abreviação:** Nenhum, literal inteiro convertido implicitamente de int
- **string:** Coloque o valor entre aspas duplas, opcionalmente prefixado com `@`
- **null**: O literal `null`

## uint literais
Literais `uint` são definidos usando o sufixo `U` ou `u`, ou usando valores integrais dentro do intervalo de `uint`:

    uint ui = 5U;

## literais int
Os literais `int` são definidos simplesmente usando valores integrais dentro do intervalo de `int`:

    int i = 5;

## literais de sbyte
O tipo `sbyte` não tem sufixo literal. Literais inteiros são convertidos implicitamente de `int`:

    sbyte sb = 127;

## literais decimais
Literais `decimais` são definidos usando o sufixo M ou m em um número real:

    decimal m = 30.5M;

## literais duplos
Literais `double` são definidos usando o sufixo D ou d, ou usando um número real:

    double d = 30.5D;


## literais flutuantes
Literais `float` são definidos usando o sufixo F ou f, ou usando um número real:

    float f = 30.5F;


## literais longos
Literais `long` são definidos usando o sufixo `L` ou `l`, ou usando valores integrais dentro do intervalo de `long`:

    long l = 5L;

## literal de cabeça
Literais `ulong` são definidos usando os sufixos `UL`, `ul`, `Ul`, `uL`, `LU`, `lu`, `Lu` ou `lU`, ou usando valores integrais dentro do intervalo de `head`:

    ulong ul = 5UL;

## literais de string
Os literais `string` são definidos envolvendo o valor com aspas duplas `"`:

    string s = "hello, this is a string literal";

Literais de string podem conter sequências de escape. Consulte [Sequências de Escape de String][1]

Além disso, o C# oferece suporte a literais de string literais (consulte [Strings literais][2]). Eles são definidos envolvendo o valor entre aspas duplas `"` e prefixando-o com `@`. Sequências de escape são ignoradas em literais de string literais e todos os caracteres de espaço em branco são incluídos:

    string s = @"The path is:
    C:\Windows\System32";
    //The backslashes and newline are included in the string


[1]: https://www.wikiod.com/pt/docs/c%23/39/string-escape-sequences
[2]: https://www.wikiod.com/pt/docs/c%23/16/verbatim-strings

## literais de caracteres
Literais `char` são definidos envolvendo o valor com aspas simples `'`:

    char c = 'h';

Os literais de caracteres podem conter sequências de escape. Consulte [Sequências de Escape de String][1]

Um literal de caractere deve ter exatamente um caractere (após todas as sequências de escape terem sido avaliadas). Literais de caracteres vazios não são válidos. O caractere padrão (retornado por `default(char)` ou `new char()`) é `'\0'`, ou o caractere NULL (não deve ser confundido com o literal `null` e referências nulas).

[1]: https://www.wikiod.com/pt/docs/c%23/39/string-escape-sequences

## literais de bytes
O tipo `byte` não tem sufixo literal. Literais inteiros são convertidos implicitamente de `int`:

    byte b = 127;

## literal curto
O tipo `short` não tem literal. Literais inteiros são convertidos implicitamente de `int`:

    short s = 127;

## literal de ushor
O tipo `ushort` não tem sufixo literal. Literais inteiros são convertidos implicitamente de `int`:

    ushort us = 127;

## literais bool
Literais `bool` são `true` ou `false`;

    bool b = true;

