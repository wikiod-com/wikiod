---
title: "Littéraux"
slug: "litteraux"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Syntaxe
- **bool :** vrai ou faux
- **byte :** Aucun, littéral entier implicitement converti à partir de int
- **sbyte :** Aucun, littéral entier implicitement converti à partir de int
- **char:** Enveloppez la valeur avec des guillemets simples
- **décimal :** M ou m
- **double :** D, d ou un nombre réel
- **flottant :** F ou f
- **int :** Aucun, par défaut pour les valeurs intégrales dans la plage de int
- **uint :** U, u ou valeurs intégrales dans la plage de uint
- **long :** L, l ou valeurs intégrales dans la plage de long
- **Ulong :** UL, UL, Ul, uL, LU, lu, Lu, lU, ou valeurs intégrales dans la plage de ulong
- **short :** Aucun, littéral entier implicitement converti à partir de int
- **ushort :** Aucun, littéral entier implicitement converti à partir de int
- **string :** Enveloppez la valeur avec des guillemets doubles, éventuellement précédés de `@`
- **null** : le littéral "null"

## littéraux uint
Les littéraux `uint` sont définis en utilisant le suffixe `U` ou `u`, ou en utilisant une valeur intégrale dans la plage de `uint` :

    uint ui = 5U;

## int littéraux
Les littéraux `int` sont définis en utilisant simplement des valeurs intégrales dans la plage de `int` :

    int i = 5;

## littéraux sbytes
Le type `sbyte` n'a pas de suffixe littéral. Les littéraux entiers sont implicitement convertis à partir de `int` :

    sbyte sb = 127;

## littéraux décimaux
Les littéraux "décimaux" sont définis en utilisant le suffixe M ou m sur un nombre réel :

    decimal m = 30.5M;

## littéraux doubles
Les littéraux "doubles" sont définis en utilisant le suffixe D ou d, ou en utilisant un nombre réel :

    double d = 30.5D;


## littéraux flottants
Les littéraux `float` sont définis en utilisant le suffixe F ou f, ou en utilisant un nombre réel :

    float f = 30.5F;


## longs littéraux
Les littéraux `long` sont définis en utilisant le suffixe `L` ou `l`, ou en utilisant une valeur intégrale dans la plage de `long` :

    long l = 5L;

## littéral de tête
Les littéraux `ulong` sont définis en utilisant les suffixes `UL`, `ul`, `Ul`, `uL`, `LU`, `lu`, `Lu` ou `lU`, ou en utilisant des valeurs intégrales ​​dans la plage de `head` :

    ulong ul = 5UL;

## littéraux de chaîne
Les littéraux `string` sont définis en enveloppant la valeur avec des guillemets doubles `"` :

    string s = "hello, this is a string literal";

Les littéraux de chaîne peuvent contenir des séquences d'échappement. Voir [Séquences d'échappement de chaîne][1]

De plus, C # prend en charge les littéraux de chaîne textuels (voir [Cordes textuelles] [2]). Celles-ci sont définies en enveloppant la valeur de guillemets doubles `"` et en la faisant précéder de `@`. Les séquences d'échappement sont ignorées dans les littéraux de chaîne textuels et tous les caractères d'espacement sont inclus :

    string s = @"The path is:
    C:\Windows\System32";
    //The backslashes and newline are included in the string


[1] : https://www.wikiod.com/fr/docs/c%23/39/string-escape-sequences
[2] : https://www.wikiod.com/fr/docs/c%23/16/verbatim-strings

## caractères littéraux
Les littéraux `char` sont définis en enveloppant la valeur avec des guillemets simples `'` :

    char c = 'h';

Les littéraux de caractères peuvent contenir des séquences d'échappement. Voir [Séquences d'échappement de chaîne][1]

Un littéral de caractère doit avoir exactement un caractère de long (après que toutes les séquences d'échappement ont été évaluées). Les littéraux de caractères vides ne sont pas valides. Le caractère par défaut (renvoyé par `default(char)` ou `new char()`) est `'\0'`, ou le caractère NULL (à ne pas confondre avec les références littérales et nulles `null`).

[1] : https://www.wikiod.com/fr/docs/c%23/39/string-escape-sequences

## octets littéraux
Le type `byte` n'a pas de suffixe littéral. Les littéraux entiers sont implicitement convertis à partir de `int` :

    byte b = 127;

## court littéral
Le type `short` n'a pas de littéral. Les littéraux entiers sont implicitement convertis à partir de `int` :

    short s = 127;

## ushort littéral
Le type `ushort` n'a pas de suffixe littéral. Les littéraux entiers sont implicitement convertis à partir de `int` :

    ushort us = 127;

## bool littéraux
Les littéraux `bool` sont soit `true` ou `false` ;

    bool b = true;

