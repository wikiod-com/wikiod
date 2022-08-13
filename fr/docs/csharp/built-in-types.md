---
title: "Types intégrés"
slug: "types-integres"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Conversion des types de valeurs encadrées
[Boxed](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx) les types de valeur ne peuvent être déballés que dans leur `Type` d'origine, même si une conversion des deux `Type`s est valide , par exemple.:

    object boxedInt = (int)1; // int boxed in an object

    long unboxedInt1 = (long)boxedInt; // invalid cast

Cela peut être évité en déballant d'abord dans le "Type" d'origine, par exemple :

    long unboxedInt2 = (long)(int)boxedInt; // valid

## Type de référence immuable - chaîne
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


## Type de valeur - caractère
    // single character s
    char c = 's';

    // character s: casted from integer value
    char c = (char)115;

    // unicode character: single character s
    char c = '\u0073';

    // unicode character: smiley face
    char c = '\u263a';

## Type de valeur - court, int, long (entiers signés 16 bits, 32 bits, 64 bits)
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

Il est également possible de rendre ces types nullables, ce qui signifie qu'en plus des valeurs habituelles, null peut également être affecté. Si une variable d'un type nullable n'est pas initialisée, elle sera null au lieu de 0. Les types nullables sont marqués en ajoutant un point d'interrogation (?) après le type.

    int a; //This is now 0.
    int? b; //This is now null.

## Type de valeur - ushort, uint, ulong (entiers non signés 16 bits, 32 bits, 64 bits)
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

Il est également possible de rendre ces types nullables, ce qui signifie qu'en plus des valeurs habituelles, null peut également être affecté. Si une variable d'un type nullable n'est pas initialisée, elle sera null au lieu de 0. Les types nullables sont marqués en ajoutant un point d'interrogation (?) après le type.

    uint a; //This is now 0.
    uint? b; //This is now null.

## Type de valeur - booléen
    // default value of boolean is false
    bool b;
    //default value of nullable boolean is null
    bool? z;
    b = true;
    if(b) {
        Console.WriteLine("Boolean has true value");
    }

Le mot clé bool est un alias de System.Boolean. Il est utilisé pour déclarer des variables pour stocker les valeurs booléennes, 'true' et 'false'.

## Comparaisons avec les types de valeurs en boîte
Si des types de valeur sont affectés à des variables de type "object", ils sont [*boxed*](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx) - la valeur est stockée dans une instance d'un `Système.Objet`. Cela peut entraîner des conséquences inattendues lors de la comparaison de valeurs avec `==`, par exemple :

    object left = (int)1;  // int in an object box
    object right = (int)1; // int in an object box

    var comparison1 = left == right;      // false

Cela peut être évité en utilisant la méthode "Equals" surchargée, qui donnera le résultat attendu.

    var comparison2 = left.Equals(right); // true

Alternativement, la même chose pourrait être faite en déballant les variables `left` et `right` afin que les valeurs `int` soient comparées :

    var comparison3 = (int)left == (int)right; // true

