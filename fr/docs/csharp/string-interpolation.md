---
title: "Interpolation de chaîne"
slug: "interpolation-de-chaine"
draft: false
images: []
weight: 9769
type: docs
toc: true
---

## Syntaxe
- $"contenu {expression} contenu"
- $"contenu {expression:format} contenu"
- $"content {expression} {{content in accolades}} content}"
- $"content {expression:format} {{content in accolades}} content}"

L'interpolation de chaîne est un raccourci pour la méthode `string.Format()` qui facilite la création de chaînes contenant des valeurs de variables et d'expressions.

    var name = "World";
    var oldWay = string.Format("Hello, {0}!", name);  // returns "Hello, World"
    var newWay = $"Hello, {name}!";                   // returns "Hello, World"

## Formater les dates en chaînes
    var date = new DateTime(2015, 11, 11);
    var str = $"It's {date:MMMM d, yyyy}, make a wish!";
    System.Console.WriteLine(str);

Vous pouvez également utiliser la méthode [`DateTime.ToString`][1] pour formater l'objet `DateTime`. Cela produira la même sortie que le code ci-dessus.

    var date = new DateTime(2015, 11, 11);
    var str = date.ToString("MMMM d, yyyy");
    str = "It's " + str + ", make a wish!";
    Console.WriteLine(str);

**Production:**
>Nous sommes le 11 novembre 2015, faites un vœu !

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/DpRwV5)

[Démo en direct avec DateTime.ToString](https://dotnetfiddle.net/YnV9J0)

> **Remarque :** "MM" correspond aux mois et "mm" aux minutes. Soyez très prudent lorsque vous les utilisez, car des erreurs peuvent introduire des bogues difficiles à découvrir.


[1] : https://msdn.microsoft.com/en-us/library/zdtaw1bw(v=vs.110).aspx

## Remplissage de la sortie
La chaîne peut être formatée pour accepter un paramètre de remplissage qui spécifiera le nombre de positions de caractères que la chaîne insérée utilisera :

    ${value, padding}

> **REMARQUE :** Les valeurs de remplissage positives indiquent un remplissage à gauche et négatives
> les valeurs de rembourrage indiquent un rembourrage droit.

** Rembourrage gauche **
----

Un rembourrage à gauche de 5 (ajoute 3 espaces avant la valeur de nombre, il occupe donc un total de 5 positions de caractère dans la chaîne résultante.)

    var number = 42;
    var str = $"The answer to life, the universe and everything is {number, 5}.";
    //str is "The answer to life, the universe and everything is    42.";
    //                                                           ^^^^^
    System.Console.WriteLine(str);
    
**Production:**
       
    The answer to life, the universe and everything is    42.
[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/PpZXmk)

** Rembourrage droit **
----

Le remplissage droit, qui utilise une valeur de remplissage négative, ajoutera des espaces à la fin de la valeur actuelle.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, -5}.";
    //str is "The answer to life, the universe and everything is 42   .";
    //                                                           ^^^^^
    System.Console.WriteLine(str);

**Production:**

    The answer to life, the universe and everything is 42   .

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/QtKjGF)

**Remplissage avec les spécificateurs de format**
----

Vous pouvez également utiliser des spécificateurs de mise en forme existants en conjonction avec le rembourrage.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, 5:f1}";
    //str is "The answer to life, the universe and everything is 42.1 ";
    //                                                           ^^^^^

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/34ZxP0)



## Expressions
Les expressions complètes peuvent également être utilisées dans les chaînes interpolées.

    var StrWithMathExpression = $"1 + 2 = {1 + 2}"; // -> "1 + 2 = 3"
    
    string world = "world";
    var StrWithFunctionCall = $"Hello, {world.ToUpper()}!"; // -> "Hello, WORLD!"


[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/u9lzeg)



## Utilisation simple
    var name = "World";
    var str = $"Hello, {name}!";
    //str now contains: "Hello, World!";

## Dans les coulisses

En interne cela

    $"Hello, {name}!" 

Sera compilé à quelque chose comme ceci:

    string.Format("Hello, {0}!", name);

    


## Formatage des nombres dans les chaînes
Vous pouvez utiliser deux-points et la [syntaxe de format numérique standard](https://msdn.microsoft.com/en-us/library/dwhawy9k.aspx) pour contrôler la mise en forme des nombres.

    var decimalValue = 120.5;

    var asCurrency = $"It costs {decimalValue:C}";
    // String value is "It costs $120.50" (depending on your local currency settings)

    var withThreeDecimalPlaces = $"Exactly {decimalValue:F3}";
    // String value is "Exactly 120.500"

    var integerValue = 57;

    var prefixedIfNecessary = $"{integerValue:D5}";
    // String value is "00057"


[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/z2XbG7)

