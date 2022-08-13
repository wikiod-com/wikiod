---
title: "Opérations courantes sur les chaînes"
slug: "operations-courantes-sur-les-chaines"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Formater une chaîne
Utilisez la méthode `String.Format()` pour remplacer un ou plusieurs éléments de la chaîne par la représentation sous forme de chaîne d'un objet spécifié :

    String.Format("Hello {0} Foo {1}", "World", "Bar") //Hello World Foo Bar

## Remplissage d'une chaîne à une longueur fixe
    string s = "Foo";
    string paddedLeft = s.PadLeft(5);        // paddedLeft = "  Foo" (pads with spaces by default)
    string paddedRight = s.PadRight(6, '+'); // paddedRight = "Foo+++"
    string noPadded = s.PadLeft(2);          // noPadded = "Foo" (original string is never shortened)


## Inverser correctement une chaîne
La plupart du temps, lorsque les gens doivent inverser une chaîne, ils le font plus ou moins comme ceci :

    char[] a = s.ToCharArray();
    System.Array.Reverse(a);
    string r = new string(a);

Cependant, ce que ces gens ne réalisent pas, c'est que c'est en fait faux. <br />
Et je ne veux pas dire à cause de la vérification NULL manquante.

C'est en fait faux car un Glyph/GraphemeCluster peut être composé de plusieurs points de code (c'est-à-dire des caractères).

Pour comprendre pourquoi il en est ainsi, nous devons d'abord être conscients du fait que le terme "caractère" signifie réellement.

[Référence :][1]
> Le caractère est un terme surchargé qui peut signifier beaucoup de choses.
> 
> Un point de code est l'unité atomique d'information. Le texte est une suite de
> points de code. Chaque point de code est un nombre auquel la signification est donnée par le
> Norme Unicode.
> 
> Un graphème est une séquence d'un ou plusieurs points de code qui s'affichent
> comme une seule unité graphique qu'un lecteur reconnaît comme une seule
> élément du système d'écriture. Par exemple, a et ä sont
> graphèmes, mais ils peuvent être constitués de plusieurs points de code (par exemple, ä peut être
> deux points de code, un pour le caractère de base a suivi d'un pour le
> diarésie ; mais il y a aussi un point de code alternatif, hérité, unique
> représentant ce graphème). Certains points de code ne font jamais partie d'aucun
> graphème (par exemple, le non-joindre de largeur nulle ou les remplacements directionnels).
> 
> Un glyphe est une image, généralement stockée dans une police (qui est une collection
> de glyphes), utilisé pour représenter des graphèmes ou des parties de ceux-ci. Les polices peuvent
> composer plusieurs glyphes en une seule représentation, par exemple, si
> le ä ci-dessus est un point de code unique, une police peut choisir de le rendre comme
> deux glyphes distincts superposés dans l'espace. Pour OTF, le GSUB de la police et
> Les tables GPOS contiennent des informations de substitution et de positionnement
> ce travail. Une police peut contenir plusieurs glyphes alternatifs pour le même
> graphème aussi.

Ainsi, en C #, un caractère est en fait un CodePoint.

Ce qui signifie que si vous inversez simplement une chaîne valide comme "Les Misérables", qui peut ressembler à ceci

    string s = "Les Mise\u0301rables";

sous forme de suite de caractères, vous obtiendrez :

> selbaŕesiM seL

Comme vous pouvez le voir, l'accent est mis sur le caractère R, au lieu du caractère e. <br />
Bien que string.reverse.reverse produise la chaîne d'origine si vous inversez les deux fois le tableau de caractères, ce type d'inversion n'est certainement PAS l'inverse de la chaîne d'origine.


Vous aurez besoin d'inverser chaque GraphemeCluster uniquement. <br />
Donc, si c'est fait correctement, vous inversez une chaîne comme celle-ci :


        private static System.Collections.Generic.List<string> GraphemeClusters(string s)
        {
            System.Collections.Generic.List<string> ls = new System.Collections.Generic.List<string>();
    
            System.Globalization.TextElementEnumerator enumerator = System.Globalization.StringInfo.GetTextElementEnumerator(s);
            while (enumerator.MoveNext())
            {
                ls.Add((string)enumerator.Current);
            }
    
            return ls;
        }
    
    
        // this 
        private static string ReverseGraphemeClusters(string s)
        {
            if(string.IsNullOrEmpty(s) || s.Length == 1)
                 return s;
            
            System.Collections.Generic.List<string> ls = GraphemeClusters(s);
            ls.Reverse();
    
            return string.Join("", ls.ToArray());
        }
    
        public static void TestMe()
        {
            string s = "Les Mise\u0301rables";
            // s = "noël";
            string r = ReverseGraphemeClusters(s);
    
            // This would be wrong:
            // char[] a = s.ToCharArray();
            // System.Array.Reverse(a);
            // string r = new string(a);
    
            System.Console.WriteLine(r);
        }

Et - oh joie - vous vous rendrez compte si vous le faites correctement comme ça, cela fonctionnera aussi pour les langues asiatiques/sud-asiatiques/est-asiatiques (et français/suédois/norvégien, etc.)...


[1] : https://stackoverflow.com/questions/27331819/whats-the-difference-between-a-character-a-code-point-a-glyph-and-a-grapheme

## Obtenir x caractères du côté droit d'une chaîne
Visual Basic a des fonctions Left, Right et Mid qui renvoient des caractères à partir de Left, Right et Middle d'une chaîne. Ces méthodes n'existent pas en C#, mais peuvent être implémentées avec `Substring()`. Ils peuvent être implémentés en tant que méthodes d'extension comme suit :


       public static class StringExtensions
       {
          /// <summary>
          /// VB Left function
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="numchars"></param>
          /// <returns>Left-most numchars characters</returns>
          public static string Left( this string stringparam, int numchars )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (numchars > stringparam.Length)
                numchars = stringparam.Length;
        
             return stringparam.Substring( 0, numchars );
          }
        
          /// <summary>
          /// VB Right function
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="numchars"></param>
          /// <returns>Right-most numchars characters</returns>
          public static string Right( this string stringparam, int numchars )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (numchars > stringparam.Length)
                numchars = stringparam.Length;
        
             return stringparam.Substring( stringparam.Length - numchars );
          }
        
          /// <summary>
          /// VB Mid function - to end of string
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
          /// <returns>Balance of string beginning at startindex character</returns>
          public static string Mid( this string stringparam, int startindex )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative startindex being passed
             startindex = Math.Abs( startindex );
        
             // Validate numchars parameter        
             if (startindex > stringparam.Length)
                startindex = stringparam.Length;
             
             // C# strings are zero-based, convert passed startindex
             return stringparam.Substring( startindex - 1 );
          }
        
          /// <summary>
          /// VB Mid function - for number of characters
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
          /// <param name="numchars">number of characters to return</param>
          /// <returns>Balance of string beginning at startindex character</returns>
          public static string Mid( this string stringparam, int startindex, int numchars)
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative startindex being passed
             startindex = Math.Abs( startindex );
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (startindex > stringparam.Length)
                startindex = stringparam.Length;
        
             // C# strings are zero-based, convert passed startindex
             return stringparam.Substring( startindex - 1, numchars );
    
           }
        }
Cette méthode d'extension peut être utilisée comme suit :

    string myLongString = "Hello World!";
    string myShortString = myLongString.Right(6);  // "World!"
    string myLeftString = myLongString.Left(5);    // "Hello"
    string myMidString1 = myLongString.Left(4);    // "lo World"
    string myMidString2 = myLongString.Left(2,3);    // "ell"








## Vérification de la chaîne vide à l'aide de String.IsNullOrEmpty() et String.IsNullOrWhiteSpace()
    string nullString = null;
    string emptyString = "";
    string whitespaceString = "    ";
    string tabString = "\t";
    string newlineString = "\n";
    string nonEmptyString = "abc123";
    
    bool result;

    result = String.IsNullOrEmpty(nullString);            // true
    result = String.IsNullOrEmpty(emptyString);           // true
    result = String.IsNullOrEmpty(whitespaceString);      // false
    result = String.IsNullOrEmpty(tabString);             // false
    result = String.IsNullOrEmpty(newlineString);         // false
    result = String.IsNullOrEmpty(nonEmptyString);        // false

    result = String.IsNullOrWhiteSpace(nullString);       // true
    result = String.IsNullOrWhiteSpace(emptyString);      // true
    result = String.IsNullOrWhiteSpace(tabString);        // true
    result = String.IsNullOrWhiteSpace(newlineString);    // true
    result = String.IsNullOrWhiteSpace(whitespaceString); // true
    result = String.IsNullOrWhiteSpace(nonEmptyString);   // false

## Couper les caractères indésirables au début et/ou à la fin des chaînes.
`Chaîne.Trim()`
--------

    string x = "   Hello World!    ";
    string y = x.Trim(); // "Hello World!"

    string q = "{(Hi!*";
    string r = q.Trim( '(', '*', '{' ); // "Hi!"


`String.TrimStart()` et `String.TrimEnd()`
--------------------------------------------

    string q = "{(Hi*";
    string r = q.TrimStart( '{' ); // "(Hi*"
    string s = q.TrimEnd( '*' );   // "{(Hi" 


## Construire une chaîne à partir de Array
La méthode `String.Join` nous aidera à construire une chaîne à partir d'un tableau/liste de caractères ou d'une chaîne. Cette méthode accepte deux paramètres. Le premier est le délimiteur ou le séparateur qui vous aidera à séparer chaque élément du tableau. Et le deuxième paramètre est le tableau lui-même.

**Chaîne de `char array` :**

    string delimiter=",";
    char[] charArray = new[] { 'a', 'b', 'c' };
    string inputString = String.Join(delimiter, charArray);
**Sortie** : `a,b,c` si nous changeons le `délimiteur` en `""` alors la sortie deviendra `abc`.

**Chaîne de `Liste de caractères` :**

    string delimiter = "|";
    List<char> charList = new List<char>() { 'a', 'b', 'c' };
    string inputString = String.Join(delimiter, charList);

**Sortie** : `a|b|c`

**Chaîne de `Liste des chaînes` :**

    string delimiter = " ";
    List<string> stringList = new List<string>() { "Ram", "is", "a","boy" };
    string inputString = String.Join(delimiter, stringList);

**Sortie** : `Ram est un garçon`

**Chaîne de `tableau de chaînes` :**

    string delimiter = "_";
    string[] stringArray = new [] { "Ram", "is", "a","boy" };
    string inputString = String.Join(delimiter, stringArray);

**Sortie** : `Ram_is_a_boy`


## Formatage à l'aide de ToString
Habituellement, nous utilisons la méthode `String.Format` à des fins de formatage, le `.ToString` est généralement utilisé pour convertir d'autres types en chaîne. Nous pouvons spécifier le format avec la méthode ToString pendant la conversion, afin d'éviter un formatage supplémentaire. Laissez-moi vous expliquer comment cela fonctionne avec différents types ;

**Entier vers chaîne formatée :**

    int intValue = 10;
    string zeroPaddedInteger = intValue.ToString("000"); // Output will be "010"
    string customFormat = intValue.ToString("Input value is 0"); // output will be   "Input value is 10" 
**double en chaîne formatée :**

    double doubleValue = 10.456;
    string roundedDouble = doubleValue.ToString("0.00"); // output 10.46
    string integerPart = doubleValue.ToString("00");    // output 10
    string customFormat = doubleValue.ToString("Input value is 0.0");  // Input value is 10.5

**Formatage DateTime à l'aide de ToString**

    DateTime currentDate = DateTime.Now; //  {7/21/2016 7:23:15 PM}
    string dateTimeString = currentDate.ToString("dd-MM-yyyy HH:mm:ss"); // "21-07-2016 19:23:15"
    string dateOnlyString = currentDate.ToString("dd-MM-yyyy"); // "21-07-2016"
    string dateWithMonthInWords = currentDate.ToString("dd-MMMM-yyyy HH:mm:ss"); // "21-July-2016 19:23:15"




## Convertir un nombre décimal en format binaire, octal et hexadécimal
1. Pour convertir un nombre décimal au format binaire, utilisez **base 2**

        Int32 Number = 15;
        Console.WriteLine(Convert.ToString(Number, 2));  //OUTPUT : 1111

2. Pour convertir un nombre décimal au format octal, utilisez **base 8**

        int Number = 15;
        Console.WriteLine(Convert.ToString(Number, 8));  //OUTPUT : 17

3. Pour convertir un nombre décimal au format hexadécimal, utilisez **base 16**

        var Number = 15;
        Console.WriteLine(Convert.ToString(Number, 16));  //OUTPUT : f



## Fractionner une chaîne par caractère spécifique
    string helloWorld = "hello world, how is it going?";
    string[] parts1 = helloWorld.Split(',');

    //parts1: ["hello world", " how is it going?"]

    string[] parts2 = helloWorld.Split(' ');

    //parts2: ["hello", "world,", "how", "is", "it", "going?"]


## Obtenir les sous-chaînes d'une chaîne donnée
    string helloWorld = "Hello World!";
    string world = helloWorld.Substring(6); //world = "World!"
    string hello = helloWorld.Substring(0,5); // hello = "Hello"

`Substring` renvoie la chaîne à partir d'un index donné, ou entre deux index (les deux inclus).

## Détermine si une chaîne commence par une séquence donnée
    string HelloWorld = "Hello World";
    HelloWorld.StartsWith("Hello"); // true
    HelloWorld.StartsWith("Foo"); // false


**Rechercher une chaîne dans une chaîne**

En utilisant le
[`System.String.Contains`][1] vous pouvez savoir si une chaîne particulière existe dans une chaîne. La méthode renvoie un booléen, vrai si la chaîne existe sinon faux.

    string s = "Hello World";
    bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 


[1] : https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx

## Rejoindre un tableau de chaînes dans un nouveau
    var parts = new[] { "Foo", "Bar", "Fizz", "Buzz"};
    var joined = string.Join(", ", parts);

    //joined = "Foo, Bar, Fizz, Buzz"

## Obtenir un caractère à un index spécifique et énumérer la chaîne
Vous pouvez utiliser la méthode `Substring` pour obtenir n'importe quel nombre de caractères d'une chaîne à n'importe quel emplacement donné. Cependant, si vous ne voulez qu'un seul caractère, vous pouvez utiliser l'indexeur de chaîne pour obtenir un seul caractère à n'importe quel index donné comme vous le faites avec un tableau :

    string s = "hello";
    char c = s[1]; //Returns 'e'

Notez que le type de retour est `char`, contrairement à la méthode `Substring` qui renvoie un type `string`.

Vous pouvez également utiliser l'indexeur pour parcourir les caractères de la chaîne :

    string s = "hello";
    foreach (char c in s)
        Console.WriteLine(c);
    /********* This will print each character on a new line:
    h
    e
    l
    l
    o
    **********/

## Fractionner une chaîne par une autre chaîne
    string str = "this--is--a--complete--sentence";
    string[] tokens = str.Split(new[] { "--" }, StringSplitOptions.None);

Résultat:

>[ "ceci", "est", "un", "complet", "phrase" ]

## Remplacement d'une chaîne dans une chaîne
À l'aide de la méthode [`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx), vous pouvez remplacer une partie d'une chaîne par une autre chaîne de caractères.

    string s = "Hello World";
     s = s.Replace("World", "Universe"); // s = "Hello Universe"
Toutes les occurrences de la chaîne de recherche sont remplacées.

Cette méthode peut également être utilisée pour supprimer une partie d'une chaîne, en utilisant [`String.Empty`](https://msdn.microsoft.com/en-us/library/system.string.empty(v=vs.110 ).aspx) :

    string s = "Hello World";
    s = s.Replace("ell", String.Empty); // s = "Ho World"


## Changer la casse des caractères dans une chaîne
La classe [`System.String`](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) prend en charge un certain nombre de méthodes pour convertir entre majuscules et minuscules caractères dans une chaîne.

- [`System.String.ToLowerInvariant`](https://msdn.microsoft.com/en-us/library/system.string.tolowerinvariant(v=vs.110).aspx) est utilisé pour renvoyer un objet String converti en minuscules.


- [`System.String.ToUpperInvariant`](https://msdn.microsoft.com/en-us/library/system.string.toupperinvariant(v=vs.110).aspx) est utilisé pour renvoyer un objet String converti en majuscule.

**Remarque :** La raison d'utiliser les versions *invariantes* de ces méthodes est d'empêcher la production de lettres inattendues spécifiques à la culture. Ceci est expliqué [ici en détail](http://stackoverflow.com/a/19778131/1379664).

Exemple:

    string s = "My String";
    s = s.ToLowerInvariant(); // "my string"
    s = s.ToUpperInvariant(); // "MY STRING"


Notez que vous *pouvez* choisir de spécifier une **[Culture](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo(v=vs.110).aspx)** spécifique lors de la conversion en minuscules et majuscules à l'aide de [String.ToLower(CultureInfo)](https://msdn.microsoft.com/en-us/library/s8z5yt00(v=vs.110).aspx) et [String.ToUpper (CultureInfo)](https://msdn.microsoft.com/en-us/library/24kc78ka(v=vs.110).aspx) méthodes en conséquence.



## Concaténer un tableau de chaînes en une seule chaîne
La méthode [`System.String.Join`](https://msdn.microsoft.com/en-us/library/57a79xd0(v=vs.110).aspx) permet de concaténer tous les éléments d'un tableau de chaînes, en utilisant un séparateur spécifié entre chaque élément :

    string[] words = {"One", "Two", "Three", "Four"};
    string singleString = String.Join(",", words); // singleString = "One,Two,Three,Four"


## Concaténation de chaînes
La concaténation de chaînes peut être effectuée à l'aide de la méthode [`System.String.Concat`](https://msdn.microsoft.com/en-us/library/system.string.concat(v=vs.110).aspx) , ou (beaucoup plus facile) en utilisant l'opérateur `+` :

    string first = "Hello ";
    string second = "World";

    string concat = first + second; // concat = "Hello World"
    concat = String.Concat(first, second); // concat = "Hello World"

En C# 6, cela peut être fait comme suit :

    string concat = $"{first},{second}";



