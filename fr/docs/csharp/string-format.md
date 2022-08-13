---
title: "String.Format"
slug: "stringformat"
draft: false
images: []
weight: 9519
type: docs
toc: true
---

Les méthodes `Format` sont un ensemble de [overloads][1] dans la classe [`System.String`][2] utilisées pour créer des chaînes qui combinent des objets dans des représentations de chaînes spécifiques. Ces informations peuvent être appliquées à [`String.Format`][1], à diverses méthodes `WriteLine` ainsi qu'à d'autres méthodes du framework .NET.

[1] : https://msdn.microsoft.com/en-us/library/system.string.format(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx

## Syntaxe
- string.Format(format de chaîne, params object[] args)
- string.Format (fournisseur IFormatProvider, format de chaîne, params object [] args)
- $"string {text} blablabla" // Depuis C#6

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| formater | Une [chaîne de format composite][1], qui définit la manière dont les *args* doivent être combinés en une chaîne. |
| arguments | Séquence d'objets à combiner en une chaîne. Comme cela utilise un argument [`params`][2], vous pouvez soit utiliser une liste d'arguments séparés par des virgules, soit un véritable tableau d'objets. |
| fournisseur | Une collection de façons de formater des objets en chaînes. Les valeurs typiques incluent [CultureInfo.InvariantCulture][3] et [CultureInfo.CurrentCulture][4]. |


[1] : https://msdn.microsoft.com/en-us/library/txafckwd(v=vs.110).aspx
[2] : https://www.wikiod.com/fr/docs/c%23/26/keywords/2513/params#t=201607212143476676934
[3] : https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.invariantculture(v=vs.110).aspx
[4] : https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx

Remarques:

- `String.Format()` gère les arguments `null` sans lever d'exception.
- Il existe des surcharges qui remplacent le paramètre `args` par un, deux ou trois paramètres d'objet.



## Depuis C# 6.0
<!-- si version [gte 6.0] -->

Depuis C# 6.0, il est possible d'utiliser l'interpolation de chaîne à la place de `String.Format`.

    string name = "John";
    string lastname = "Doe";
    Console.WriteLine($"Hello {name} {lastname}!");

> Bonjour John Doe !

<sup>Plus d'exemples pour cela sous la rubrique Fonctionnalités C# 6.0 : https://www.wikiod.com/fr/docs/c%23/24/c-sharp-6-0-features/49/string-interpolation#t=201607220912379524818. </sup>

<!-- fin de version si -->
 

## Endroits où String.Format est "intégré" dans le framework
Il existe plusieurs endroits où vous pouvez utiliser `String.Format` *indirectement* : le secret est de rechercher la surcharge avec la signature `string format, params object[] args`, par exemple :

    Console.WriteLine(String.Format("{0} - {1}", name, value));

Peut être remplacé par une version plus courte :

    Console.WriteLine("{0} - {1}", name, value);

Il existe d'autres méthodes qui utilisent également `String.Format`, par exemple :

    Debug.WriteLine(); // and Print()
    StringBuilder.AppendFormat();

## Créer un fournisseur de format personnalisé
    public class CustomFormat : IFormatProvider, ICustomFormatter
    {
        public string Format(string format, object arg, IFormatProvider formatProvider)
        {
            if (!this.Equals(formatProvider))
            {
                return null;
            }

            if (format == "Reverse")
            {
                return String.Join("", arg.ToString().Reverse());
            }

            return arg.ToString();
        }

        public object GetFormat(Type formatType)
        {
            return formatType==typeof(ICustomFormatter) ? this:null;
        }
    }

Usage:

    String.Format(new CustomFormat(), "-> {0:Reverse} <-", "Hello World");

Production:

    -> dlroW olleH <-

## Formatage des dates
    DateTime date = new DateTime(2016, 07, 06, 18, 30, 14);
    // Format: year, month, day hours, minutes, seconds

    Console.Write(String.Format("{0:dd}",date)); 

    //Format by Culture info
    String.Format(new System.Globalization.CultureInfo("mn-MN"),"{0:dddd}",date);

<!-- si version [gte 6.0] -->
    Console.Write($"{date:ddd}");
<!-- fin de version si -->

production :

    06
    Лхагва
    06

| Prescripteur| Signification| Échantillon| Résultat|
| ------ | ------ | ------ | ------ |
|d| Date |`{0:d}`|06/07/2016|
|jj| Jour, complété par des zéros |`{0:dd}`|06|
|ddd|Nom du jour abrégé|`{0:ddd}`|Mer|
|dddd|Nom complet du jour|`{0:dddd}`|Mercredi|
|D|Longue date|`{0:D}`|mercredi 6 juillet 2016|
|f|Date et heure complètes, courtes|`{0:f}`|Mercredi 6 juillet 2016 18h30|
|ff|Deuxièmes fractions, 2 chiffres|`{0:ff}`|20|
|fff|Deuxièmes fractions, 3 chiffres|`{0:fff}`|201|
|ffff|Deuxièmes fractions, 4 chiffres|`{0:ffff}`|2016|
|F|Date et heure complètes, long|`{0:F}`|Mercredi 6 juillet 2016 18:30:14|
|g|Date et heure par défaut|`{0:g}`|06/07/2016 18h30|
|gg|Era|`{0:gg}`|A.D|
|hh|Heure (2 chiffres, 12H)|`{0:hh}`|06|
|HH|Heure (2 chiffres, 24H)|`{0:HH}`|18|
|M|Mois et jour|`{0:M}`|6 juillet|
|mm|Minutes, complétées par des zéros|`{0:mm}`|30|
|MM|Mois, complété par des zéros|`{0:MM}`|07|
|MMM|Nom du mois à 3 lettres|`{0:MMM}`|Jul|
|MMMM|Nom complet du mois|`{0:MMMM}`|Juillet|
|ss|Secondes|`{0:ss}`|14|
|r| RFC1123 date|`{0:r}`|mer, 06 juillet 2016 18:30:14 GMT|
|s| Chaîne de date triable|`{0:s}`|2016-07-06T18:30:14|
|t| Courte durée |`{0:t}`|18h30|
|T|Longtemps|`{0:T}`|18:30:14|
|ht|AM/PM|`{0:ht}`|PM|
|u|Heure locale triable universelle|`{0:u}`|2016-07-06 18:30:14Z|
|U| Universal GMT|`{0:U}`|Mercredi 6 juillet 2016 09:30:14|
|Y| Mois et année|`{0:Y}`|Juillet 2016|
|aa|année à 2 chiffres|`{0:aa}`|16|
|aaaa|année à 4 chiffres|`{0:aaaa}`|2016|
|zz|Décalage du fuseau horaire à 2 chiffres|`{0:zz}`|+09|
|zzz|décalage de fuseau horaire complet|`{0:zzz}`|+09:00|

## Formatage des devises
Le spécificateur de format "c" (ou devise) convertit un nombre en une chaîne qui représente un montant en devise.

    string.Format("{0:c}", 112.236677) // $112.23 - defaults to system

## Précision ##
La valeur par défaut est 2. Utilisez c1, c2, c3 et ainsi de suite pour contrôler la précision.

    string.Format("{0:C1}", 112.236677) //$112.2
    string.Format("{0:C3}", 112.236677) //$112.237
    string.Format("{0:C4}", 112.236677) //$112.2367
    string.Format("{0:C9}", 112.236677) //$112.236677000

## Symbole de la monnaie ##

1. Passez l'instance `CultureInfo` pour utiliser le symbole de culture personnalisé.


    string.Format(new CultureInfo("en-US"), "{0:c}", 112.236677); //$112.24
    string.Format(new CultureInfo("de-DE"), "{0:c}", 112.236677); //112,24 €
    string.Format(new CultureInfo("hi-IN"), "{0:c}", 112.236677); //₹ 112.24


2. Utilisez n'importe quelle chaîne comme symbole monétaire. Utilisez `NumberFormatInfo` pour personnaliser le symbole monétaire.


    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;
    nfi = (NumberFormatInfo) nfi.Clone();
    nfi.CurrencySymbol = "?";
    string.Format(nfi, "{0:C}", 112.236677); //?112.24
    nfi.CurrencySymbol = "?%^&";
    string.Format(nfi, "{0:C}", 112.236677); //?%^&112.24

## Position du symbole monétaire ##

Utilisez [CurrencyPositivePattern][1] pour les valeurs positives et [CurrencyNegativePattern][2] pour les valeurs négatives.

    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
    nfi.CurrencyPositivePattern = 0;
    string.Format(nfi, "{0:C}", 112.236677); //$112.24 - default
    nfi.CurrencyPositivePattern = 1;
    string.Format(nfi, "{0:C}", 112.236677); //112.24$
    nfi.CurrencyPositivePattern = 2;
    string.Format(nfi, "{0:C}", 112.236677); //$ 112.24
    nfi.CurrencyPositivePattern = 3; 
    string.Format(nfi, "{0:C}", 112.236677); //112.24 $

L'utilisation du motif négatif est la même que celle du motif positif. Beaucoup plus de cas d'utilisation, veuillez vous référer au lien d'origine.

## Séparateur décimal personnalisé ##

    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
    nfi.CurrencyPositivePattern = 0;
    nfi.CurrencyDecimalSeparator = "..";
    string.Format(nfi, "{0:C}", 112.236677); //$112..24

[1] : https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencypositivepattern(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencynegativepattern(v=vs.110).aspx

## Utilisation d'un format numérique personnalisé
`NumberFormatInfo` peut être utilisé pour formater à la fois les nombres entiers et flottants.

    // invariantResult is "1,234,567.89"
    var invarianResult = string.Format(CultureInfo.InvariantCulture, "{0:#,###,##}", 1234567.89);

    // NumberFormatInfo is one of classes that implement IFormatProvider
    var customProvider = new NumberFormatInfo
    {
        NumberDecimalSeparator = "_NS_", // will be used instead of ','
        NumberGroupSeparator = "_GS_", // will be used instead of '.'
    };

    // customResult is "1_GS_234_GS_567_NS_89"
    var customResult = string.Format(customProvider, "{0:#,###.##}", 1234567.89);



## Aligner à gauche/droite, remplir avec des espaces
La deuxième valeur entre les accolades dicte la longueur de la chaîne de remplacement.
En ajustant la deuxième valeur pour qu'elle soit positive ou négative, l'alignement de la chaîne peut être modifié.

    string.Format("LEFT:  string: ->{0,-5}<- int: ->{1,-5}<-", "abc", 123);
    string.Format("RIGHT: string: ->{0,5}<- int: ->{1,5}<-", "abc", 123);

Production:

    LEFT:  string: ->abc  <- int: ->123  <-
    RIGHT: string: ->  abc<- int: ->  123<-


## Formats numériques
    // Integral types as hex
    string.Format("Hexadecimal: byte2: {0:x2}; byte4: {0:X4}; char: {1:x2}", 123, (int)'A');

    // Integers with thousand separators
    string.Format("Integer, thousand sep.: {0:#,#}; fixed length: >{0,10:#,#}<", 1234567);

    // Integer with leading zeroes
    string.Format("Integer, leading zeroes: {0:00}; ", 1);

    // Decimals
    string.Format("Decimal, fixed precision: {0:0.000}; as percents: {0:0.00%}", 0.12);

Production:

    Hexadecimal: byte2: 7b; byte4: 007B; char: 41
    Integer, thousand sep.: 1,234,567; fixed length: > 1,234,567<
    Integer, leading zeroes: 01; 
    Decimal, fixed precision: 0.120; as percents: 12.00%


## Échapper aux accolades à l'intérieur d'une expression String.Format()
    string outsidetext = "I am outside of bracket";
    string.Format("{{I am in brackets!}} {0}", outsidetext);

    //Outputs "{I am in brackets!} I am outside of bracket"

## ToString()
La méthode ToString() est présente sur tous les types d'objets de référence. Cela est dû au fait que tous les types de référence sont dérivés de Object qui contient la méthode ToString(). La méthode ToString() sur la classe de base de l'objet renvoie le nom du type. Le fragment ci-dessous imprimera "User" sur la console.

    public class User
    {
        public string Name { get; set; }
        public int Id { get; set; }
    }

    ...

    var user = new User {Name = "User1", Id = 5};
    Console.WriteLine(user.ToString());


Cependant, la classe User peut également remplacer ToString() afin de modifier la chaîne qu'elle renvoie. Le fragment de code ci-dessous affiche "Id : 5, Name : User1" sur la console.

    public class User
    {
        public string Name { get; set; }
        public int Id { get; set; }
        public override ToString()
        {
            return string.Format("Id: {0}, Name: {1}", Id, Name);
        }
    }

    ...

    var user = new User {Name = "User1", Id = 5};
    Console.WriteLine(user.ToString());


## Relation avec ToString()
Bien que la méthode `String.Format()` soit certainement utile pour formater les données sous forme de chaînes, elle peut souvent être un peu exagérée, en particulier lorsqu'il s'agit d'un seul objet, comme indiqué ci-dessous :

    String.Format("{0:C}", money);  // yields "$42.00"

Une approche plus simple pourrait être d'utiliser simplement la méthode `ToString()` disponible sur tous les objets dans C#. Il prend en charge toutes les mêmes [chaînes de formatage standard et personnalisées](https://msdn.microsoft.com/en-us/library/dwhawy9k(v=vs.110).aspx), mais ne nécessite pas le paramètre nécessaire mapping car il n'y aura qu'un seul argument :

    money.ToString("C");  // yields "$42.00"

**Mises en garde et restrictions de formatage**
---

Bien que cette approche puisse être plus simple dans certains scénarios, l'approche `ToString()` est limitée en ce qui concerne l'ajout d'un rembourrage gauche ou droit comme vous pourriez le faire dans la méthode `String.Format()` :

    String.Format("{0,10:C}", money);  // yields "    $42.00"

Afin d'accomplir ce même comportement avec la méthode `ToString()`, vous devez utiliser une autre méthode comme `PadLeft()` ou `PadRight()` respectivement :

    money.ToString("C").PadLeft(10);  // yields "    $42.00"

