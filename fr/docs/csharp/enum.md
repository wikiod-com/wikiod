---
title: "Énumération"
slug: "enumeration"
draft: false
images: []
weight: 9420
type: docs
toc: true
---

Une énumération peut dériver de l'un des types suivants : byte, sbyte, short, ushort, int, uint, long, ulong. La valeur par défaut est int et peut être modifiée en spécifiant le type dans la définition enum :

public enum Weekday : byte { Lundi = 1, Mardi = 2, Mercredi = 3, Jeudi = 4, Vendredi = 5 }

Ceci est utile lors de l'appel P/au code natif, du mappage à des sources de données et dans des circonstances similaires. En général, l'entier par défaut doit être utilisé, car la plupart des développeurs s'attendent à ce qu'une énumération soit un int.

## Syntaxe
- enum Colors { Rouge, Vert, Bleu } // Déclaration d'énumération
- enum Colors : byte { Red, Green, Blue } // Déclaration avec un type spécifique
- enum Colors { Red = 23, Green = 45, Blue = 12 } // Déclaration avec des valeurs définies
- Colors.Red // Accéder à un élément d'un Enum
- int value = (int)Colors.Red // Récupère la valeur int d'un élément enum
- Colors color = (Colors)intValue // Récupère un élément enum à partir de int

Un Enum (abréviation de "type énuméré") est un type composé d'un ensemble de constantes nommées, représentées par un identifiant spécifique au type.

Les énumérations sont plus utiles pour représenter des concepts qui ont un nombre (généralement petit) de valeurs discrètes possibles. Par exemple, ils peuvent être utilisés pour représenter un jour de la semaine ou un mois de l'année. Ils peuvent également être utilisés comme indicateurs qui peuvent être combinés ou vérifiés, à l'aide d'opérations au niveau du bit.

## Énumération sous forme de drapeaux
Le `FlagsAttribute` peut être appliqué à une énumération en modifiant le comportement de `ToString()` pour correspondre à la nature de l'énumération :

    [Flags]
    enum MyEnum
    {
        //None = 0, can be used but not combined in bitwise operations
        FlagA = 1,
        FlagB = 2,
        FlagC = 4,
        FlagD = 8  
        //you must use powers of two or combinations of powers of two 
        //for bitwise operations to work
    }
    
    var twoFlags = MyEnum.FlagA | MyEnum.FlagB;
    
    // This will enumerate all the flags in the variable: "FlagA, FlagB".
    Console.WriteLine(twoFlags);

Étant donné que `FlagsAttribute` s'appuie sur les constantes d'énumération pour être des puissances de deux (ou leurs combinaisons) et que les valeurs enum sont finalement des valeurs numériques, vous êtes limité par la taille du type numérique sous-jacent. Le plus grand type numérique disponible que vous pouvez utiliser est `UInt64`, qui vous permet de spécifier 64 constantes d'énumération de drapeau distinctes (non combinées). Le mot clé `enum` utilise par défaut le type sous-jacent `int`, qui est `Int32`. Le compilateur autorisera la déclaration de valeurs supérieures à 32 bits. Ceux-ci s'enrouleront sans avertissement et se traduiront par deux ou plusieurs membres enum de la même valeur. Par conséquent, si une énumération est destinée à accueillir un bitset de plus de 32 drapeaux, vous devez spécifier explicitement un type plus grand :

    public enum BigEnum : ulong
    {
        BigValue = 1 << 63
    }

Bien que les drapeaux ne soient souvent qu'un seul bit, ils peuvent être combinés en "ensembles" nommés pour une utilisation plus facile.

    [Flags]
    enum FlagsEnum
    {
        None = 0,
        Option1 = 1,
        Option2 = 2,
        Option3 = 4,
           
        Default = Option1 | Option3,
        All = Option1 | Option2 | Option3,
    }

Pour éviter d'épeler les valeurs décimales des puissances de deux, l'[opérateur de décalage vers la gauche (<<)](https://msdn.microsoft.com/en-gb/library/a1sway8w.aspx) peut également être utilisé pour déclarer la même énumération

    [Flags]
    enum FlagsEnum
    {
        None = 0,
        Option1 = 1 << 0,
        Option2 = 1 << 1,
        Option3 = 1 << 2,
           
        Default = Option1 | Option3,
        All = Option1 | Option2 | Option3,
    }

À partir de C# 7.0, les [littéraux binaires](https://www.wikiod.com/fr/docs/c%23/1936/c-sharp-7-0-features/6327/binary-literals#t=201705181538117083427) peuvent également être utilisés .

Pour vérifier si la valeur de la variable enum a un certain drapeau défini, la méthode [`HasFlag`][1] peut être utilisée. Disons que nous avons

    [Flags]
    enum MyEnum
    {
        One = 1,
        Two = 2,
        Three = 4
    }

Et une "valeur"
    
    var value = MyEnum.One | MyEnum.Two;

Avec `HasFlag`, nous pouvons vérifier si l'un des drapeaux est défini
    
    if(value.HasFlag(MyEnum.One))
        Console.WriteLine("Enum has One");

    if(value.HasFlag(MyEnum.Two))
        Console.WriteLine("Enum has Two");

    if(value.HasFlag(MyEnum.Three))
        Console.WriteLine("Enum has Three");

Nous pouvons également parcourir toutes les valeurs de enum pour obtenir tous les drapeaux définis

    var type = typeof(MyEnum);
    var names = Enum.GetNames(type);

    foreach (var name in names)
    {
        var item = (MyEnum)Enum.Parse(type, name);

        if (value.HasFlag(item))
            Console.WriteLine("Enum has " + name);
    }
    
Ou

    foreach(MyEnum flagToCheck in Enum.GetValues(typeof(MyEnum)))
    {
        if(value.HasFlag(flagToCheck))
        {
             Console.WriteLine("Enum has " + flagToCheck);
        }
    }

Les trois exemples imprimeront :

    Enum has One
    Enum has Two


[1] : https://msdn.microsoft.com/en-us/library/system.enum.hasflag(v=vs.110).aspx

## Les bases de l'énumération

À partir de [MSDN][1] :
> Un type d'énumération (également appelé énumération ou énumération) fournit un moyen efficace de définir un ensemble de **constantes intégrales** nommées qui peuvent être **affectées à une variable**.

Essentiellement, une énumération est un type qui n'autorise qu'un ensemble d'options finies, et chaque option correspond à un nombre. Par défaut, ces nombres augmentent dans l'ordre dans lequel les valeurs sont déclarées, en commençant par zéro. Par exemple, on pourrait déclarer une énumération pour les jours de la semaine :

    public enum Day
    {
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
        Sunday
    }

Cette énumération pourrait être utilisée comme ceci :

    // Define variables with values corresponding to specific days
    Day myFavoriteDay = Day.Friday;
    Day myLeastFavoriteDay = Day.Monday;
    
    // Get the int that corresponds to myFavoriteDay
    // Friday is number 4
    int myFavoriteDayIndex = (int)myFavoriteDay;
    
    // Get the day that represents number 5
    Day dayFive = (Day)5;

Par défaut, le type sous-jacent de chaque élément de `enum` est `int`, mais `byte`, `sbyte`, `short`, `ushort`, `uint`, `long` et `ulong` peuvent être utilisés comme bien. Si vous utilisez un type autre que `int`, vous devez spécifier le type en utilisant deux-points après le nom de l'énumération :

    public enum Day : byte 
    {
        // same as before 
    }

Les nombres après le nom sont maintenant des octets au lieu d'entiers. Vous pouvez obtenir le type sous-jacent de l'énumération comme suit :

    Enum.GetUnderlyingType(typeof(Days)));

Production:

<!-- langue : aucune -->
    System.Byte

Démo : [violon .NET][2]

[1] : https://msdn.microsoft.com/en-us/library/cc138362.aspx

[2] : https://dotnetfiddle.net/EGi301

## Utilisation de la notation << pour les drapeaux
L'opérateur de décalage à gauche (`<<`) peut être utilisé dans les déclarations d'énumération des drapeaux pour s'assurer que chaque drapeau a exactement un '1' en représentation binaire, comme le devraient les drapeaux.

Cela contribue également à améliorer la lisibilité des grandes énumérations contenant de nombreux indicateurs.


    [Flags]
    public enum MyEnum 
    {
        None  = 0,
        Flag1 = 1 << 0,
        Flag2 = 1 << 1,
        Flag3 = 1 << 2,
        Flag4 = 1 << 3,
        Flag5 = 1 << 4,
        ...
        Flag31 = 1 << 30
    }

Il est évident maintenant que `MyEnum` contient uniquement les indicateurs appropriés et non des éléments désordonnés comme `Flag30 = 1073741822` (ou 1111111111111111111111111110 en binaire), ce qui est inapproprié.

## Tester les valeurs d'énumération de style flags avec une logique au niveau du bit
Une valeur d'énumération de style drapeaux doit être testée avec une logique au niveau du bit car elle peut ne correspondre à aucune valeur unique.

    [Flags]
    enum FlagsEnum
    {
        Option1 = 1,
        Option2 = 2,
        Option3 = 4,
        Option2And3 = Option2 | Option3;
    
        Default = Option1 | Option3,
    }
    
La valeur `Default` est en fait une combinaison de deux autres _merged_ avec un OU au niveau du bit. Par conséquent, pour tester la présence d'un indicateur, nous devons utiliser un ET au niveau du bit.

    var value = FlagsEnum.Default;

    bool isOption2And3Set = (value & FlagsEnum.Option2And3) == FlagsEnum.Option2And3;

    Assert.True(isOption2And3Set);



## Enum à la chaîne et retour
    public enum DayOfWeek
    {
        Sunday,
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday
    }
    
        
    // Enum to string
    string thursday = DayOfWeek.Thursday.ToString(); // "Thursday"
    
    string seventhDay = Enum.GetName(typeof(DayOfWeek), 6); // "Saturday"
    
    string monday = Enum.GetName(typeof(DayOfWeek), DayOfWeek.Monday); // "Monday"
    
    
    // String to enum (.NET 4.0+ only - see below for alternative syntax for earlier .NET versions)
    DayOfWeek tuesday;
    Enum.TryParse("Tuesday", out tuesday); // DayOfWeek.Tuesday
    
    DayOfWeek sunday;
    bool matchFound1 = Enum.TryParse("SUNDAY", out sunday); // Returns false (case-sensitive match)
    
    DayOfWeek wednesday;
    bool matchFound2 = Enum.TryParse("WEDNESDAY", true, out wednesday); // Returns true; DayOfWeek.Wednesday (case-insensitive match)
    
    
    // String to enum (all .NET versions)
    DayOfWeek friday = (DayOfWeek)Enum.Parse(typeof(DayOfWeek), "Friday"); // DayOfWeek.Friday

    DayOfWeek caturday = (DayOfWeek)Enum.Parse(typeof(DayOfWeek), "Caturady"); // Thows ArgumentException
    
    // All names of an enum type as strings
    string[] weekdays = Enum.GetNames(typeof(DayOfWeek));

## Ajouter et supprimer des valeurs de l'énumération marquée
Ce code consiste à ajouter et à supprimer une valeur d'une instance enum marquée :

    [Flags]
    public enum MyEnum
    {
        Flag1 = 1 << 0,
        Flag2 = 1 << 1,
        Flag3 = 1 << 2
    }

    var value = MyEnum.Flag1;

    // set additional value
    value |= MyEnum.Flag2;  //value is now Flag1, Flag2
    value |= MyEnum.Flag3;  //value is now Flag1, Flag2, Flag3

    // remove flag
    value &= ~MyEnum.Flag2; //value is now Flag1, Flag3    


## Valeur par défaut pour enum == ZERO
**La valeur par défaut d'une énumération est zéro**. Si une énumération ne définit pas un élément avec une valeur de zéro, sa valeur par défaut sera zéro.
    
    public class Program
    {        
        enum EnumExample
        {
            one = 1,
            two = 2
        }
        
        public void Main()
        {              
            var e = default(EnumExample);
            
            if (e == EnumExample.one)
                Console.WriteLine("defaults to one");
            else
                Console.WriteLine("Unknown");    
        }    
    }

Exemple:
https://dotnetfiddle.net/l5Rwie

## Ajout d'informations de description supplémentaires à une valeur d'énumération
Dans certains cas, vous souhaiterez peut-être ajouter une description supplémentaire à une valeur enum, par exemple lorsque la valeur enum elle-même est moins lisible que ce que vous souhaitez afficher pour l'utilisateur. Dans de tels cas, vous pouvez utiliser la classe [`System.ComponentModel.DescriptionAttribute`](https://msdn.microsoft.com/en-us/library/system.componentmodel.descriptionattribute(v=vs.110).aspx).

Par exemple:

    public enum PossibleResults
    {
        [Description("Success")]
        OK = 1,
        [Description("File not found")]
        FileNotFound = 2,
        [Description("Access denied")]
        AccessDenied = 3
    }

Maintenant, si vous souhaitez renvoyer la description d'une valeur enum spécifique, vous pouvez procéder comme suit :

    public static string GetDescriptionAttribute(PossibleResults result)
    {
            return ((DescriptionAttribute)Attribute.GetCustomAttribute((result.GetType().GetField(result.ToString())), typeof(DescriptionAttribute))).Description;
    }

    static void Main(string[] args)
    {
        PossibleResults result = PossibleResults.FileNotFound;
        Console.WriteLine(result); // Prints "FileNotFound"
        Console.WriteLine(GetDescriptionAttribute(result)); // Prints "File not found"
    }

Cela peut également être facilement transformé en une méthode d'extension pour toutes les énumérations :

    static class EnumExtensions
    {
        public static string GetDescription(this Enum enumValue)
        {
            return ((DescriptionAttribute)Attribute.GetCustomAttribute((enumValue.GetType().GetField(enumValue.ToString())), typeof(DescriptionAttribute))).Description;
        }
    }

Et puis facilement utilisé comme ceci:
`Console.WriteLine(result.GetDescription());`


## Les énumérations peuvent avoir des valeurs inattendues
Étant donné qu'une énumération peut être transtypée vers et depuis son type intégral sous-jacent, la valeur peut se trouver en dehors de la plage de valeurs donnée dans la définition du type enum.

Bien que le type d'énumération ci-dessous `DaysOfWeek` n'ait que 7 valeurs définies, il peut toujours contenir n'importe quelle valeur `int`.

    public enum DaysOfWeek
    {
        Monday = 1,
        Tuesday = 2,
        Wednesday = 3,
        Thursday = 4,
        Friday = 5,
        Saturday = 6,
        Sunday = 7
    }

    DaysOfWeek d = (DaysOfWeek)31;
    Console.WriteLine(d); // prints 31

    DaysOFWeek s = DaysOfWeek.Sunday;
    s++; // No error

Il n'existe actuellement aucun moyen de définir une énumération qui n'a pas ce comportement.

Cependant, des valeurs d'énumération non définies peuvent être détectées en utilisant la méthode "Enum.IsDefined". Par exemple,

    DaysOfWeek d = (DaysOfWeek)31;
    Console.WriteLine(Enum.IsDefined(typeof(DaysOfWeek),d)); // prints False

## Récupère toutes les valeurs des membres d'une énumération
    enum MyEnum
    {
        One,
        Two,
        Three
    }
    
    foreach(MyEnum e in Enum.GetValues(typeof(MyEnum)))
        Console.WriteLine(e);

Cela imprimera :

    One
    Two
    Three

## Manipulation au niveau du bit à l'aide d'énumérations
Le [FlagsAttribute][1] doit être utilisé chaque fois que l'énumérable représente une collection d'indicateurs, plutôt qu'une valeur unique.
La valeur numérique attribuée à chaque valeur d'énumération aide lors de la manipulation d'énumérations à l'aide d'opérateurs au niveau du bit.


**Exemple 1 : Avec [Drapeaux]**

    [Flags]
    enum Colors
    {
        Red=1,
        Blue=2,
        Green=4,
        Yellow=8
    }

    var color = Colors.Red | Colors.Blue;
    Console.WriteLine(color.ToString());

> imprimés Rouge,Bleu

    

****Exemple 2 : Sans [Drapeaux]****

  
    enum Colors
    {
        Red=1,
        Blue=2,
        Green=4,
        Yellow=8
    }
    var color = Colors.Red | Colors.Blue;
    Console.WriteLine(color.ToString());

> tirages 3


[1] : https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx

