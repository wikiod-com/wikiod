---
title: "Méthodes d'extension"
slug: "methodes-dextension"
draft: false
images: []
weight: 6772
type: docs
toc: true
---

## Syntaxe
- public static ReturnType MyExtensionMethod (cette cible TargetType)
- public static ReturnType MyExtensionMethod (cette cible TargetType, TArg1 arg1, ...)

## Paramètres
| Paramètre | Détails |
| --------- | ------- |  
| ce | Le premier paramètre d'une méthode d'extension doit toujours être précédé du mot clé `this`, suivi de l'identifiant avec lequel se référer à l'instance "actuelle" de l'objet que vous étendez |


Les méthodes d'extension sont du sucre syntaxique qui permet d'invoquer des méthodes statiques sur des instances d'objet comme si elles étaient un membre du type lui-même.

Les méthodes d'extension nécessitent un objet cible explicite. Vous devrez utiliser le mot clé `this` pour accéder à la méthode à partir du type étendu lui-même.

Les méthodes d'extension doivent être déclarées statiques et doivent vivre dans une classe statique.

**Quel espace de noms ?**

Le choix de l'espace de noms pour votre classe de méthode d'extension est un compromis entre visibilité et détectabilité.

L'[option][1] la plus souvent mentionnée est d'avoir un espace de noms personnalisé pour vos méthodes d'extension. Cependant cela impliquera un effort de communication afin que les utilisateurs de votre code sachent que les méthodes d'extension existent, et où les trouver.

Une alternative consiste à choisir un espace de noms tel que les développeurs découvriront vos méthodes d'extension via Intellisense. Donc, si vous voulez étendre la classe `Foo`, il est logique de mettre les méthodes d'extension dans le même espace de noms que `Foo`.

Il est important de réaliser que **rien ne vous empêche d'utiliser l'espace de noms de "quelqu'un d'autre"** : ainsi, si vous souhaitez étendre `IEnumerable`, vous pouvez ajouter votre méthode d'extension dans l'espace de noms `System.Linq`.

Ce n'est pas *toujours* une bonne idée. Par exemple, dans un cas précis, vous pouvez vouloir étendre un type commun (`bool IsApproxEqualTo(this double value, double other)` par exemple), mais sans que cela "pollue" l'ensemble de `System`. Dans ce cas, il est préférable de choisir un espace de noms local et spécifique.

Enfin, il est également possible de mettre les méthodes d'extension dans *aucun espace de noms* !

Une bonne question de référence : [Comment gérez-vous les espaces de noms de vos méthodes d'extension ?][2]

**Applicabilité**

Des précautions doivent être prises lors de la création de méthodes de vulgarisation pour s'assurer qu'elles sont appropriées pour toutes les entrées possibles et ne sont pas seulement pertinentes pour des situations spécifiques. Par exemple, il est possible d'étendre des classes système telles que `string`, ce qui rend votre nouveau code disponible pour **n'importe quelle** chaîne. Si votre code doit exécuter une logique spécifique au domaine sur un format de chaîne spécifique au domaine, une méthode d'extension ne serait pas appropriée car sa présence confondrait les appelants travaillant avec d'autres chaînes dans le système.

**La liste suivante contient les fonctionnalités et propriétés de base des méthodes d'extension**

1. Il doit s'agir d'une méthode statique.
2. Il doit être situé dans une classe statique.
3. Il utilise le mot clé "this" comme premier paramètre avec un type dans .NET et cette méthode sera appelée par une instance de type donnée côté client.
4. Il a également montré par VS intellisense. Lorsque nous appuyons sur le point `.` après une instance de type, il apparaît alors dans VS intellisense.
5. Une méthode d'extension doit se trouver dans le même espace de noms qu'elle est utilisée ou vous devez importer l'espace de noms de la classe à l'aide d'une instruction using.
6. Vous pouvez donner n'importe quel nom à la classe qui a une méthode d'extension, mais la classe doit être statique.
7. Si vous souhaitez ajouter de nouvelles méthodes à un type et que vous ne disposez pas du code source correspondant, la solution consiste à utiliser et à implémenter des méthodes d'extension de ce type.
8. Si vous créez des méthodes d'extension qui ont les mêmes méthodes de signature que le type que vous étendez, les méthodes d'extension ne seront jamais appelées.


[1] : http://stackoverflow.com/q/1226189
[2] : http://stackoverflow.com/questions/2520446/how-do-you-manage-the-namespaces-of-your-extension-methods

## Méthodes d'extension - aperçu
Les méthodes d'extension ont été introduites dans C# 3.0. Les méthodes d'extension étendent et ajoutent un comportement aux types existants sans créer de nouveau type dérivé, recompiler ou modifier le type d'origine. *Elles sont particulièrement utiles lorsque vous ne pouvez pas modifier la source d'un type que vous cherchez à améliorer.* Des méthodes d'extension peuvent être créées pour les types de système, les types définis par des tiers et les types que vous avez définis vous-même. La méthode d'extension peut être invoquée comme s'il s'agissait d'une méthode membre du type d'origine. Cela permet d'utiliser **Method Chaining** pour implémenter une **Fluent Interface**.

Une méthode d'extension est créée en ajoutant une **méthode statique** à une **classe statique** qui est distincte du type d'origine étendu. La classe statique contenant la méthode d'extension est souvent créée dans le seul but de contenir les méthodes d'extension.

Les méthodes d'extension prennent un premier paramètre spécial qui désigne le type d'origine étendu. Ce premier paramètre est agrémenté du mot-clé `this` (qui constitue un usage particulier et distinct de `this` en C#&mdash;il doit être compris comme différent de l'usage de `this` qui permet de se référer aux membres de l'instance courante de l'objet) .

Dans l'exemple suivant, le type d'origine étendu est la classe `string`. `String` a été étendu par une méthode `Shorten()`, qui fournit la fonctionnalité supplémentaire de raccourcissement. La classe statique `StringExtensions` a été créée pour contenir la méthode d'extension. La méthode d'extension `Shorten()` montre qu'il s'agit d'une extension de `string` via le premier paramètre spécialement marqué. Pour montrer que la méthode `Shorten()` étend `string`, le premier paramètre est marqué par `this`. Par conséquent, la signature complète du premier paramètre est `ce texte de chaîne`, où `chaîne` est le type d'origine étendu et `texte` est le nom du paramètre choisi.

    static class StringExtensions
    {
        public static string Shorten(this string text, int length) 
        {
            return text.Substring(0, length);
        }
    }

    class Program
    {
        static void Main()
        {
            // This calls method String.ToUpper()
            var myString = "Hello World!".ToUpper();

            // This calls the extension method StringExtensions.Shorten()
            var newString = myString.Shorten(5); 

            // It is worth noting that the above call is purely syntactic sugar
            // and the assignment below is functionally equivalent
            var newString2 = StringExtensions.Shorten(myString, 5);
        }
    }

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/uiPhpP)

-------------------------------------------------- ---------------------------------

L'objet passé comme *premier argument d'une méthode d'extension* (qui est accompagné du mot clé `this`) est l'instance sur laquelle la méthode d'extension est appelée.

Par exemple, lorsque ce code est exécuté :

    "some string".Shorten(5);

Les valeurs des arguments sont les suivantes :

    text: "some string"
    length: 5

* Notez que les méthodes d'extension ne sont utilisables que si elles se trouvent dans le même espace de noms que leur définition, si l'espace de noms est importé explicitement par le code à l'aide de la méthode d'extension ou si la classe d'extension est sans espace de noms. * Les directives du framework .NET recommandent mettre les classes d'extension dans leur propre espace de noms. Cependant, cela peut entraîner des problèmes de découverte.

Cela n'entraîne aucun conflit entre les méthodes d'extension et les bibliothèques utilisées, à moins que les espaces de noms susceptibles d'entrer en conflit ne soient explicitement extraits. Par exemple [LINQ Extensions] [1] :
    
    using System.Linq; // Allows use of extension methods from the System.Linq namespace

    class Program
    {
        static void Main()
        {
            var ints = new int[] {1, 2, 3, 4};

            // Call Where() extension method from the System.Linq namespace
            var even = ints.Where(x => x % 2 == 0); 
        }
    }

[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/IF223c)

-------------------------------------------------- ---------------------------------

Depuis C# 6.0, il est également possible de mettre une directive `using static` à la _class_ contenant les méthodes d'extension. Par exemple, `using static System.Linq.Enumerable;`. Cela rend les méthodes d'extension de cette classe particulière disponibles sans amener d'autres types du même espace de noms dans la portée.

-------------------------------------------------- ---------------------------------

Lorsqu'une méthode de classe avec la même signature est disponible, le compilateur lui donne la priorité sur l'appel de méthode d'extension. Par exemple:

    class Test
    {
       public void Hello()
       {
           Console.WriteLine("From Test");
       }
    }

    static class TestExtensions
    {
        public static void Hello(this Test test)
        {
            Console.WriteLine("From extension method");
        }
    }

    class Program
    {
        static void Main()
        {
            Test t = new Test();
            t.Hello(); // Prints "From Test"
        }
    }

<!-- fin de version si -->

[Démo en direct sur .NET Fiddle](https://dotnetfiddle.net/fI3sCJ)

-------------------------------------------------- ---------------------------------

Notez que s'il y a deux fonctions d'extension avec la même signature, et que l'une d'entre elles se trouve dans le même espace de noms, alors celle-ci sera prioritaire. D'un autre côté, si les deux sont accessibles par `using`, une erreur de compilation s'ensuivra avec le message :
>**L'appel est ambigu entre les méthodes ou propriétés suivantes**


-------------------------------------------------- ---------------------------------


Notez que la commodité syntaxique d'appeler une méthode d'extension via `originalTypeInstance.ExtensionMethod()` est une commodité facultative. La méthode peut également être appelée de manière traditionnelle, de sorte que le premier paramètre spécial est utilisé comme paramètre de la méthode.

C'est-à-dire les deux travaux suivants :

    //Calling as though method belongs to string--it seamlessly extends string
    String s = "Hello World";
    s.Shorten(5);  
    
    //Calling as a traditional static method with two parameters
    StringExtensions.Shorten(s, 5);

[1] : https://www.wikiod.com/fr/docs/c%23/68/linq-queries

## Vérification nulle
Les méthodes d'extension sont des méthodes statiques qui se comportent comme des méthodes d'instance. Cependant, contrairement à ce qui se passe lors de l'appel d'une méthode d'instance sur une référence `null`, lorsqu'une méthode d'extension est appelée avec une référence `null`, elle ne lève pas de [`NullReferenceException`][1]. Cela peut être très utile dans certains scénarios.

Par exemple, considérez la classe statique suivante :

    public static class StringExtensions
    {
        public static string EmptyIfNull(this string text)
        {
            return text ?? String.Empty;
        }

        public static string NullIfEmpty(this string text)
        {
            return String.Empty == text ? null : text;
        }
    }

<!-- séparé -->

    string nullString = null;
    string emptyString = nullString.EmptyIfNull();// will return ""
    string anotherNullString = emptyString.NullIfEmpty(); // will return null

[Démo en direct sur .NET Fiddle][2]

[1] : https://msdn.microsoft.com/en-us/library/system.nullreferenceexception(v=vs.110).aspx
[2] : https://dotnetfiddle.net/jNQWqg

## Utiliser explicitement une méthode d'extension
Les méthodes d'extension peuvent également être utilisées comme des méthodes de classe statique ordinaires. Cette façon d'appeler une méthode d'extension est plus détaillée, mais est nécessaire dans certains cas.

    static class StringExtensions
    {
        public static string Shorten(this string text, int length) 
        {
            return text.Substring(0, length);
        }
    }

Usage:

    var newString = StringExtensions.Shorten("Hello World", 5);

# Quand appeler les méthodes d'extension en tant que méthodes statiques

Il existe encore des scénarios dans lesquels vous auriez besoin d'utiliser une méthode d'extension en tant que méthode statique :

* Résoudre les conflits avec une méthode membre. Cela peut se produire si une nouvelle version d'une bibliothèque introduit une nouvelle méthode membre avec la même signature. Dans ce cas, la méthode membre sera préférée par le compilateur.
* Résoudre les conflits avec une autre méthode d'extension avec la même signature. Cela peut se produire si deux bibliothèques incluent des méthodes d'extension similaires et que les espaces de noms des deux classes avec des méthodes d'extension sont utilisés dans le même fichier.
* Passer la méthode d'extension en tant que groupe de méthodes dans le paramètre délégué.
* Faire votre propre reliure via `Reflection`.
* Utilisation de la méthode d'extension dans la fenêtre Exécution de Visual Studio.

# Utilisation statique

Si une directive `using static` est utilisée pour amener les membres statiques d'une classe statique dans la portée globale, les méthodes d'extension sont ignorées. Exemple:

    using static OurNamespace.StringExtensions; // refers to class in previous example

    // OK: extension method syntax still works.
    "Hello World".Shorten(5);
    // OK: static method syntax still works.
    OurNamespace.StringExtensions.Shorten("Hello World", 5);
    // Compile time error: extension methods can't be called as static without specifying class.
    Shorten("Hello World", 5);

Si vous supprimez le modificateur `this` du premier argument de la méthode `Shorten`, la dernière ligne sera compilée.


## Les méthodes d'extension ne peuvent voir que les membres publics (ou internes) de la classe étendue
    public class SomeClass
    {
        public void DoStuff()
        {
            
        }

        protected void DoMagic()
        {
            
        }
    }

    public static class SomeClassExtensions
    {
        public static void DoStuffWrapper(this SomeClass someInstance)
        {
            someInstance.DoStuff(); // ok
        }

        public static void DoMagicWrapper(this SomeClass someInstance)
        {
            someInstance.DoMagic(); // compilation error
        }
    }

Les méthodes d'extension ne sont qu'un sucre syntaxique et ne sont pas réellement membres de la classe qu'elles étendent. Cela signifie qu'ils ne peuvent pas casser l'encapsulation - ils n'ont accès qu'aux champs, propriétés et méthodes "publics" (ou lorsqu'ils sont implémentés dans le même assembly, "internes").

## Méthodes d'extension génériques
Tout comme les autres méthodes, les méthodes d'extension peuvent utiliser des génériques. Par exemple:

    static class Extensions
    {
        public static bool HasMoreThanThreeElements<T>(this IEnumerable<T> enumerable)
        {
            return enumerable.Take(4).Count() > 3;
        }
    }
et l'appeler serait comme:

    IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
    var hasMoreThanThreeElements = numbers.HasMoreThanThreeElements();

[Voir la démo][1]

De même pour plusieurs arguments de type :

    public static TU GenericExt<T, TU>(this T obj)
    {
         TU ret = default(TU);
         // do some stuff with obj
         return ret;
    }

L'appeler serait comme:

    IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
    var result = numbers.GenericExt<IEnumerable<int>,String>();

[Voir la démo][2]

Vous pouvez également créer des méthodes d'extension pour les types partiellement liés dans plusieurs types génériques :

    class MyType<T1, T2>
    {
    }
    
    static class Extensions
    {
        public static void Example<T>(this MyType<int, T> test)
        {        
        }
    }

L'appeler serait comme:

    MyType<int, string> t = new MyType<int, string>();
    t.Example();

[Voir la démo][4]

Vous pouvez également spécifier des contraintes de type avec [`where`][3] :

    public static bool IsDefault<T>(this T obj) where T : struct, IEquatable<T>
    {
         return EqualityComparer<T>.Default.Equals(obj, default(T));
    }

Indicatif d'appel :

    int number = 5;
    var IsDefault = number.IsDefault();

[Voir la démo][5]


[1] : https://dotnetfiddle.net/UlCa3i
[2] : https://dotnetfiddle.net/aMNO0X
[3] : https://www.wikiod.com/fr/docs/c%23/26/keywords/8137/where-type-constraints#t=201607221442171394675
[4] : https://dotnetfiddle.net/1FjUOH
[5] : https://dotnetfiddle.net/Jom3cS

## Méthodes d'extension pour le chaînage
Lorsqu'une méthode d'extension renvoie une valeur qui a le même type que son argument `this`, elle peut être utilisée pour "chaîner" un ou plusieurs appels de méthode avec une signature compatible. Cela peut être utile pour les types scellés et/ou primitifs, et permet la création d'API dites "fluides" si les noms de méthodes se lisent comme un langage humain naturel.

    void Main()
    {
        int result = 5.Increment().Decrement().Increment(); 
        // result is now 6
    }
    
    public static class IntExtensions 
    {
        public static int Increment(this int number) {
            return ++number;
        }

        public static int Decrement(this int number) {
            return --number;
        }
    }

Ou comme ça

    void Main()
    {
        int[] ints = new[] { 1, 2, 3, 4, 5, 6};
        int[] a = ints.WhereEven();
        //a is { 2, 4, 6 };
        int[] b = ints.WhereEven().WhereGreaterThan(2);
        //b is { 4, 6 };
    }
    
    public static class IntArrayExtensions
    {
        public static int[] WhereEven(this int[] array)
        {
            //Enumerable.* extension methods use a fluent approach
            return array.Where(i => (i%2) == 0).ToArray();
        }
    
        public static int[] WhereGreaterThan(this int[] array, int value)
        {
            return array.Where(i => i > value).ToArray();
        }
    }

## Méthodes d'extension avec énumération
Les méthodes d'extension sont utiles pour ajouter des fonctionnalités aux énumérations.

Une utilisation courante consiste à implémenter une méthode de conversion.

    public enum YesNo
    {
        Yes,
        No,
    }
    
    public static class EnumExtentions
    {
        public static bool ToBool(this YesNo yn)
        {
            return yn == YesNo.Yes;
        }
        public static YesNo ToYesNo(this bool yn)
        {
            return yn ? YesNo.Yes : YesNo.No;
        }
    }

Vous pouvez maintenant convertir rapidement votre valeur enum en un type différent. Dans ce cas, un booléen.

    bool yesNoBool = YesNo.Yes.ToBool(); // yesNoBool == true
    YesNo yesNoEnum = false.ToYesNo();   // yesNoEnum == YesNo.No


Alternativement, les méthodes d'extension peuvent être utilisées pour ajouter des propriétés comme des méthodes.

    public enum Element
    {
        Hydrogen,
        Helium,
        Lithium,
        Beryllium,
        Boron,
        Carbon,
        Nitrogen,
        Oxygen
        //Etc
    }

    public static class ElementExtensions
    {
        public static double AtomicMass(this Element element)
        {
            switch(element)
            {
                case Element.Hydrogen:  return 1.00794;
                case Element.Helium:    return 4.002602;
                case Element.Lithium:   return 6.941;
                case Element.Beryllium: return 9.012182;
                case Element.Boron:     return 10.811;
                case Element.Carbon:    return 12.0107;
                case Element.Nitrogen:  return 14.0067;
                case Element.Oxygen:    return 15.9994;
                //Etc
            }
            return double.Nan;
        }
    }

    var massWater = 2*Element.Hydrogen.AtomicMass() + Element.Oxygen.AtomicMass();

## Distribution des méthodes d'extension en fonction du type statique
Le type statique (à la compilation) est utilisé plutôt que le type dynamique (à l'exécution) pour faire correspondre les paramètres.

    public class Base 
    { 
        public virtual string GetName()
        {
            return "Base";
        }
    }

    public class Derived : Base
    { 
        public override string GetName()
        {
            return "Derived";
        }
    }

    public static class Extensions
    {
        public static string GetNameByExtension(this Base item)
        {
            return "Base";
        }

        public static string GetNameByExtension(this Derived item)
        {
            return "Derived";
        }
    }

    public static class Program   
    {
        public static void Main()
        {
            Derived derived = new Derived();
            Base @base = derived;

            // Use the instance method "GetName"
            Console.WriteLine(derived.GetName()); // Prints "Derived"
            Console.WriteLine(@base.GetName()); // Prints "Derived"

            // Use the static extension method "GetNameByExtension"
            Console.WriteLine(derived.GetNameByExtension()); // Prints "Derived"
            Console.WriteLine(@base.GetNameByExtension()); // Prints "Base"
        }
    }
[Démo en direct sur .NET Fiddle] (https://dotnetfiddle.net/7BGp8o)

De plus, la répartition basée sur le type statique ne permet pas d'appeler une méthode d'extension sur un objet "dynamique" :

    public class Person
    {
        public string Name { get; set; }
    }
    
    public static class ExtenionPerson
    {
        public static string GetPersonName(this Person person)
        {
            return person.Name;
        }
    }
    
    dynamic person = new Person { Name = "Jon" };
    var name = person.GetPersonName(); // RuntimeBinderException is thrown

## Méthodes d'extension sur les interfaces
Une fonctionnalité utile des méthodes d'extension est que vous pouvez créer des méthodes communes pour une interface. Normalement, une interface ne peut pas avoir d'implémentations partagées, mais avec les méthodes d'extension, elles le peuvent.

    public interface IVehicle
    {
        int MilesDriven { get; set; }
    }
    
    public static class Extensions
    {
        public static int FeetDriven(this IVehicle vehicle)
        {
            return vehicle.MilesDriven * 5028;
        }
    }

Dans cet exemple, la méthode `FeetDriven` peut être utilisée sur n'importe quel `IVehicle`. Cette logique dans cette méthode s'appliquerait à tous les `IVehicle`s, donc cela peut être fait de cette façon afin qu'il n'y ait pas besoin d'avoir un `FeetDriven` dans la définition de `IVehicle` qui serait implémenté de la même manière pour tous les enfants .

## Les méthodes d'extension ne sont pas prises en charge par le code dynamique.

    static class Program
    {
        static void Main()
        {
            dynamic dynamicObject = new ExpandoObject();
    
            string awesomeString = "Awesome";
    
            // Prints True
            Console.WriteLine(awesomeString.IsThisAwesome());
    
            dynamicObject.StringValue = awesomeString;
    
            // Prints True
            Console.WriteLine(StringExtensions.IsThisAwesome(dynamicObject.StringValue)); 
            
            // No compile time error or warning, but on runtime throws RuntimeBinderException
            Console.WriteLine(dynamicObject.StringValue.IsThisAwesome());
        }
    }
    
    static class StringExtensions
    {
        public static bool IsThisAwesome(this string value)
        {
            return value.Equals("Awesome");
        }
    }

> La raison pour laquelle [l'appel des méthodes d'extension à partir du code dynamique] ne fonctionne pas est que, dans le code normal, les méthodes d'extension non dynamiques fonctionnent en effectuant une recherche complète de toutes les classes connues du compilateur pour une classe statique qui a une méthode d'extension qui allumettes. La recherche s'effectue dans l'ordre en fonction de l'imbrication de l'espace de noms et des directives "using" disponibles dans chaque espace de noms.
> 
> Cela signifie que pour obtenir une invocation de méthode d'extension dynamique résolue correctement, le DLR doit savoir *au moment de l'exécution* ce que toutes les imbrications d'espace de noms et les directives "using" étaient *dans votre code source*. Nous n'avons pas de mécanisme pratique pour coder toutes ces informations dans le site d'appel. Nous avons envisagé d'inventer un tel mécanisme, mais nous avons décidé qu'il était trop coûteux et qu'il produisait trop de risques liés au calendrier pour en valoir la peine.

[Source](http://stackoverflow.com/a/5313149/1610754)

## Méthodes d'extension en combinaison avec des interfaces
Il est très pratique d'utiliser des méthodes d'extension avec des interfaces car l'implémentation peut être stockée en dehors de la classe et tout ce qu'il faut pour ajouter des fonctionnalités à la classe est de décorer la classe avec l'interface.

    public interface IInterface
    {
       string Do()
    }

    public static class ExtensionMethods{
        public static string DoWith(this IInterface obj){
          //does something with IInterface instance
        }
    }

    public class Classy : IInterface
    {
       // this is a wrapper method; you could also call DoWith() on a Classy instance directly,
       // provided you import the namespace containing the extension method
       public Do(){
           return this.DoWith();
       }
    }


utiliser comme:

     var classy = new Classy();
     classy.Do(); // will call the extension
     classy.DoWith(); // Classy implements IInterface so it can also be called this way

## Les extensions et les interfaces permettent ensemble le code DRY et les fonctionnalités de type mixin
Les méthodes d'extension vous permettent de simplifier vos définitions d'interface en n'incluant que les fonctionnalités essentielles requises dans l'interface elle-même et en vous permettant de définir des méthodes pratiques et des surcharges en tant que méthodes d'extension. Les interfaces avec moins de méthodes sont plus faciles à implémenter dans les nouvelles classes. Conserver les surcharges en tant qu'extensions plutôt que de les inclure directement dans l'interface vous évite de copier du code passe-partout dans chaque implémentation, ce qui vous aide à garder votre code SEC. Ceci est en fait similaire au modèle mixin que C # ne prend pas en charge.

Les extensions de `System.Linq.Enumerable` à `IEnumerable<T>` en sont un excellent exemple. `IEnumerable<T>` nécessite uniquement que la classe d'implémentation implémente deux méthodes : générique et non générique `GetEnumerator()`. Mais `System.Linq.Enumerable` fournit d'innombrables utilitaires utiles en tant qu'extensions permettant une utilisation concise et claire de `IEnumerable<T>`.

Ce qui suit est une interface très simple avec des surcharges pratiques fournies en tant qu'extensions.

    public interface ITimeFormatter
    {
       string Format(TimeSpan span);
    }

    public static class TimeFormatter
    {
        // Provide an overload to *all* implementers of ITimeFormatter.
        public static string Format(
            this ITimeFormatter formatter,
            int millisecondsSpan)
            => formatter.Format(TimeSpan.FromMilliseconds(millisecondsSpan));
    }

    // Implementations only need to provide one method. Very easy to
    // write additional implementations.
    public class SecondsTimeFormatter : ITimeFormatter
    {
       public string Format(TimeSpan span)
       {
           return $"{(int)span.TotalSeconds}s";
       }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var formatter = new SecondsTimeFormatter();
            // Callers get two method overloads!
            Console.WriteLine($"4500ms is rougly {formatter.Format(4500)}");
            var span = TimeSpan.FromSeconds(5);
            Console.WriteLine($"{span} is formatted as {formatter.Format(span)}");
        }
    }


## Exemple de méthode d'extension IList<T> : comparaison de 2 listes
Vous pouvez utiliser la méthode d'extension suivante pour comparer le contenu de deux instances IList< T > du même type.

Par défaut, les éléments sont comparés en fonction de leur ordre dans la liste et des éléments eux-mêmes, passer false au paramètre `isOrdered` ne comparera que les éléments eux-mêmes, quel que soit leur ordre.

Pour que cette méthode fonctionne, le type générique (`T`) doit remplacer les méthodes `Equals` et `GetHashCode`.

**Usage:**

    List<string> list1 = new List<string> {"a1", "a2", null, "a3"};
    List<string> list2 = new List<string> {"a1", "a2", "a3", null};

    list1.Compare(list2);//this gives false
    list1.Compare(list2, false);//this gives true. they are equal when the order is disregarded

**Méthode:**

    public static bool Compare<T>(this IList<T> list1, IList<T> list2, bool isOrdered = true) 
    {
        if (list1 == null && list2 == null)
            return true;
        if (list1 == null || list2 == null || list1.Count != list2.Count)
            return false;

        if (isOrdered)
        {
            for (int i = 0; i < list2.Count; i++)
            {
                var l1 = list1[i]; 
                var l2 = list2[i];
                if (
                     (l1 == null && l2 != null) || 
                     (l1 != null && l2 == null) || 
                     (!l1.Equals(l2)))
                {
                        return false;
                }
            }
            return true;
        }
        else
        {
            List<T> list2Copy = new List<T>(list2);
            //Can be done with Dictionary without O(n^2)
            for (int i = 0; i < list1.Count; i++)
            {
                if (!list2Copy.Remove(list1[i]))
                    return false;
            }
            return true;
        }
    }

## Méthodes d'extension en tant que wrappers fortement typés
Les méthodes d'extension peuvent être utilisées pour écrire des wrappers fortement typés pour des objets de type dictionnaire. Par exemple un cache, `HttpContext.Items` et cetera...

    public static class CacheExtensions
    {
        public static void SetUserInfo(this Cache cache, UserInfo data) => 
            cache["UserInfo"] = data;

        public static UserInfo GetUserInfo(this Cache cache) => 
            cache["UserInfo"] as UserInfo;
    }

Cette approche supprime la nécessité d'utiliser des littéraux de chaîne comme clés dans toute la base de code ainsi que la nécessité de convertir le type requis lors de l'opération de lecture. Dans l'ensemble, cela crée une manière plus sécurisée et fortement typée d'interagir avec des objets aussi vaguement typés que les dictionnaires.

## Utilisation des méthodes d'extension pour créer de belles classes de mappeur
Nous pouvons créer de meilleures classes de mappeur avec des méthodes d'extension,
Supposons que si j'ai des classes DTO comme

     public class UserDTO
     {
            public AddressDTO Address { get; set; }
     }
    
     public class AddressDTO
     {
            public string Name { get; set; }
     }

et j'ai besoin de mapper aux classes de modèles de vue correspondantes

    public class UserViewModel
    {
        public AddressViewModel Address { get; set; }
    }
    
    public class AddressViewModel
    {
        public string Name { get; set; }
    }

alors je peux créer ma classe mapper comme ci-dessous

    public static class ViewModelMapper
    {
          public static UserViewModel ToViewModel(this UserDTO user)
          {
                return user == null ?
                    null :
                    new UserViewModel()
                    {
                        Address = user.Address.ToViewModel()
                        // Job = user.Job.ToViewModel(),
                        // Contact = user.Contact.ToViewModel() .. and so on
                    };
          }
    
          public static AddressViewModel ToViewModel(this AddressDTO userAddr)
          {
                return userAddr == null ?
                    null :
                    new AddressViewModel()
                    {
                        Name = userAddr.Name
                    };
          }
    }

Enfin, je peux invoquer mon mappeur comme ci-dessous

        UserDTO userDTOObj = new UserDTO() {
                Address = new AddressDTO() {
                    Name = "Address of the user"
                }
            };

        UserViewModel user = userDTOObj.ToViewModel(); // My DTO mapped to Viewmodel


La beauté ici est que toutes les méthodes de mappage ont un nom commun (ToViewModel) et nous pouvons les réutiliser de plusieurs façons

## Utilisation de méthodes d'extension pour créer de nouveaux types de collections (par exemple, DictList)
Vous pouvez créer des méthodes d'extension pour améliorer la convivialité des collections imbriquées comme un `Dictionary` avec une valeur `List<T>`.

Considérez les méthodes d'extension suivantes :

    public static class DictListExtensions
    {
        public static void Add<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
                where TCollection : ICollection<TValue>, new()
        {
            TCollection list;
            if (!dict.TryGetValue(key, out list))
            {
                list = new TCollection();
                dict.Add(key, list);
            }

            list.Add(value);
        }

        public static bool Remove<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
            where TCollection : ICollection<TValue>
        {
            TCollection list;
            if (!dict.TryGetValue(key, out list))
            {
                return false;
            }

            var ret = list.Remove(value);
            if (list.Count == 0)
            {
                dict.Remove(key);
            }
            return ret;
        }
    }

vous pouvez utiliser les méthodes d'extension comme suit :

    var dictList = new Dictionary<string, List<int>>();

    dictList.Add("example", 5);
    dictList.Add("example", 10);
    dictList.Add("example", 15);
    
    Console.WriteLine(String.Join(", ", dictList["example"])); // 5, 10, 15

    dictList.Remove("example", 5);
    dictList.Remove("example", 10);
    
    Console.WriteLine(String.Join(", ", dictList["example"])); // 15
    
    dictList.Remove("example", 15);
    
    Console.WriteLine(dictList.ContainsKey("example")); // False

[Voir la démo] (https://dotnetfiddle.net/UbdQuC)

## Méthodes d'extension pour gérer les cas particuliers

Les méthodes d'extension peuvent être utilisées pour "masquer" le traitement de règles métier inélégantes qui, autrement, nécessiteraient d'encombrer une fonction appelante avec des instructions if/then. Ceci est similaire et analogue à la gestion des valeurs nulles avec les méthodes d'extension. Par exemple,

    public static class CakeExtensions
    {
        public static Cake EnsureTrueCake(this Cake cake)
        {
            //If the cake is a lie, substitute a cake from grandma, whose cakes aren't as tasty but are known never to be lies. If the cake isn't a lie, don't do anything and return it.
            return CakeVerificationService.IsCakeLie(cake) ? GrandmasKitchen.Get1950sCake() : cake;
        }
    }

<!-- séparé -->

    Cake myCake = Bakery.GetNextCake().EnsureTrueCake();
    myMouth.Eat(myCake);//Eat the cake, confident that it is not a lie.


## Utilisation de méthodes d'extension avec des méthodes statiques et des rappels
Envisagez d'utiliser des méthodes d'extension en tant que fonctions qui encapsulent d'autres codes, voici un excellent exemple qui utilise à la fois une méthode statique et une méthode d'extension pour encapsuler la construction Try Catch. Rendez votre code à l'épreuve des balles...

    using System;
    using System.Diagnostics;
    
    namespace Samples
    {
        /// <summary>
        /// Wraps a try catch statement as a static helper which uses 
        /// Extension methods for the exception
        /// </summary>
        public static class Bullet
        {
            /// <summary>
            /// Wrapper for Try Catch Statement
            /// </summary>
            /// <param name="code">Call back for code</param>
            /// <param name="error">Already handled and logged exception</param>
            public static void Proof(Action code, Action<Exception> error)
            {
                try
                {
                    code();
                }
                catch (Exception iox)
                {
                    //extension method used here
                    iox.Log("BP2200-ERR-Unexpected Error");
                    //callback, exception already handled and logged
                    error(iox);
                }
            }
            /// <summary>
            /// Example of a logging method helper, this is the extension method
            /// </summary>
            /// <param name="error">The Exception to log</param>
            /// <param name="messageID">A unique error ID header</param>
            public static void Log(this Exception error, string messageID)
            {
                Trace.WriteLine(messageID);
                Trace.WriteLine(error.Message);
                Trace.WriteLine(error.StackTrace);
                Trace.WriteLine("");
            }
        }
        /// <summary>
        /// Shows how to use both the wrapper and extension methods.
        /// </summary>
        public class UseBulletProofing
        {
            public UseBulletProofing()
            {
                var ok = false;
                var result = DoSomething();
                if (!result.Contains("ERR"))
                {
                    ok = true;
                    DoSomethingElse();
                }
            }
    
            /// <summary>
            /// How to use Bullet Proofing in your code.
            /// </summary>
            /// <returns>A string</returns>
            public string DoSomething()
            {
                string result = string.Empty;
                //Note that the Bullet.Proof method forces this construct.
                Bullet.Proof(() =>
                {
                    //this is the code callback
                    result = "DST5900-INF-No Exceptions in this code";
                }, error =>
                {
                    //error is the already logged and handled exception
                    //determine the base result
                    result = "DTS6200-ERR-An exception happened look at console log";
                    if (error.Message.Contains("SomeMarker"))
                    {
                        //filter the result for Something within the exception message
                        result = "DST6500-ERR-Some marker was found in the exception";
                    }
                });
                return result;
            }
    
            /// <summary>
            /// Next step in workflow
            /// </summary>
            public void DoSomethingElse()
            {
                //Only called if no exception was thrown before
            }
        }
    }

