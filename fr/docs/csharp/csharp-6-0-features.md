---
title: "Fonctionnalités C# 6.0"
slug: "fonctionnalites-c-60"
draft: false
images: []
weight: 994
type: docs
toc: true
---

Cette sixième itération du langage C# est fournie par le compilateur Roslyn. Ce compilateur est sorti avec la version 4.6 du .NET Framework, mais il peut générer du code de manière rétrocompatible pour permettre de cibler les versions antérieures du framework. Le code C# version 6 peut être compilé de manière totalement rétrocompatible avec .NET 4.0. Il peut également être utilisé pour les frameworks antérieurs, mais certaines fonctionnalités nécessitant une prise en charge de framework supplémentaire peuvent ne pas fonctionner correctement.

La sixième version de C # est sortie en juillet 2015 aux côtés de Visual Studio 2015 et .NET 4.6.

Outre l'ajout de nouvelles fonctionnalités de langage, il inclut une réécriture complète du compilateur. Auparavant, `csc.exe` était une application Win32 native écrite en C++, avec C# 6, c'est maintenant une application gérée .NET écrite en C#. Cette réécriture était connue sous le nom de projet "Roslyn" et le code est maintenant open source et disponible sur [GitHub][1].


[1] : https://github.com/dotnet/roslyn

## Filtres d'exceptions
<!-- language-all: lang-cs -->
[Les filtres d'exception][1] donnent aux développeurs la possibilité d'ajouter une condition (sous la forme d'une expression "booléenne") à un bloc [catch][2], permettant au "catch" de s'exécuter uniquement si la condition est évaluée à ` vrai'.

Les filtres d'exception permettent la propagation des informations de débogage dans l'exception d'origine, alors que l'utilisation d'une instruction "if" dans un bloc "catch" et la relance de l'exception arrêtent la propagation des informations de débogage dans l'exception d'origine. Avec les filtres d'exception, l'exception continue de se propager vers le haut dans la pile des appels *sauf si* la condition est remplie. Par conséquent, les filtres d'exception facilitent grandement l'expérience de débogage. Au lieu de s'arrêter sur l'instruction `throw`, le débogueur s'arrêtera sur l'instruction lançant l'exception, avec l'état actuel et toutes les variables locales préservées. Les vidages sur incident sont affectés de la même manière.

>Les filtres d'exception sont pris en charge par le [**CLR**][3] depuis le début et ils sont accessibles depuis VB.NET et F# depuis plus d'une décennie en exposant une partie du modèle de gestion des exceptions du CLR. Ce n'est qu'après la sortie de C# 6.0 que la fonctionnalité a également été disponible pour les développeurs C#.
---

Utiliser des filtres d'exception
-

Les filtres d'exception sont utilisés en ajoutant une clause "when" à l'expression "catch". Il est possible d'utiliser n'importe quelle expression renvoyant un `bool` dans une clause `when` (sauf [await][4]). La variable d'exception déclarée `ex` est accessible depuis la clause `when` :

    var SqlErrorToIgnore = 123;
    try
    {
        DoSQLOperations();
    }
    catch (SqlException ex) when (ex.Number != SqlErrorToIgnore)
    {
        throw new Exception("An error occurred accessing the database", ex);
    }

Plusieurs blocs `catch` avec des clauses `when` peuvent être combinés. La première clause "when" renvoyant "true" entraînera l'interception de l'exception. Son bloc `catch` sera entré, tandis que les autres clauses `catch` seront ignorées (leurs clauses `when` ne seront pas évaluées). Par exemple:

    try
    { ... }
    catch (Exception ex) when (someCondition) //If someCondition evaluates to true,
                                              //the rest of the catches are ignored.
    { ... }
    catch (NotImplementedException ex) when (someMethod()) //someMethod() will only run if
                                                           //someCondition evaluates to false
    { ... }
    catch(Exception ex) // If both when clauses evaluate to false
    { ... }

---
Clause quand risquée
-

>**Attention**
>
>Il peut être risqué d'utiliser des filtres d'exception : lorsqu'une `Exception` est lancée depuis la clause `when`, l'`Exception` de la clause `when` est ignorée et est traitée comme `false`. Cette approche permet aux développeurs d'écrire une clause "quand" sans prendre en compte les cas non valides.

L'exemple suivant illustre un tel scénario :

    public static void Main()
    {
        int a = 7;
        int b = 0;
        try
        {
            DoSomethingThatMightFail();
        }
        catch (Exception ex) when (a / b == 0)
        {
            // This block is never reached because a / b throws an ignored
            // DivideByZeroException which is treated as false.
        }
        catch (Exception ex)
        {
            // This block is reached since the DivideByZeroException in the 
            // previous when clause is ignored.
        }
    }

    public static void DoSomethingThatMightFail()
    {
        // This will always throw an ArgumentNullException.
        Type.GetType(null);
    }

[Voir la démo][5]

Notez que les filtres d'exception évitent les problèmes déroutants de numéro de ligne associés à l'utilisation de `throw` lorsque le code défaillant se trouve dans la même fonction. Par exemple, dans ce cas, le numéro de ligne est signalé comme 6 au lieu de 3 :

    1. int a = 0, b = 0;
    2. try {
    3.     int c = a / b;
    4. }
    5. catch (DivideByZeroException) {
    6.     throw;
    7. }

Le numéro de ligne d'exception est signalé comme 6 car l'erreur a été détectée et renvoyée avec l'instruction "throw" à la ligne 6.

La même chose ne se produit pas avec les filtres d'exception :

    1. int a = 0, b = 0;
    2. try {
    3.     int c = a / b;
    4. }
    5. catch (DivideByZeroException) when (a != 0) {
    6.     throw;
    7. }

Dans cet exemple, "a" vaut 0, la clause "catch" est ignorée, mais 3 est signalé comme numéro de ligne. C'est parce qu'ils **ne déroulent pas la pile**. Plus précisément, l'exception *n'est pas interceptée* à la ligne 5 car "a" est en fait égal à "0" et il n'y a donc aucune possibilité que l'exception soit renvoyée à la ligne 6 car la ligne 6 ne s'exécute pas.

---

La journalisation comme effet secondaire
-

Les appels de méthode dans la condition peuvent provoquer des effets secondaires, de sorte que les filtres d'exception peuvent être utilisés pour exécuter du code sur des exceptions sans les intercepter. Un exemple courant qui en tire parti est une méthode `Log` qui renvoie toujours `false`. Cela permet de suivre les informations du journal pendant le débogage sans qu'il soit nécessaire de relancer l'exception.

>**Sachez que** bien que cela semble être une méthode de journalisation confortable, cela peut être risqué, surtout si des assemblages de journalisation tiers sont utilisés. Celles-ci peuvent générer des exceptions lors de la connexion dans des situations non évidentes qui peuvent ne pas être détectées facilement (voir la clause **Risky `when(...)`** ci-dessus).

<pre><code>essayer
{
    DoSomethingThatMightFail(s);
}
catch (Exception ex) <b>quand</b> (Log(ex, "Une erreur s'est produite"))
{
    // This catch block will never be reached
}

// ...

static bool Log(Exception ex, string message, params object[] args)
{
    Debug.Print(message, args);
    return false;
}</code></pre>

[Voir la démo][6]

L'approche courante dans les versions précédentes de C# consistait à consigner et à relancer l'exception.

<!-- si version [lt 6.0] -->
    try
    {
        DoSomethingThatMightFail(s);
    }
    catch (Exception ex)
    {
         Log(ex, "An error occurred");
         throw;
    }

    // ...

    static void Log(Exception ex, string message, params object[] args)
    {
        Debug.Print(message, args);
    }

[Voir la démo][7]
<!-- fin de version si -->

---

Le bloc "enfin"
=

Le bloc [`finally`][8] s'exécute à chaque fois, que l'exception soit levée ou non. Une subtilité avec les expressions dans `quand` est les filtres d'exception sont exécutés plus haut dans la pile *avant* d'entrer dans les blocs `finally` internes. Cela peut entraîner des résultats et des comportements inattendus lorsque le code tente de modifier l'état global (comme l'utilisateur ou la culture du thread actuel) et de le remettre dans un bloc "finally".

Exemple : bloc "finally"
-

    private static bool Flag = false;

    static void Main(string[] args)
    {
        Console.WriteLine("Start");
        try
        {
            SomeOperation();
        }
        catch (Exception) when (EvaluatesTo())
        {
            Console.WriteLine("Catch");
        }
        finally
        {
            Console.WriteLine("Outer Finally");
        }
    }

    private static bool EvaluatesTo()
    {
        Console.WriteLine($"EvaluatesTo: {Flag}");
        return true;
    }

    private static void SomeOperation()
    {
        try
        {
            Flag = true;
            throw new Exception("Boom");
        }
        finally
        {
            Flag = false;
            Console.WriteLine("Inner Finally");
        }
    }

Sortie produite :

>Démarrer
Évalue jusqu'à : Vrai
Intérieur Enfin
Attraper
Extérieur Enfin

[Voir la démo][9]

Dans l'exemple ci-dessus, si la méthode `SomeOperation` ne souhaite pas "fuir" les changements d'état globaux vers les clauses `when` de l'appelant, elle doit également contenir un bloc `catch` pour modifier l'état. Par exemple:

    private static void SomeOperation()
    {
        try
        {
            Flag = true;
            throw new Exception("Boom");
        }
        catch
        {
           Flag = false;
           throw;
        }
        finally
        {
            Flag = false;
            Console.WriteLine("Inner Finally");
        }
    }

Il est également courant de voir des classes d'assistance [`IDisposable`][10] tirer parti de la sémantique des blocs [using][11] pour atteindre le même objectif, car `IDisposable.Dispose` sera toujours appelé avant une exception appelée dans un ` using` block commence à bouillonner dans la pile.


[1] : https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6#exception-filters
[2] : https://www.wikiod.com/fr/docs/c%23/26/keywords/148/try-catch-finally-throw
[3] : https://msdn.microsoft.com/en-us/library/8bs2ecf4(v=vs.110).aspx
[4] : https://www.wikiod.com/fr/docs/c%23/26/keywords/5993/async-await
[5] : https://dotnetfiddle.net/Iex6DP
[6] : https://dotnetfiddle.net/pqPc7B
[7] : https://dotnetfiddle.net/kEWLue
[8] : https://www.wikiod.com/fr/docs/c%23/40/exception-handling/172/finally-block
[9] : https://ideone.com/gxfBA8
[10] : https://www.wikiod.com/fr/docs/c%23/1795/idisposable-interface
[11] : https://www.wikiod.com/fr/docs/c%23/26/keywords/5062/using

## Interpolation de chaîne
L'interpolation de chaîne permet au développeur de combiner des "variables" et du texte pour former une chaîne.
___

# Exemple de base

Deux variables `int` sont créées : `foo` et `bar`.

    int foo = 34;
    int bar = 42;
    
    string resultString = $"The foo is {foo}, and the bar is {bar}.";

    Console.WriteLine(resultString);

**Production**:
> Le foo est 34, et la barre est 42.

[Voir la démo][3]

Les accolades dans les chaînes peuvent toujours être utilisées, comme ceci :
<pre><code>var foo = 34 ;
où barre = 42 ;

// Notation d'interpolation de chaîne (nouveau style)
Console.WriteLine($"Le foo est <b>{{foo}}</b>, et la barre est <b>{{bar}}</b>.");</code></pre>

Cela produit la sortie suivante :

> Le foo est {foo}, et la barre est {bar}.

___

# Utilisation de l'interpolation avec des littéraux de chaîne textuels

L'utilisation de `@` avant la chaîne entraînera l'interprétation textuelle de la chaîne. Ainsi, par ex. Les caractères Unicode ou les sauts de ligne resteront exactement tels qu'ils ont été saisis. Cependant, cela n'affectera pas les expressions dans une chaîne interpolée comme illustré dans l'exemple suivant :<pre><code>Console.WriteLine($@"Au cas où ce n'était pas clair :
\u00B9
Le fou
est <b>{foo}</b>,
et la barre
est <b>{bar}</b>.");</code></pre>
Production:
> Au cas où ce n'était pas clair :
\u00B9
Le fou
a 34 ans,
et la barre
a 42 ans.

[Voir la démo][4]

___

# Expressions
Avec l'interpolation de chaîne, les *expressions* entre accolades `{}` peuvent également être évaluées. Le résultat sera inséré à l'emplacement correspondant dans la chaîne. Par exemple, pour calculer le maximum de `foo` et `bar` et l'insérer, utilisez `Math.Max` entre les accolades :<pre><code>Console.WriteLine($"Et le plus grand est : <b >{ Math.Max(foo, bar) }</b>");</code></pre>

Production:

>Et le plus grand est : 42

*Remarque : tout espace blanc de début ou de fin (y compris l'espace, la tabulation et le CRLF/nouvelle ligne) entre l'accolade et l'expression est complètement ignoré et n'est pas inclus dans la sortie*

[Voir la démo][5]

Autre exemple, les variables peuvent être formatées en tant que devise :<pre><code>Console.WriteLine($"Foo mis en forme en tant que devise à 4 décimales : <b>{foo:c4}</b>");< /code></pre>

Production:

>Foo formaté comme une devise à 4 décimales : 34,0000 $

[Voir la démo][6]

Ou ils peuvent être formatés en tant que dates :<pre><code>Console.WriteLine($"Today is : <b>{DateTime.Today:dddd, MMMM dd - yyyy}</b>");</code>< /pré>

Production:

>Aujourd'hui c'est : lundi 20 juillet 2015

[Voir la démo][7]

Les instructions avec un [opérateur conditionnel (ternaire)][8] peuvent également être évaluées dans l'interpolation. Cependant, ceux-ci doivent être entourés de parenthèses, car les deux-points sont autrement utilisés pour indiquer le formatage comme indiqué ci-dessus :

<pre><code>Console.WriteLine($"{(foo > bar ? "Foo est plus grand que bar !" : "Bar est plus grand que foo!")}");</code></pre>

Production:
> Le bar est plus grand que foo !

[Voir la démo][9]

Les expressions conditionnelles et les spécificateurs de format peuvent être mélangés :

    Console.WriteLine($"Environment: {(Environment.Is64BitProcess ? 64 : 32):00'-bit'} process");

Production:

> Environnement : processus 32 bits

___

# Séquences d'échappement
L'échappement des caractères barre oblique inverse (`\`) et guillemet (`"`) fonctionne exactement de la même manière dans les chaînes interpolées que dans les chaînes non interpolées, pour les littéraux de chaîne textuels et non textuels :
<pre><code>Console.WriteLine($"Foo est : <b>{foo}</b>. Dans une chaîne non verbatim, nous devons échapper \" et \\ avec des barres obliques inverses.");
Console.WriteLine($@"Foo est : <b>{foo}</b>. Dans une chaîne verbatim, nous devons échapper "" avec un guillemet supplémentaire, mais nous n'avons pas besoin d'échapper \");
</code></pre>

Production:
>Foo vaut 34. Dans une chaîne non verbatim, nous devons échapper " et \ avec des barres obliques inverses.
Foo vaut 34. Dans une chaîne verbatim, nous devons échapper " avec un guillemet supplémentaire, mais nous n'avons pas besoin d'échapper \

Pour inclure une accolade `{` ou `}` dans une chaîne interpolée, utilisez deux accolades `{{` ou `}}` :<pre><code>$"{{foo}} est : <b>{ foo}</b>"</code></pre>

Production:
>{foo} est : 34

[Voir la démo][10]
___

# Type de chaîne formattable
Le type d'une expression d'interpolation de chaîne `$"..."` [n'est pas toujours][11] une chaîne simple. Le compilateur décide du type à affecter en fonction du contexte :<pre><code>string s = $"hello, <b>{name}</b>" ;
System.FormatableString s = $"Bonjour, <b>{nom}</b>" ;
System.IFormattable s = $"Bonjour, <b>{name}</b>";</code></pre>

C'est également l'ordre de préférence de type lorsque le compilateur doit choisir quelle méthode surchargée va être appelée.

Un [nouveau type][12], `System.FormatableString`, représente une chaîne de format composite, ainsi que les arguments à formater. Utilisez ceci pour écrire des applications qui gèrent spécifiquement les arguments d'interpolation :

    public void AddLogItem(FormattableString formattableString)
    {
        foreach (var arg in formattableString.GetArguments())
        {
            // do something to interpolation argument 'arg'
        }

        // use the standard interpolation and the current culture info
        // to get an ordinary String:
        var formatted = formattableString.ToString();

        // ...
    }
Appelez la méthode ci-dessus avec :<pre><code>AddLogItem($"Le foo est <b>{foo}</b>, et la barre est <b>{bar}</b>.");</ code></pre>
Par exemple, on pourrait choisir de ne pas encourir le coût de performance lié au formatage de la chaîne si le niveau de journalisation allait déjà filtrer l'élément de journal.<hr>
# Conversions implicites
Il existe des conversions de type implicites à partir d'une chaîne interpolée :<pre><code>var s = $"Foo : <b>{foo}</b>" ;
System.IFormattable s = $"Foo : <b>{foo}</b>" ;</code></pre>
Vous pouvez également produire une variable `IFormattable` qui vous permet de convertir la chaîne avec un contexte invariant :<pre><code>var s = $"Bar : <b>{bar}</b>" ;
System.FormatableString s = $"Bar : <b>{bar}</b>" ;</code></pre><hr>
# Méthodes de culture actuelles et invariantes
Si l'analyse du code est activée, les chaînes interpolées produiront toutes un avertissement [CA1305][13] (spécifiez `IFormatProvider`).
Une méthode statique peut être utilisée pour appliquer la culture actuelle.

    public static class Culture
    {
        public static string Current(FormattableString formattableString)
        {
            return formattableString?.ToString(CultureInfo.CurrentCulture);
        }
        public static string Invariant(FormattableString formattableString)
        {
            return formattableString?.ToString(CultureInfo.InvariantCulture);
        }
    }
Ensuite, pour produire une chaîne correcte pour la culture actuelle, utilisez simplement l'expression :<pre><code>Culture.Current($"interpolated <b>{typeof(string).Name}</b> string.")
Culture.Invariant($"chaîne <b>{typeof(string).Name}</b> interpolée.")</code></pre>
**Remarque** : `Current` et `Invariant` ne peuvent pas être créés en tant que méthodes d'extension car, par défaut, le compilateur attribue le type `String` à *expression de chaîne interpolée*, ce qui entraîne l'échec de la compilation du code suivant :

    $"interpolated {typeof(string).Name} string.".Current();
La classe `FormattableString` contient déjà la méthode `Invariant()`, donc le moyen le plus simple de passer à la culture invariante est de s'appuyer sur `using static`:<pre><code>using static System.FormattableString;

chaîne invariant = Invariant($"Now = <b>{DateTime.Now}</b>");
chaîne actuelle = $"Maintenant = <b>{DateHeure.Maintenant}</b>" ;</code></pre><hr>
# Dans les coulisses
Les chaînes interpolées ne sont qu'un sucre syntaxique pour `String.Format()`. Le compilateur ([Roslyn][14]) le transformera en `String.Format` dans les coulisses :

    var text = $"Hello {name + lastName}";
    
Ce qui précède sera converti en quelque chose comme ceci :

    string text = string.Format("Hello {0}", new object[] {
        name + lastName
    });
<h>

# Interpolation de chaînes et Linq

Il est possible d'utiliser des chaînes interpolées dans les instructions Linq pour augmenter encore la lisibilité.

    var fooBar = (from DataRow x in fooBarTable.Rows
              select string.Format("{0}{1}", x["foo"], x["bar"])).ToList();

Peut être réécrit comme suit :

    var fooBar = (from DataRow x in fooBarTable.Rows
              select $"{x["foo"]}{x["bar"]}").ToList();

# Chaînes interpolées réutilisables
Avec `string.Format`, vous pouvez créer des chaînes de format réutilisables :

    public const string ErrorFormat = "Exception caught:\r\n{0}";

    // ...

    Logger.Log(string.Format(ErrorFormat, ex));

Les chaînes interpolées, cependant, ne seront pas compilées avec des espaces réservés faisant référence à des variables inexistantes. Ce qui suit ne compilera pas :

    public const string ErrorFormat = $"Exception caught:\r\n{error}";
    // CS0103: The name 'error' does not exist in the current context

Au lieu de cela, créez un `Func<>` qui consomme des variables et renvoie une `String` :

    public static Func<Exception, string> FormatError =
        error => $"Exception caught:\r\n{error}";

    // ...

    Logger.Log(FormatError(ex));
<h>

# Interpolation et localisation de chaînes

Si vous localisez votre application, vous vous demandez peut-être s'il est possible d'utiliser l'interpolation de chaîne avec la localisation. En effet, ce serait bien d'avoir la possibilité de stocker dans des fichiers de ressources `String`s comme :<pre><code>"Mon nom est <b>{name} {middlename} {surname}</b>"</ code></pre>
au lieu du bien moins lisible :

    "My name is {0} {1} {2}"
Le processus d'interpolation `String` se produit *au moment de la compilation*, contrairement au formatage de chaîne avec `string.Format` qui se produit *au moment de l'exécution*. Les expressions dans une chaîne interpolée doivent faire référence à des noms dans le contexte actuel et doivent être stockées dans des fichiers de ressources. Cela signifie que si vous souhaitez utiliser la localisation, vous devez le faire comme suit :

    var FirstName = "John";
    
    // method using different resource file "strings"
    // for French ("strings.fr.resx"), German ("strings.de.resx"), 
    // and English ("strings.en.resx")
    void ShowMyNameLocalized(string name, string middlename = "", string surname = "")
    {
        // get localized string
        var localizedMyNameIs = Properties.strings.Hello;
        // insert spaces where necessary
        name = (string.IsNullOrWhiteSpace(name) ? "" : name + " ");
        middlename = (string.IsNullOrWhiteSpace(middlename) ? "" : middlename + " ");
        surname = (string.IsNullOrWhiteSpace(surname) ? "" : surname + " ");
        // display it
        Console.WriteLine($"{localizedMyNameIs} {name}{middlename}{surname}".Trim());
    }

    // switch to French and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("fr-FR");
    ShowMyNameLocalized(FirstName);

    // switch to German and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("de-DE");
    ShowMyNameLocalized(FirstName);

    // switch to US English and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("en-US");
    ShowMyNameLocalized(FirstName);

Si les chaînes de ressources pour les langues utilisées ci-dessus sont correctement stockées dans les fichiers de ressources individuels, vous devriez obtenir le résultat suivant :
> Bonjour, mon nom est John<br/>
> Bonjour, je m'appelle Jean<br/>
> Bonjour, je m'appelle Jean<br/>

**Notez** que cela implique que le nom suit la chaîne localisée dans chaque langue. Si ce n'est pas le cas, vous devez ajouter des espaces réservés aux chaînes de ressources et modifier la fonction ci-dessus ou vous devez interroger les informations de culture dans la fonction et fournir une instruction switch case contenant les différents cas.
Pour plus de détails sur les fichiers de ressources, consultez [Comment utiliser la localisation en C#](https://stackoverflow.com/a/1142840/1016343).

Il est recommandé d'utiliser une langue de secours par défaut que la plupart des gens comprendront, au cas où une traduction ne serait pas disponible. Je suggère d'utiliser l'anglais comme langue de repli par défaut.

# Interpolation récursive

Bien que cela ne soit pas très utile, il est permis d'utiliser une "chaîne" interpolée de manière récursive à l'intérieur des accolades d'une autre :

    Console.WriteLine($"String has {$"My class is called {nameof(MyClass)}.".Length} chars:");
    Console.WriteLine($"My class is called {nameof(MyClass)}.");

Production:

> La chaîne comporte 27 caractères :

> Ma classe s'appelle MyClass.

[1] : https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6#string-interpolation
[2] : https://dotnetfiddle.net/0JjwL5
[3] : https://ideone.com/bRFOaV
[4] : https://dotnetfiddle.net/FLs4Ae
[5] : https://ideone.com/qY1Y4B
[6] : https://ideone.com/CPB8UJ
[7] : https://ideone.com/PkjA6k
[8] : https://msdn.microsoft.com/en-us/library/ty67wk28.aspx
[9] : https://ideone.com/sX6tO3
[10] : https://dotnetfiddle.net/BuudHP
[11] : http://stackoverflow.com/questions/38119074
[12] : https://msdn.microsoft.com/en-us/library/system.formattablestring(v=vs.110).aspx
[13] : https://msdn.microsoft.com/en-us/library/ms182190.aspx
[14] : https://github.com/dotnet/roslyn


## Initialiseurs de propriétés automatiques
# Introduction

Les propriétés peuvent être initialisées avec l'opérateur `=` après le `}` de fermeture. La classe `Coordinate` ci-dessous montre les options disponibles pour initialiser une propriété :


<!-- si version [gte 6.0] -->
    public class Coordinate
    { 
        public int X { get; set; } = 34; // get or set auto-property with initializer
   
        public int Y { get; } = 89;      // read-only auto-property with initializer              
    }
<!-- fin de version si -->

---

## Accesseurs avec une visibilité différente

Vous pouvez initialiser des propriétés automatiques qui ont une visibilité différente sur leurs accesseurs. Voici un exemple avec un setter protégé :

        public string Name { get; protected set; } = "Cheeze";

L'accesseur peut également être "interne", "interne protégé" ou "privé".

---

## Propriétés en lecture seule

Outre la flexibilité de la visibilité, vous pouvez également initialiser les propriétés automatiques en lecture seule. Voici un exemple :

        public List<string> Ingredients { get; } = 
            new List<string> { "dough", "sauce", "cheese" };

Cet exemple montre également comment initialiser une propriété avec un type complexe. De plus, les propriétés automatiques ne peuvent pas être en écriture seule, ce qui empêche également l'initialisation en écriture seule.

---

# Ancien style (avant C# 6.0)

Avant C# 6, cela nécessitait un code beaucoup plus verbeux. Nous utilisions une variable supplémentaire appelée propriété de support pour que la propriété donne une valeur par défaut ou pour initialiser la propriété publique comme ci-dessous,

<!-- si version [lt 6.0] -->
    public class Coordinate
    {
        private int _x = 34;
        public int X { get { return _x; } set { _x = value; } }
   
        private readonly int _y = 89;
        public int Y { get { return _y; } }
        
        private readonly int _z;
        public int Z { get { return _z; } }
    
        public Coordinate()
        {
            _z = 42;
        }
    }

***Remarque :** Avant C# 6.0, vous pouviez toujours initialiser les [**propriétés implémentées automatiquement**][2] (propriétés avec un getter et un setter) en lecture et en écriture depuis le constructeur, mais vous ne pouviez pas initialiser le bien conforme à sa déclaration*

[Voir la démo][3]
<!-- fin de version si -->

---

# Utilisation

Les initialiseurs doivent évaluer des expressions statiques, tout comme les initialiseurs de champ. Si vous avez besoin de référencer des membres non statiques, vous pouvez soit initialiser des propriétés dans des constructeurs comme avant, soit utiliser des propriétés à corps d'expression. Les expressions non statiques, comme celle ci-dessous (commentée), généreront une erreur de compilation :

    
    // public decimal X { get; set; } = InitMe();  // generates compiler error

    decimal InitMe() { return 4m; }

Mais les méthodes statiques **peuvent** être utilisées pour initialiser les propriétés automatiques :

    public class Rectangle
    {
        public double Length { get; set; } = 1;
        public double Width { get; set; } = 1;
        public double Area { get; set; } = CalculateArea(1, 1);

        public static double CalculateArea(double length, double width)
        {
            return length * width;
        }
    }

Cette méthode peut également être appliquée aux propriétés avec différents niveaux d'accesseurs :

    public short Type { get; private set; } = 15;

L'initialiseur de propriété automatique permet l'affectation de propriétés directement dans leur déclaration. Pour les propriétés en lecture seule, il prend en charge toutes les exigences requises pour garantir que la propriété est immuable. Considérez, par exemple, la classe `FingerPrint` dans l'exemple suivant :

    public class FingerPrint
    {
      public DateTime TimeStamp { get; } = DateTime.UtcNow;

      public string User { get; } =
        System.Security.Principal.WindowsPrincipal.Current.Identity.Name;

      public string Process { get; } =
        System.Diagnostics.Process.GetCurrentProcess().ProcessName;
    }

[Voir la démo][4]

---

# Notes de mise en garde

Veillez à ne pas confondre les initialiseurs de propriétés automatiques ou de champs avec des [méthodes de corps d'expression][5] d'aspect similaire qui utilisent `=>` par opposition à `=`, et des champs qui n'incluent pas `{ get; }`.

Par exemple, chacune des déclarations suivantes est différente.

    public class UserGroupDto
    {
        // Read-only auto-property with initializer:       
        public ICollection<UserDto> Users1 { get; } = new HashSet<UserDto>();
        
        // Read-write field with initializer:
        public ICollection<UserDto> Users2 = new HashSet<UserDto>();

        // Read-only auto-property with expression body:
        public ICollection<UserDto> Users3 => new HashSet<UserDto>();
    }

`{ obtenir ; }` dans la déclaration de propriété donne un champ public. La propriété automatique en lecture seule "Users1" et le champ en lecture-écriture "Users2" ne sont initialisés qu'une seule fois, mais un champ public permet de modifier l'instance de collection depuis l'extérieur de la classe, ce qui n'est généralement pas souhaitable. Changer une propriété automatique en lecture seule avec corps d'expression en propriété en lecture seule avec initialiseur nécessite non seulement de supprimer `>` de `=>`, mais d'ajouter `{ get; }`.

Le symbole différent (`=>` au lieu de `=`) dans `Users3` fait que chaque accès à la propriété renvoie une nouvelle instance de `HashSet<UserDto>` qui, tout en étant valide C# (du point de vue du compilateur) est peu susceptible d'être le comportement souhaité lorsqu'il est utilisé pour un membre de la collection.

Le code ci-dessus est équivalent à :

    public class UserGroupDto
    {
        // This is a property returning the same instance
        // which was created when the UserGroupDto was instantiated.
        private ICollection<UserDto> _users1 = new HashSet<UserDto>();
        public ICollection<UserDto> Users1 { get { return _users1; } }

        // This is a field returning the same instance
        // which was created when the UserGroupDto was instantiated.
        public virtual ICollection<UserDto> Users2 = new HashSet<UserDto>();

        // This is a property which returns a new HashSet<UserDto> as
        // an ICollection<UserDto> on each call to it.
        public ICollection<UserDto> Users3 { get { return new HashSet<UserDto>(); } }
    }


[2] : https://www.wikiod.com/fr/docs/c%23/49/properties/3365/auto-implemented-properties#t=201608062134378589394
[3] : http://ideone.com/2OgrPQ
[4] : http://ideone.com/qjDRmx
[5] : https://www.wikiod.com/fr/docs/c%23/24/c-sharp-6-0-features/44/expression-bodied-function-members

## Propagation nulle
L'opérateur `?.` et l'opérateur `?[...]` sont appelés [opérateur conditionnel nul][1]. Il est aussi parfois désigné par d'autres noms tels que [opérateur de navigation sûre] [2].

Ceci est utile, car si l'opérateur `.` (accesseur de membre) est appliqué à une expression évaluée à `null`, le programme lèvera une `NullReferenceException`. Si le développeur utilise à la place l'opérateur `?.` (null-conditionnel), l'expression sera évaluée à null au lieu de lever une exception.

Notez que si l'opérateur `?.` est utilisé et que l'expression n'est pas nulle, `?.` et `.` sont équivalents.

---

# Bases

    var teacherName = classroom.GetTeacher().Name;
    // throws NullReferenceException if GetTeacher() returns null

[Voir la démo][3]

Si la `salle de classe` n'a pas d'enseignant, `GetTeacher()` peut renvoyer `null`. Lorsqu'il est `null` et que la propriété `Name` est accessible, une `NullReferenceException` sera levée.

Si nous modifions cette instruction pour utiliser la syntaxe `?.`, le résultat de l'expression entière sera `null` :

    var teacherName = classroom.GetTeacher()?.Name;
    // teacherName is null if GetTeacher() returns null

[Voir la démo][4]

Par la suite, si `classroom` pouvait également être `null`, nous pourrions également écrire cette déclaration comme suit :

    var teacherName = classroom?.GetTeacher()?.Name;
    // teacherName is null if GetTeacher() returns null OR classroom is null

[Voir la démo][5]

Voici un exemple de court-circuit : lorsqu'une opération d'accès conditionnel utilisant l'opérateur conditionnel null est évaluée à null, l'expression entière est évaluée à null immédiatement, sans traiter le reste de la chaîne.

Lorsque le membre terminal d'une expression contenant l'opérateur conditionnel nul est d'un type valeur, l'expression est évaluée à un `Nullable<T>` de ce type et ne peut donc pas être utilisée comme remplacement direct de l'expression sans `?.` .

    bool hasCertification = classroom.GetTeacher().HasCertification;
    // compiles without error but may throw a NullReferenceException at runtime

    bool hasCertification = classroom?.GetTeacher()?.HasCertification;
    // compile time error: implicit conversion from bool? to bool not allowed

    bool? hasCertification = classroom?.GetTeacher()?.HasCertification;
    // works just fine, hasCertification will be null if any part of the chain is null

    bool hasCertification = classroom?.GetTeacher()?.HasCertification.GetValueOrDefault();
    // must extract value from nullable to assign to a value type variable

---

# Utiliser avec l'opérateur Null-Coalescing (??)

Vous pouvez combiner l'opérateur conditionnel nul avec l'[opérateur de coalescence nulle][6] (`??`) pour renvoyer une valeur par défaut si l'expression se résout en `null`. En utilisant notre exemple ci-dessus :

    var teacherName = classroom?.GetTeacher()?.Name ?? "No Name";
    // teacherName will be "No Name" when GetTeacher() 
    // returns null OR classroom is null OR Name is null

---

# Utiliser avec les indexeurs

L'opérateur conditionnel nul peut être utilisé avec [indexers][7] :

    var firstStudentName = classroom?.Students?[0]?.Name;

Dans l'exemple ci-dessus :

* Le premier `?.` garantit que `classroom` n'est pas `null`.
* Le deuxième `?` garantit que la collection entière `Students` n'est pas `null`.
* Le troisième `?.` après l'indexeur garantit que l'indexeur `[0]` n'a pas renvoyé d'objet `null`. Il convient de noter que cette opération peut **toujours** lever une `IndexOutOfRangeException`.

---

# Utiliser avec les fonctions void

L'opérateur conditionnel nul peut également être utilisé avec les fonctions "void". Cependant, dans ce cas, l'instruction ne sera pas évaluée à "null". Cela empêchera simplement une `NullReferenceException`.

    List<string> list = null;
    list?.Add("hi");          // Does not evaluate to null


---

# Utiliser avec l'invocation d'événement

En supposant la définition d'événement suivante :

    private event EventArgs OnCompleted;

Lors de l'appel d'un événement, traditionnellement, il est préférable de vérifier si l'événement est "null" au cas où aucun abonné n'est présent :

    var handler = OnCompleted;
    if (handler != null)
    {
        handler(EventArgs.Empty);
    }

Depuis l'introduction de l'opérateur conditionnel nul, l'invocation peut être réduite à une seule ligne :

    OnCompleted?.Invoke(EventArgs.Empty);

---

# Limites

L'opérateur conditionnel nul produit rvalue, pas lvalue, c'est-à-dire qu'il ne peut pas être utilisé pour l'attribution de propriétés, l'abonnement à des événements, etc. Par exemple, le code suivant ne fonctionnera pas :

    // Error: The left-hand side of an assignment must be a variable, property or indexer
    Process.GetProcessById(1337)?.EnableRaisingEvents = true;
    // Error: The event can only appear on the left hand side of += or -=
    Process.GetProcessById(1337)?.Exited += OnProcessExited;

---

# Pièges

Notez que:

    int? nameLength = person?.Name.Length;    // safe if 'person' is null

n'est __pas__ identique à :

    int? nameLength = (person?.Name).Length;  // avoid this

car le premier correspond à :

    int? nameLength = person != null ? (int?)person.Name.Length : null;

et ce dernier correspond à :

    int? nameLength = (person != null ? person.Name : null).Length;

Bien que l'opérateur ternaire `?:` soit utilisé ici pour expliquer la différence entre deux cas, ces opérateurs ne sont pas équivalents. Cela peut être facilement démontré avec l'exemple suivant :

    void Main()
    {
        var foo = new Foo();
        Console.WriteLine("Null propagation");
        Console.WriteLine(foo.Bar?.Length);

        Console.WriteLine("Ternary");
        Console.WriteLine(foo.Bar != null ? foo.Bar.Length : (int?)null);
    }
    
    class Foo
    {
        public string Bar
        {
            get
            {
                Console.WriteLine("I was read");
                return string.Empty;
            }
        }
    }

Qui sort :

> Propagation nulle
> j'ai été lu
>0
>Ternaire
> j'ai été lu
> j'ai été lu
>0

[Voir la démo][8]

Eviter plusieurs invocations équivalentes serait :

    var interimResult = foo.Bar;
    Console.WriteLine(interimResult != null ? interimResult.Length : (int?)null);

Et cette différence explique quelque peu pourquoi l'opérateur de propagation nul n'est [pas encore pris en charge] [9] dans les arbres d'expression.


[1] : https://msdn.microsoft.com/en-us/library/dn986595.aspx
[2] : https://en.wikipedia.org/wiki/Safe_navigation_operator
[3] : http://ideone.com/p8OGBB
[4] : http://ideone.com/3aqGlE
[5] : http://ideone.com/voljZh
[6] : https://msdn.microsoft.com/en-us/library/ms173224.aspx
[7] : https://msdn.microsoft.com/en-us/library/6x16t2tx.aspx
[8] : https://dotnetfiddle.net/BytXEz
[9] : https://roslyn.codeplex.com/discussions/571077


## Membres de fonction à corps d'expression
Les membres de fonction à corps d'expression permettent l'utilisation d'expressions lambda en tant que corps membres. Pour les membres simples, cela peut se traduire par un code plus propre et plus lisible.

Les fonctions à corps d'expression peuvent être utilisées pour les propriétés, les indexeurs, les méthodes et les opérateurs.

---

# Propriétés

    public decimal TotalPrice => BasePrice + Taxes;

Est équivalent à:

    public decimal TotalPrice
    {
        get
        {
            return BasePrice + Taxes;
        }
    }

Lorsqu'une fonction à corps d'expression est utilisée avec une propriété, la propriété est implémentée en tant que propriété getter uniquement.

[Voir la démo][1]

---

# Indexeurs

    public object this[string key] => dictionary[key];

Est équivalent à:

    public object this[string key]
    {
        get
        {
            return dictionary[key];
        }
    }

---

# Méthodes

    static int Multiply(int a, int b) => a * b;

Est équivalent à:

    static int Multiply(int a, int b)
    {
        return a * b;
    }

Qui peut également être utilisé avec les méthodes `void` :

    public void Dispose() => resource?.Dispose();

Un remplacement de `ToString` pourrait être ajouté à la classe `Pair<T>` :

    public override string ToString() => $"{First}, {Second}";

De plus, cette approche simpliste fonctionne avec le mot-clé `override` :

    public class Foo
    {
        public int Bar { get; }
    
        public string override ToString() => $"Bar: {Bar}";
    }

---

# Les opérateurs

Cela peut également être utilisé par les opérateurs :

    public class Land
    {
        public double Area { get; set; }

        public static Land operator +(Land first, Land second) =>
            new Land { Area = first.Area + second.Area };
    }

---

# Limites

Les membres de fonction à corps d'expression ont certaines limitations. Ils ne peuvent pas contenir d'instructions de bloc ni aucune autre instruction contenant des blocs : `if`, `switch`, `for`, `foreach`, `while`, `do`, `try`, etc.

Certaines instructions "if" peuvent être remplacées par des opérateurs ternaires. Certaines instructions `for` et `foreach` peuvent être converties en requêtes LINQ, par exemple :

    IEnumerable<string> Digits
    {
        get
        {
            for (int i = 0; i < 10; i++)
                yield return i.ToString();
        }
    }

<!---->

    IEnumerable<string> Digits => Enumerable.Range(0, 10).Select(i => i.ToString());

Dans tous les autres cas, l'ancienne syntaxe des membres de fonction peut être utilisée.

Les membres de fonction à corps d'expression peuvent contenir `async`/`wait`, mais c'est souvent redondant :

    async Task<int> Foo() => await Bar();  

Peut être remplacé par :

    Task<int> Foo() => Bar();

[1] : https://dotnetfiddle.net/djFd7O


## Nom de l'opérateur
L'opérateur `nameof` renvoie le nom d'un élément de code sous la forme d'une `string`. Ceci est utile lors de la levée d'exceptions liées aux arguments de méthode et également lors de l'implémentation de `INotifyPropertyChanged`.

    public string SayHello(string greeted)
    {
        if (greeted == null)
            throw new ArgumentNullException(nameof(greeted));
        
        Console.WriteLine("Hello, " + greeted);
    }

L'opérateur `nameof` est évalué au moment de la compilation et transforme l'expression en une chaîne littérale. Ceci est également utile pour les chaînes nommées d'après leur membre qui les expose. Considérer ce qui suit:

    public static class Strings
    {
        public const string Foo = nameof(Foo); // Rather than Foo = "Foo"
        public const string Bar = nameof(Bar); // Rather than Bar = "Bar"
    }

Comme les expressions `nameof` sont des constantes de compilation, elles peuvent être utilisées dans les attributs, les étiquettes `case`, les instructions `switch`, etc.

<hr/>

Il est pratique d'utiliser `nameof` avec `Enum`s. À la place de:

    Console.WriteLine(Enum.One.ToString());

il est possible d'utiliser :

    Console.WriteLine(nameof(Enum.One))

La sortie sera "Un" dans les deux cas.

<hr/>

L'opérateur `nameof` peut accéder aux membres non statiques en utilisant une syntaxe de type statique. Au lieu de faire :

    string foo = "Foo";
    string lengthName = nameof(foo.Length);

Peut être remplacé par :

    string lengthName = nameof(string.Length);

La sortie sera "Length" dans les deux exemples. Cependant, ce dernier empêche la création d'instances inutiles.

<hr/>

Bien que l'opérateur `nameof` fonctionne avec la plupart des constructions de langage, il existe certaines limitations. Par exemple, vous ne pouvez pas utiliser l'opérateur `nameof` sur des types génériques ouverts ou des valeurs de retour de méthode :

    public static int Main()
    {   
        Console.WriteLine(nameof(List<>)); // Compile-time error
        Console.WriteLine(nameof(Main())); // Compile-time error
    }

De plus, si vous l'appliquez à un type générique, le paramètre de type générique sera ignoré :

    Console.WriteLine(nameof(List<int>));  // "List"
    Console.WriteLine(nameof(List<bool>)); // "List"

Pour plus d'exemples, voir [ce sujet][1] dédié à `nameof`.

<hr/>

# Solution de contournement pour les versions précédentes ([plus de détails][2])

Bien que l'opérateur `nameof` n'existe pas en C# pour les versions antérieures à 6.0, une fonctionnalité similaire peut être obtenue en utilisant `MemberExpression` comme dans ce qui suit :

<!-- si version [lt 6.0] -->
Expression:

    public static string NameOf<T>(Expression<Func<T>> propExp)
    {
        var memberExpression = propExp.Body as MemberExpression;
        return memberExpression != null ? memberExpression.Member.Name : null;
    }

    public static string NameOf<TObj, T>(Expression<Func<TObj, T>> propExp)
    {
        var memberExpression = propExp.Body as MemberExpression;
        return memberExpression != null ? memberExpression.Member.Name : null;
    }

Usage:

    string variableName = NameOf(() => variable);
    string propertyName = NameOf((Foo o) => o.Bar);

<!-- fin de version si -->

Notez que cette approche entraîne la création d'une arborescence d'expressions à chaque appel, de sorte que les performances sont bien inférieures à celles de l'opérateur `nameof` qui est évalué au moment de la compilation et n'a aucune surcharge au moment de l'exécution.


[1] : https://www.wikiod.com/fr/docs/c%23/80/nameof-operator#t=201608031424500177545
[2] : https://www.wikiod.com/fr/docs/c%23/80/nameof-operator/26157/name-of-extension-support-added-for-before-c-sharp-6-version#t= 201612071107472552734

## Utilisation du type statique
La directive `using static [Namespace.Type]` permet l'importation de membres statiques de types et de valeurs d'énumération. Les méthodes d'extension sont importées en tant que méthodes d'extension (à partir d'un seul type), et non dans la portée de niveau supérieur.

<!-- si version [gte 6.0] -->

    using static System.Console;
    using static System.ConsoleColor;
    using static System.Math;
    
    class Program
    {
        static void Main()
        {
            BackgroundColor = DarkBlue;
            WriteLine(Sqrt(2));
        }
    }

[Violon de démonstration en direct] [1]
<!-- fin de version si -->

<!-- si version [lt 6.0] -->

    using System;
    
    class Program
    {
        static void Main()
        {
            Console.BackgroundColor = ConsoleColor.DarkBlue;
            Console.WriteLine(Math.Sqrt(2));
        }
    }

<!-- fin de version si -->


[1] : https://dotnetfiddle.net/7Ll3XN

## Initialiseurs d'index
Les initialiseurs d'index permettent de créer et d'initialiser des objets avec des index en même temps.

Cela rend l'initialisation des dictionnaires très facile :

    var dict = new Dictionary<string, int>()
    {
        ["foo"] = 34,
        ["bar"] = 42
    };


Tout objet qui a un getter ou un setter indexé peut être utilisé avec cette syntaxe :

    class Program
    {
        public class MyClassWithIndexer
        {
            public int this[string index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
        }

        public static void Main()
        {
            var x = new MyClassWithIndexer()
            {
                ["foo"] = 34,
                ["bar"] = 42
            };

            Console.ReadKey();
        }
    }

Production:
>Indice : foo, valeur : 34
>Indice : barre, valeur : 42


[Voir la démo][1]

Si la classe a plusieurs indexeurs, il est possible de les affecter tous dans un seul groupe d'instructions :

    class Program
    {
        public class MyClassWithIndexer
        {
            public int this[string index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
            public string this[int index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
        }

        public static void Main()
        {
            var x = new MyClassWithIndexer()
            {
                ["foo"] = 34,
                ["bar"] = 42,
                [10] = "Ten",
                [42] = "Meaning of life"
            };
        }
    }

Production:
>Indice : foo, valeur : 34
>Indice : barre, valeur : 42
>Indice : 10, valeur : Dix
>Indice : 42, valeur : Sens de la vie

Il convient de noter que l'accesseur `set` de l'indexeur peut se comporter différemment par rapport à une méthode `Add` (utilisée dans les initialiseurs de collection).

Par exemple:

    var d = new Dictionary<string, int>
    {
        ["foo"] = 34,
        ["foo"] = 42,
    }; // does not throw, second value overwrites the first one

contre:

    var d = new Dictionary<string, int>
    {
        { "foo", 34 },
        { "foo", 42 },
    }; // run-time ArgumentException: An item with the same key has already been added.


[1] : https://dotnetfiddle.net/Evs4Qx

## Résolution de surcharge améliorée
L'extrait de code suivant montre un exemple de transmission d'un groupe de méthodes (par opposition à un lambda) lorsqu'un délégué est attendu. La résolution de surcharge résoudra désormais ce problème au lieu de générer une erreur de surcharge ambiguë en raison de la capacité de **C# 6** à vérifier le type de retour de la méthode qui a été transmise.

    using System;
    public class Program
    {
        public static void Main()
        {
            Overloaded(DoSomething);
        }
    
        static void Overloaded(Action action)
        {
           Console.WriteLine("overload with action called");
        }
    
        static void Overloaded(Func<int> function)
        {
           Console.WriteLine("overload with Func<int> called");
        }
    
        static int DoSomething()
        {
            Console.WriteLine(0);
            return 0;
        }
    }

Résultats:

<!-- si version [éq 6.0] -->
**Production**
> surcharge avec Func\<int\> appelé

[Voir la démo][1]
<!-- fin de version si -->

<!-- si version [éq 5.0] -->
**Erreur**
> erreur CS0121 : L'appel est ambigu entre les méthodes ou propriétés suivantes :
     'Program.Overloaded(System.Action)' and 'Program.Overloaded(System.Func)'

<!-- fin de version si -->

**C# 6** peut également bien gérer le cas suivant de correspondance exacte pour les expressions lambda qui aurait entraîné une erreur dans **C# 5**.

    using System;

    class Program
    {
        static void Foo(Func<Func<long>> func) {}
        static void Foo(Func<Func<int>> func) {}

        static void Main()
        {
            Foo(() => () => 7);
        }
    }


[1] : https://dotnetfiddle.net/Vnudqy

## Attendre dans catch et enfin
Il est possible d'utiliser l'expression `wait` pour appliquer [wait operator][1] à [Tasks][2] ou [Task(Of TResult)][3] dans les blocs `catch` et `finally` en C#6 .

Il n'était pas possible d'utiliser l'expression `wait` dans les blocs `catch` et `finally` dans les versions antérieures en raison des limitations du compilateur. C#6 facilite grandement l'attente des tâches asynchrones en autorisant l'expression "wait".

    try
    {
        //since C#5
        await service.InitializeAsync();
    } 
    catch (Exception e)
    {
        //since C#6
        await logger.LogAsync(e);
    }
    finally
    {
        //since C#6
        await service.CloseAsync();
    }

Il était nécessaire en C # 5 d'utiliser un `bool` ou de déclarer une `Exception` en dehors du try catch pour effectuer des opérations asynchrones. Cette méthode est illustrée dans l'exemple suivant :
    
    bool error = false;
    Exception ex = null;

    try
    {
        // Since C#5
        await service.InitializeAsync();
    } 
    catch (Exception e)
    {
        // Declare bool or place exception inside variable
        error = true;
        ex = e;
    }

    // If you don't use the exception
    if (error)
    {
        // Handle async task
    }

    // If want to use information from the exception
    if (ex != null)
    {
        await logger.LogAsync(e);
    }    

    // Close the service, since this isn't possible in the finally
    await service.CloseAsync();


[1] : https://msdn.microsoft.com/en-us/library/hh156528.aspx
[2] : https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.aspx
[3] : https://msdn.microsoft.com/en-us/library/dd321424.aspx

## Modifications mineures et corrections de bugs
Les parenthèses sont désormais interdites autour des paramètres nommés. Ce qui suit compile en C#5, mais pas en C#6

<!-- si version [lte 5.0] -->

    Console.WriteLine((value: 23));

<!-- fin de version si -->

Les opérandes `is` et `as` ne sont plus autorisés à être des groupes de méthodes. Ce qui suit compile en C#5, mais pas en C#6

<!-- si version [lte 5.0] -->

    var result = "".Any is byte;

> Le compilateur natif a autorisé cela (bien qu'il ait affiché un avertissement), et en fait n'a même pas vérifié la compatibilité de la méthode d'extension, permettant des choses folles comme `1.Any is string` ou `IDisposable.Dispose is object`.

<!-- fin de version si -->

Voir [cette référence][1] pour les mises à jour sur les modifications.


[1] : http://blog.slaks.net/2014-05-28/exploring-roslyn-part-3-breaking-changes/

## Utilisation d'une méthode d'extension pour l'initialisation de la collection
La syntaxe d'initialisation de la collection peut être utilisée lors de l'instanciation de toute classe qui implémente `IEnumerable` et a une méthode nommée `Add` qui prend un seul paramètre.

Dans les versions précédentes, cette méthode `Add` devait être une méthode **instance** sur la classe en cours d'initialisation. En C#6, il peut également s'agir d'une méthode d'extension.

    public class CollectionWithAdd : IEnumerable
    {
        public void Add<T>(T item)
        {
            Console.WriteLine("Item added with instance add method: " + item);
        }

        public IEnumerator GetEnumerator()
        {
            // Some implementation here
        }
    }
    
    public class CollectionWithoutAdd : IEnumerable
    {
        public IEnumerator GetEnumerator()
        {
            // Some implementation here
        }
    }
    
    public static class Extensions
    {
        public static void Add<T>(this CollectionWithoutAdd collection, T item)
        {
            Console.WriteLine("Item added with extension add method: " + item);
        }
    }
    
    public class Program
    {
        public static void Main()
        {
            var collection1 = new CollectionWithAdd{1,2,3}; // Valid in all C# versions
            var collection2 = new CollectionWithoutAdd{4,5,6}; // Valid only since C# 6
        }
    }


Cela affichera :

> Élément ajouté avec la méthode d'ajout d'instance : 1
> Élément ajouté avec la méthode d'ajout d'instance : 2
> Élément ajouté avec la méthode d'ajout d'instance : 3
>Article ajouté avec la méthode d'ajout d'extension : 4
>Article ajouté avec la méthode d'ajout d'extension : 5
>Article ajouté avec la méthode d'ajout d'extension : 6

## Désactiver les améliorations des avertissements
Dans C# 5.0 et versions antérieures, le développeur ne pouvait supprimer les avertissements que par numéro. Avec l'introduction des analyseurs Roslyn, C# a besoin d'un moyen de désactiver les avertissements émis par des bibliothèques spécifiques. Avec C# 6.0, la directive pragma peut supprimer les avertissements par nom.

Avant de:

    #pragma warning disable 0501

C# 6.0 :

    #pragma warning disable CS0501

