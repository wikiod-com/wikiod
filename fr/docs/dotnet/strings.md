---
title: "Cordes"
slug: "cordes"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

Dans les chaînes .NET, "System.String" est une séquence de caractères "System.Char", chaque caractère est une unité de code encodée en UTF-16. Cette distinction est importante car la définition de _langue parlée_ de _caractère_ et la définition de caractère .NET (et de nombreuses autres langues) sont différentes.

Un _caractère_, qui devrait être correctement appelé [graphème][1], est affiché sous la forme d'un [glyphe][2] et est défini par un ou plusieurs [points de code][3] Unicode. Chaque point de code est ensuite codé dans une séquence de [code-units][4]. Maintenant, il devrait être clair pourquoi un seul `System.Char` ne représente pas toujours un graphème, voyons dans le monde réel en quoi ils sont différents :

* Un graphème, en raison de la [combinaison de caractères][5], peut entraîner deux points de code ou plus : <kbd>à</kbd> est composé de deux points de code : _U+0061 LETTRE MINUSCULE LATINE A_ et _U+ 0300 COMBINAISON GRAVE ACCENT_. C'est l'erreur la plus courante car `"à".Length == 2` alors que vous pouvez vous attendre à `1`.
* Il y a des caractères en double, par exemple <kbd>à</kbd> peut être un seul point de code _U+00E0 LETTRE MINUSCULE LATINE A AVEC GRAVE_ ou deux points de code comme expliqué ci-dessus. Évidemment, ils doivent comparer le même: `"\u00e0" == "\u0061\u0300"` (même si `"\u00e0".Length != "\u0061\u0300".Length`). Ceci est possible grâce à la _normalisation des chaînes_ effectuée par la méthode `String.Normalize()`.
* Une séquence Unicode peut contenir une séquence composée ou décomposée, par exemple le caractère <kbd>한</kbd> _U+D55C HAN CHARACTER_ peut être un seul point de code (codé comme une seule unité de code en UTF-16) ou un séquence décomposée de ses syllabes <kbd>ᄒ</kbd>, <kbd>ᅡ</kbd> et <kbd>ᆫ</kbd>. Ils doivent être comparés égaux.
* Un point de code peut être encodé en plusieurs unités de code : le caractère <kbd>𠂊</kbd> _U+2008A HAN CHARACTER_ est encodé sous la forme de deux `System.Char` (`"\ud840\udc8a"`) pair s'il s'agit d'un seul point de code : l'encodage UTF-16 n'est pas de taille fixe ! C'est une source d'innombrables bogues (également de graves bogues de sécurité), si par exemple votre application applique une longueur maximale et tronque aveuglément la chaîne, vous risquez de créer une chaîne invalide.
* Certaines langues ont [digraphe][6] et des trigraphes, par exemple en tchèque <kbd>ch</kbd> est une lettre autonome (après <kbd>h</kbd> et avant <kbd>i</kbd> puis lors de la commande d'une liste de cordes, vous aurez *fyzika* avant *chemie*.

Il y a beaucoup plus de problèmes concernant la gestion du texte, voir par exemple [Comment puis-je effectuer une comparaison caractère par caractère compatible Unicode ?] [7] pour une introduction plus large et plus de liens vers des arguments connexes.

En général, lorsque vous traitez avec du texte _international_, vous pouvez utiliser cette fonction simple pour énumérer des éléments de texte dans une chaîne (en évitant de casser les substituts et l'encodage Unicode) :

    public static class StringExtensions
    {
        public static IEnumerable<string> EnumerateCharacters(this string s)
        {
            if (s == null)
                return Enumerable.Empty<string>();

            var enumerator = StringInfo.GetTextElementEnumerator(s.Normalize());
            while (enumerator.MoveNext())
                yield return (string)enumerator.Value;
        }
    }


[1] : https://en.wikipedia.org/wiki/Grapheme
[2] : https://en.wikipedia.org/wiki/Glyph
[3] : https://en.wikipedia.org/wiki/Code_point
[4] : https://en.wikipedia.org/wiki/Character_encoding#Code_unit
[5] : https://en.wikipedia.org/wiki/Combining_character
[6] : https://en.wikipedia.org/wiki/Digraph_(orthographe)
[7] : http://stackoverflow.com/q/27229589/1207195

## Compter les caractères
Si vous devez compter _characters_ alors, pour les raisons expliquées dans la section _Remarks_, vous ne pouvez pas simplement utiliser la propriété Length car c'est la longueur du tableau de `System.Char` qui ne sont pas des caractères mais des unités de code (pas de code Unicode- points ni graphèmes). Le bon code est alors :

    int length = text.EnumerateCharacters().Count();

Une petite optimisation peut réécrire la méthode d'extension `EnumerateCharacters()` spécifiquement à cette fin :

    public static class StringExtensions
    {
        public static int CountCharacters(this string text)
        {
            if (String.IsNullOrEmpty(text))
                return 0;
    
            int count = 0;
            var enumerator = StringInfo.GetTextElementEnumerator(text);
            while (enumerator.MoveNext())
                ++count;
    
            return count;
        }
    }

## Compter les caractères distincts
Si vous avez besoin de compter des caractères distincts, pour les raisons expliquées dans la section * Remarques *, vous ne pouvez pas simplement utiliser la propriété `Length` car c'est la longueur du tableau de `System.Char` qui ne sont pas des caractères mais des unités de code (pas les points de code Unicode ni les graphèmes). Si, par exemple, vous écrivez simplement `text.Distinct().Count()` vous obtiendrez des résultats incorrects, corrigez le code :

    int distinctCharactersCount = text.EnumerateCharacters().Count();

Une étape supplémentaire consiste à ** compter les occurrences de chaque caractère **, si les performances ne sont pas un problème, vous pouvez simplement le faire comme ceci (dans cet exemple, quel que soit le cas):

    var frequencies = text.EnumerateCharacters()
        .GroupBy(x => x, StringComparer.CurrentCultureIgnoreCase)
        .Select(x => new { Character = x.Key, Count = x.Count() };

## Convertir une chaîne vers/depuis un autre encodage
Les chaînes .NET contiennent `System.Char` (unités de code UTF-16). Si vous souhaitez enregistrer (ou gérer) du texte avec un autre encodage, vous devez travailler avec un tableau de `System.Byte`.

Les conversions sont effectuées par des classes dérivées de `System.Text.Encoder` et `System.Text.Decoder` qui, ensemble, peuvent convertir vers/depuis un autre encodage (d'un tableau encodé _X_ octet `byte[]` à un UTF-16 codé `System.String` et vice-versa).

Étant donné que l'encodeur/décodeur fonctionne généralement très près l'un de l'autre, ils sont regroupés dans une classe dérivée de `System.Text.Encoding`, les classes dérivées offrent des conversions vers/depuis les encodages populaires (UTF-8, UTF-16, etc. ).

Exemples:
=

Convertir une chaîne en UTF-8
-
    byte[] data = Encoding.UTF8.GetBytes("This is my text");
---
Convertir des données UTF-8 en une chaîne
-
    var text = Encoding.UTF8.GetString(data);

---
Modifier l'encodage d'un fichier texte existant
-

Ce code lira le contenu d'un fichier texte encodé en UTF-8 et l'enregistrera encodé en UTF-16. Notez que ce code n'est pas optimal si le fichier est gros car il lira tout son contenu en mémoire :

    var content = File.ReadAllText(path, Encoding.UTF8);
    File.WriteAllText(content, Encoding.UTF16);

## Comparer des chaînes
Bien que `String` soit un type de référence, l'opérateur `==` compare les valeurs de chaîne plutôt que les références.

Comme vous le savez peut-être, `string` n'est qu'un tableau de caractères. Mais si vous pensez que la vérification et la comparaison de l'égalité des chaînes se fait caractère par caractère, vous vous trompez. Cette opération est spécifique à la culture (voir les remarques ci-dessous) : certaines séquences de caractères peuvent être traitées comme égales en fonction de la [culture][1].

Réfléchissez à deux fois avant de court-circuiter la vérification d'égalité en comparant la "longueur" [propriétés] [2] de deux chaînes !

Utilisez des surcharges de `String.Equals` [method][3] qui acceptent une valeur supplémentaire `StringComparison` [enumeration][4], si vous avez besoin de modifier le comportement par défaut.


[1] : https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx
[2] : https://msdn.microsoft.com/library/system.string.length(v=vs.110).aspx
[3] : https://msdn.microsoft.com/en-us/library/t4411bks(v=vs.110).aspx
[4] : https://msdn.microsoft.com/en-us/library/system.stringcomparison(v=vs.110).aspx

## Compter les occurrences d'un caractère
Pour les raisons expliquées dans la section _Remarques_, vous ne pouvez pas simplement le faire (sauf si vous souhaitez compter les occurrences d'une unité de code spécifique) :

    int count = text.Count(x => x == ch);

Vous avez besoin d'une fonction plus complexe :

    public static int CountOccurrencesOf(this string text, string character)
    {
        return text.EnumerateCharacters()
            .Count(x => String.Equals(x, character, StringComparer.CurrentCulture));
    }

Notez que la comparaison de chaînes (contrairement à la comparaison de caractères qui est invariante de culture) doit toujours être effectuée selon les règles d'une culture spécifique.

## Séparer la chaîne en blocs de longueur fixe
Nous ne pouvons pas diviser une chaîne en points arbitraires (parce qu'un `System.Char` peut ne pas être valide seul car c'est un caractère de combinaison ou une partie d'un substitut) alors le code doit en tenir compte (notez qu'avec _length_ je veux dire le nombre de _graphèmes_ pas le nombre de _code-units_) :

    public static IEnumerable<string> Split(this string value, int desiredLength)
    {
        var characters = StringInfo.GetTextElementEnumerator(value);
        while (characters.MoveNext())
            yield return String.Concat(Take(characters, desiredLength));
    }
    
    private static IEnumerable<string> Take(TextElementEnumerator enumerator, int count)
    {
        for (int i = 0; i < count; ++i)
        {
            yield return (string)enumerator.Current;
    
            if (!enumerator.MoveNext())
                yield break;
        }
    }

## Méthode virtuelle Object.ToString()
Tout dans .NET est un objet, donc chaque type a `ToString()` [method][1] défini dans `Object` [class][2] qui peut être remplacé. L'implémentation par défaut de cette méthode renvoie simplement le nom du type :

    public class Foo
    {
    }
    
    var foo = new Foo();
    Console.WriteLine(foo); // outputs Foo

`ToString()` est implicitement appelé lors de la concaténation d'une valeur avec une chaîne :

    public class Foo
    {
        public override string ToString()
        {
            return "I am Foo";
        }
    }
    
    var foo = new Foo();
    Console.WriteLine("I am bar and "+foo);// outputs I am bar and I am Foo

Le résultat de cette méthode est également largement utilisé par les outils de débogage. Si, pour une raison quelconque, vous ne souhaitez pas remplacer cette méthode, mais souhaitez personnaliser la façon dont le débogueur affiche la valeur de votre type, utilisez [DebuggerDisplay Attribute][4] ([MSDN][3]) :

    // [DebuggerDisplay("Person = FN {FirstName}, LN {LastName}")]
    [DebuggerDisplay("Person = FN {"+nameof(Person.FirstName)+"}, LN {"+nameof(Person.LastName)+"}")]
    public class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set;}
        // ...
    }


[1] : https://msdn.microsoft.com/en-us/library/system.object.tostring(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.object(v=vs.110).aspx
[3] : https://msdn.microsoft.com/en-us/library/system.diagnostics.debuggerdisplayattribute(v=vs.110).aspx
[4] : https://www.wikiod.com/fr/docs/c%23/1062/attributes/4689/debuggerdisplay-attribute#t=201702221225586559231

## Immuabilité des chaînes
Les chaînes sont immuables. Vous ne pouvez tout simplement pas modifier la chaîne existante. Toute opération sur la chaîne crée une nouvelle instance de la chaîne ayant une nouvelle valeur. Cela signifie que si vous avez besoin de remplacer un seul caractère dans une très longue chaîne, de la mémoire sera allouée pour une nouvelle valeur.

    string veryLongString = ...
    // memory is allocated
    string newString = veryLongString.Remove(0,1); // removes first character of the string.

Si vous devez effectuer de nombreuses opérations avec une valeur de chaîne, utilisez `StringBuilder` [class][1] qui est conçu pour une manipulation efficace des chaînes :

    var sb = new StringBuilder(someInitialString);
    foreach(var str in manyManyStrings)
    {
        sb.Append(str);
    } 
    var finalString = sb.ToString();

[1] : https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

