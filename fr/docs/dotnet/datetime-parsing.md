---
title: "Analyse DateTime"
slug: "analyse-datetime"
draft: false
images: []
weight: 9816
type: docs
toc: true
---

## ParseExact
    var dateString = "2015-11-24";

    var date = DateTime.ParseExact(dateString, "yyyy-MM-dd", null);
    Console.WriteLine(date);

> 24/11/2015 00:00:00

Notez que passer `CultureInfo.CurrentCulture` comme troisième paramètre est identique à passer `null`. Ou, vous pouvez transmettre une culture spécifique.

**Formater les chaînes**

*La chaîne d'entrée peut être dans n'importe quel format correspondant à la chaîne de format*

    var date = DateTime.ParseExact("24|201511", "dd|yyyyMM", null);
    Console.WriteLine(date);

> 24/11/2015 00:00:00

*Tous les caractères qui ne sont pas des spécificateurs de format sont traités comme des littéraux*

    var date = DateTime.ParseExact("2015|11|24", "yyyy|MM|dd", null);
    Console.WriteLine(date);

> 24/11/2015 00:00:00

*La casse compte pour les spécificateurs de format*

    var date = DateTime.ParseExact("2015-01-24 11:11:30", "yyyy-mm-dd hh:MM:ss", null);
    Console.WriteLine(date);

> 24/11/2015 11:01:30

Notez que les valeurs de mois et de minutes ont été analysées dans les mauvaises destinations.

*Les chaînes de format à un seul caractère doivent être l'un des formats standard*

    var date = DateTime.ParseExact("11/24/2015", "d", new CultureInfo("en-US"));
    var date = DateTime.ParseExact("2015-11-24T10:15:45", "s", null);
    var date = DateTime.ParseExact("2015-11-24 10:15:45Z", "u", null);

**Exceptions**

*ArgumentNullException*

    var date = DateTime.ParseExact(null, "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", null, null);

*FormatException*

    var date = DateTime.ParseExact("", "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", "", null);
    var date = DateTime.ParseExact("2015-0C-24", "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", "yyyy-QQ-dd", null);

    // Single-character format strings must be one of the standard formats
    var date = DateTime.ParseExact("2015-11-24", "q", null);

    // Format strings must match the input exactly* (see next section)
    var date = DateTime.ParseExact("2015-11-24", "d", null); // Expects 11/24/2015 or 24/11/2015 for most cultures

**Gestion de plusieurs formats possibles**

    var date = DateTime.ParseExact("2015-11-24T10:15:45", 
      new [] { "s", "t", "u", "yyyy-MM-dd" }, // Will succeed as long as input matches one of these
      CultureInfo.CurrentCulture, DateTimeStyles.None);

**Gérer les différences culturelles**

    var dateString = "10/11/2015";
    var date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-US"));
    Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

> Jour : 11 ; Mois : 10

    date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-GB"));
    Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

> Jour : 10 ; Mois : 11


## TryParse
Cette méthode accepte une chaîne en entrée, tente de l'analyser dans un `DateTime` et renvoie un résultat booléen indiquant le succès ou l'échec. Si l'appel réussit, la variable transmise en tant que paramètre "out" est renseignée avec le résultat analysé.

Si l'analyse échoue, la variable transmise en tant que paramètre `out` est définie sur la valeur par défaut, `DateTime.MinValue`.

**TryParse(string, out DateTime)**

    DateTime parsedValue;

    if (DateTime.TryParse("monkey", out parsedValue))
    {
       Console.WriteLine("Apparently, 'monkey' is a date/time value. Who knew?");
    }

Cette méthode tente d'analyser la chaîne d'entrée en fonction des paramètres régionaux du système et des formats connus tels que ISO 8601 et d'autres formats courants.

    DateTime.TryParse("11/24/2015 14:28:42", out parsedValue); // true
    DateTime.TryParse("2015-11-24 14:28:42", out parsedValue); // true
    DateTime.TryParse("2015-11-24T14:28:42", out parsedValue); // true
    DateTime.TryParse("Sat, 24 Nov 2015 14:28:42", out parsedValue); // true

Étant donné que cette méthode n'accepte pas les informations de culture, elle utilise les paramètres régionaux du système. Cela peut conduire à des résultats inattendus.

    // System set to en-US culture
    bool result = DateTime.TryParse("24/11/2015", out parsedValue);
    Console.WriteLine(result);

> Faux

    // System set to en-GB culture
    bool result = DateTime.TryParse("11/24/2015", out parsedValue);
    Console.WriteLine(result);

> Faux

    // System set to en-GB culture
    bool result = DateTime.TryParse("10/11/2015", out parsedValue);
    Console.WriteLine(result);

> Vrai

Notez que si vous êtes aux États-Unis, vous pourriez être surpris que le résultat analysé soit le 10 novembre et non le 11 octobre.

**TryParse(string, IFormatProvider, DateTimeStyles, out DateTime)**

    if (DateTime.TryParse(" monkey ", new CultureInfo("en-GB"),
        DateTimeStyles.AllowLeadingWhite | DateTimeStyles.AllowTrailingWhite, out parsedValue)
    {
        Console.WriteLine("Apparently, ' monkey ' is a date/time value. Who knew?");
    }

Contrairement à sa méthode sœur, cette surcharge permet de spécifier une culture et un ou plusieurs styles spécifiques. Passer `null` pour le paramètre `IFormatProvider` utilise la culture système.

*Exceptions*

Notez qu'il est possible que cette méthode lève une exception sous certaines conditions. Ceux-ci se rapportent aux paramètres introduits pour cette surcharge : `IFormatProvider` et `DateTimeStyles`.

* `NotSupportedException` : `IFormatProvider` spécifie une culture neutre
* `ArgumentException` : `DateTimeStyles` n'est pas une option valide ou contient des indicateurs incompatibles tels que `AssumeLocal` et `AssumeUniversal`.

## TryParseExact
Cette méthode se comporte comme une combinaison de `TryParse` et `ParseExact` : elle permet de spécifier des formats personnalisés et renvoie un résultat booléen indiquant le succès ou l'échec plutôt que de lever une exception si l'analyse échoue.

**TryParseExact(string, string, IFormatProvider, DateTimeStyles, out DateTime)**

Cette surcharge tente d'analyser la chaîne d'entrée par rapport à un format spécifique. La chaîne d'entrée doit correspondre à ce format pour être analysée.

    DateTime.TryParseExact("11242015", "MMddyyyy", null, DateTimeStyles.None, out parsedValue); // true

**TryParseExact(string, string[], IFormatProvider, DateTimeStyles, out DateTime)**

Cette surcharge tente d'analyser la chaîne d'entrée par rapport à un tableau de formats. La chaîne d'entrée doit correspondre à au moins un format pour être analysée.

    DateTime.TryParseExact("11242015", new [] { "yyyy-MM-dd", "MMddyyyy" }, null, DateTimeStyles.None, out parsedValue); // true


