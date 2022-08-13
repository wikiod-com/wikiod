---
title: "Méthodes DateHeure"
slug: "methodes-dateheure"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

## Formatage de la date et de l'heure

**Formatage standard de la date et de l'heure**
 
DateTimeFormatInfo spécifie un ensemble de spécificateurs pour un formatage simple de la date et de l'heure. Chaque spécificateur correspond à un modèle de format DateTimeFormatInfo particulier.
    
        
    //Create datetime
    DateTime dt = new DateTime(2016,08,01,18,50,23,230);
    
    var t = String.Format("{0:t}", dt); // "6:50 PM"                             ShortTime
    var d = String.Format("{0:d}", dt); // "8/1/2016"                            ShortDate
    var T = String.Format("{0:T}", dt); // "6:50:23 PM"                          LongTime
    var D = String.Format("{0:D}", dt); // "Monday, August 1, 2016"              LongDate
    var f = String.Format("{0:f}", dt); // "Monday, August 1, 2016 6:50 PM"      LongDate+ShortTime
    var F = String.Format("{0:F}", dt); // "Monday, August 1, 2016 6:50:23 PM"   FullDateTime
    var g = String.Format("{0:g}", dt); // "8/1/2016 6:50 PM"                    ShortDate+ShortTime
    var G = String.Format("{0:G}", dt); // "8/1/2016 6:50:23 PM"                 ShortDate+LongTime
    var m = String.Format("{0:m}", dt); // "August 1"                            MonthDay
    var y = String.Format("{0:y}", dt); // "August 2016"                         YearMonth
    var r = String.Format("{0:r}", dt); // "SMon, 01 Aug 2016 18:50:23 GMT"      RFC1123
    var s = String.Format("{0:s}", dt); // "2016-08-01T18:50:23"                 SortableDateTime
    var u = String.Format("{0:u}", dt); // "2016-08-01 18:50:23Z"                UniversalSortableDateTime

**Formatage personnalisé de la date et de l'heure**

Il existe les spécificateurs de format personnalisés suivants :

- `y` (année)
- `M` (mois)
- `d` (jour)
- `h` (heure 12)
- `H` (heure 24)
- `m` (minute)
- `s` (seconde)
- `f` (seconde fraction)
- `F` (seconde fraction, les zéros de fin sont coupés)
- `t` (P.M ou A.M)
- `z` (fuseau horaire).


    var year =       String.Format("{0:y yy yyy yyyy}", dt); // "16 16 2016 2016"  year
    var month =      String.Format("{0:M MM MMM MMMM}", dt); // "8 08 Aug August"  month
    var day =        String.Format("{0:d dd ddd dddd}", dt); // "1 01 Mon Monday"  day
    var hour =       String.Format("{0:h hh H HH}",     dt); // "6 06 18 18"       hour 12/24
    var minute =     String.Format("{0:m mm}",          dt); // "50 50"            minute
    var secound =    String.Format("{0:s ss}",          dt); // "23 23"            second
    var fraction =   String.Format("{0:f ff fff ffff}", dt); // "2 23 230 2300"    sec.fraction
    var fraction2 =  String.Format("{0:F FF FFF FFFF}", dt); // "2 23 23 23"       without zeroes
    var period =     String.Format("{0:t tt}",          dt); // "P PM"             A.M. or P.M.
    var zone =       String.Format("{0:z zz zzz}",      dt); // "+0 +00 +00:00"    time zone

Vous pouvez également utiliser le séparateur de date `/` (barre oblique) et le séparateur d'heure `:` (deux-points).

[Pour un exemple de code](https://dotnetfiddle.net/rcovMN)

Pour plus d'informations [MSDN](https://msdn.microsoft.com/en-us/library/system.globalization.datetimeformatinfo.aspx).

## DateHeure.AjouterJours(Double)
Ajoutez des jours dans un objet dateTime.

    DateTime today = DateTime.Now;
    DateTime answer = today.AddDays(36);
    Console.WriteLine("Today: {0:dddd}", today);
    Console.WriteLine("36 days from today: {0:dddd}", answer);

Vous pouvez également soustraire des jours en passant une valeur négative :

    DateTime today = DateTime.Now;
    DateTime answer = today.AddDays(-3);
    Console.WriteLine("Today: {0:dddd}", today);
    Console.WriteLine("-3 days from today: {0:dddd}", answer);


## DateHeure.AjouterHeures(Double)
    double[] hours = {.08333, .16667, .25, .33333, .5, .66667, 1, 2, 
                            29, 30, 31, 90, 365};
    DateTime dateValue = new DateTime(2009, 3, 1, 12, 0, 0);
    
    foreach (double hour in hours)
      Console.WriteLine("{0} + {1} hour(s) = {2}", dateValue, hour, 
                               dateValue.AddHours(hour));

## DateTime.Parse (chaîne)
    // Converts the string representation of a date and time to its DateTime equivalent

    var dateTime = DateTime.Parse("14:23 22 Jul 2016");
    
    Console.WriteLine(dateTime.ToString());

## DateTime.TryParse(String, DateTime)
    // Converts the specified string representation of a date and time to its DateTime equivalent and returns a value that indicates whether the conversion succeeded
    
    string[] dateTimeStrings = new []{
        "14:23 22 Jul 2016",
        "99:23 2x Jul 2016",
        "22/7/2016 14:23:00"
    };
    
    foreach(var dateTimeString in dateTimeStrings){

        DateTime dateTime;
        
        bool wasParsed = DateTime.TryParse(dateTimeString, out dateTime);
        
        string result = dateTimeString +
            (wasParsed 
                ? $"was parsed to {dateTime}" 
                : "can't be parsed to DateTime");
                
        Console.WriteLine(result);
    }

## DateTime.AddMilliseconds(Double)
    string dateFormat = "MM/dd/yyyy hh:mm:ss.fffffff"; 
    DateTime date1 = new DateTime(2010, 9, 8, 16, 0, 0);
    Console.WriteLine("Original date: {0} ({1:N0} ticks)\n",
                      date1.ToString(dateFormat), date1.Ticks);
    
    DateTime date2 = date1.AddMilliseconds(1);
    Console.WriteLine("Second date:   {0} ({1:N0} ticks)",
                      date2.ToString(dateFormat), date2.Ticks);
    Console.WriteLine("Difference between dates: {0} ({1:N0} ticks)\n",
                      date2 - date1, date2.Ticks - date1.Ticks);                        
    
    DateTime date3 = date1.AddMilliseconds(1.5);
    Console.WriteLine("Third date:    {0} ({1:N0} ticks)",
                      date3.ToString(dateFormat), date3.Ticks);
    Console.WriteLine("Difference between dates: {0} ({1:N0} ticks)",
                      date3 - date1, date3.Ticks - date1.Ticks);   

## DateHeure.Compare(DateHeure t1, DateHeure t2 )
    DateTime date1 = new DateTime(2009, 8, 1, 0, 0, 0);
    DateTime date2 = new DateTime(2009, 8, 1, 12, 0, 0);
    int result = DateTime.Compare(date1, date2);
    string relationship;
    
    if (result < 0)
        relationship = "is earlier than";
    else if (result == 0)
        relationship = "is the same time as";         
    else relationship = "is later than";
    
    Console.WriteLine("{0} {1} {2}", date1, relationship, date2);

## DateTime.DaysInMonth(Int32, Int32)
    const int July = 7;
    const int Feb = 2;

    int daysInJuly = System.DateTime.DaysInMonth(2001, July);
    Console.WriteLine(daysInJuly);

    // daysInFeb gets 28 because the year 1998 was not a leap year.
    int daysInFeb = System.DateTime.DaysInMonth(1998, Feb);
    Console.WriteLine(daysInFeb);

    // daysInFebLeap gets 29 because the year 1996 was a leap year.
    int daysInFebLeap = System.DateTime.DaysInMonth(1996, Feb);
    Console.WriteLine(daysInFebLeap);

## DateTime.AddYears(Int32)
Ajoutez des années sur l'objet dateTime :

    DateTime baseDate = new DateTime(2000, 2, 29);
    Console.WriteLine("Base Date: {0:d}\n", baseDate);
    
    // Show dates of previous fifteen years.
    for (int ctr = -1; ctr >= -15; ctr--)
       Console.WriteLine("{0,2} year(s) ago:{1:d}", 
                          Math.Abs(ctr), baseDate.AddYears(ctr));

    Console.WriteLine();

    // Show dates of next fifteen years.
    for (int ctr = 1; ctr <= 15; ctr++)
       Console.WriteLine("{0,2} year(s) from now: {1:d}", 
                         ctr, baseDate.AddYears(ctr));

## Avertissement des fonctions pures lorsqu'il s'agit de DateTime
Wikipédia définit actuellement une fonction pure comme suit :

1. La fonction évalue toujours la même valeur de résultat étant donné la ou les mêmes valeurs d'argument. La valeur du résultat de la fonction ne peut pas dépendre d'informations ou d'états cachés susceptibles de changer pendant l'exécution du programme ou entre différentes exécutions du programme, ni d'aucune entrée externe des périphériques d'E/S.
2. L'évaluation du résultat ne provoque aucun effet secondaire ou sortie sémantiquement observable, comme la mutation d'objets modifiables ou la sortie vers des périphériques d'E/S

En tant que développeur, vous devez être au courant des méthodes pures et vous tomberez souvent dessus dans de nombreux domaines. Un que j'ai vu qui pique de nombreux développeurs juniors travaille avec les méthodes de classe DateTime. Beaucoup d'entre eux sont purs et si vous ne les connaissez pas, vous pouvez être surpris. Un exemple:

            DateTime sample = new DateTime(2016, 12, 25);
            sample.AddDays(1);
            Console.WriteLine(sample.ToShortDateString());

Compte tenu de l'exemple ci-dessus, on peut s'attendre à ce que le résultat imprimé sur la console soit "26/12/2016", mais en réalité, vous vous retrouvez avec la même date. En effet, AddDays est une méthode pure et n'affecte pas la date d'origine. Pour obtenir la sortie attendue, vous devez modifier l'appel AddDays comme suit :

            sample = sample.AddDays(1);




## DateTime.TryParseExact(String, String, IFormatProvider, DateTimeStyles, DateTime)
Convertit la représentation sous forme de chaîne spécifiée d'une date et d'une heure en son équivalent DateTime à l'aide du format spécifié, des informations de format spécifiques à la culture et du style. Le format de la représentation sous forme de chaîne doit correspondre exactement au format spécifié. La méthode renvoie une valeur qui indique si la conversion a réussi.

Par exemple


    CultureInfo enUS = new CultureInfo("en-US");
    string dateString;
    System.DateTime dateValue;

Analyser la date sans indicateur de style.

    dateString = " 5/01/2009 8:30 AM";
    if (DateTime.TryParseExact(dateString, "g", enUS, DateTimeStyles.None, out dateValue))
    {
       Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
    }
    else
    {
       Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
    }


    // Allow a leading space in the date string.
    if(DateTime.TryParseExact(dateString, "g", enUS, DateTimeStyles.AllowLeadingWhite, out dateValue))
    {
       Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
    else
    {
       Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
    }


Utilisez des formats personnalisés avec M et MM.

    dateString = "5/01/2009 09:00";
    if(DateTime.TryParseExact(dateString, "M/dd/yyyy hh:mm", enUS, DateTimeStyles.None, out dateValue))
    {
        Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
    }
    else
    {
       Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
    }

    // Allow a leading space in the date string.
    if(DateTime.TryParseExact(dateString, "MM/dd/yyyy hh:mm", enUS, DateTimeStyles.None, out dateValue))
    {
       Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
    }
    else
    {
       Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
    }


Analyser une chaîne avec des informations de fuseau horaire.

    dateString = "05/01/2009 01:30:42 PM -05:00";
    if (DateTime.TryParseExact(dateString, "MM/dd/yyyy hh:mm:ss tt zzz", enUS, DateTimeStyles.None, out dateValue))
    {
        Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
    }
    else
    {
       Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
    }


    // Allow a leading space in the date string.
    if (DateTime.TryParseExact(dateString, "MM/dd/yyyy hh:mm:ss tt zzz", enUS, DateTimeStyles.AdjustToUniversal, out dateValue))
    {
       Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
    }
    else
    {
       Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
    }


Analyser une chaîne représentant UTC.

    dateString = "2008-06-11T16:11:20.0904778Z";
    if(DateTime.TryParseExact(dateString, "o", CultureInfo.InvariantCulture, DateTimeStyles.None, out dateValue))
    {
       Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
    }
    else
    {
      Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
    }

    if (DateTime.TryParseExact(dateString, "o", CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind, out dateValue))
    {
       Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
    }
    else
    {
       Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
    }


Les sorties

    ' 5/01/2009 8:30 AM' is not in an acceptable format.
    Converted ' 5/01/2009 8:30 AM' to 5/1/2009 8:30:00 AM (Unspecified).
    Converted '5/01/2009 09:00' to 5/1/2009 9:00:00 AM (Unspecified).
    '5/01/2009 09:00' is not in an acceptable format.
    Converted '05/01/2009 01:30:42 PM -05:00' to 5/1/2009 11:30:42 AM (Local).
    Converted '05/01/2009 01:30:42 PM -05:00' to 5/1/2009 6:30:42 PM (Utc).
    Converted '2008-06-11T16:11:20.0904778Z' to 6/11/2008 9:11:20 AM (Local).
    Converted '2008-06-11T16:11:20.0904778Z' to 6/11/2008 4:11:20 PM (Utc).

## DateHeure.Add(TimeSpan)
    // Calculate what day of the week is 36 days from this instant.
    System.DateTime today = System.DateTime.Now;
    System.TimeSpan duration = new System.TimeSpan(36, 0, 0, 0);
    System.DateTime answer = today.Add(duration);
    System.Console.WriteLine("{0:dddd}", answer);

## Parse et TryParse avec des informations sur la culture
Vous voudrez peut-être l'utiliser lors de l'analyse de DateTimes de [différentes cultures (langues)] [1], l'exemple suivant analyse la date néerlandaise.

    DateTime dateResult;
    var dutchDateString = "31 oktober 1999 04:20";
    var dutchCulture = CultureInfo.CreateSpecificCulture("nl-NL");
    DateTime.TryParse(dutchDateString, dutchCulture, styles, out dateResult);
    // output {31/10/1999 04:20:00}

Exemple d'analyse :

    DateTime.Parse(dutchDateString, dutchCulture)
    // output {31/10/1999 04:20:00}


[1] : https://msdn.microsoft.com/en-gb/library/ee825488(v=cs.20).aspx

## DateTime comme initialiseur dans la boucle for
    // This iterates through a range between two DateTimes 
    // with the given iterator (any of the Add methods) 
    
    DateTime start = new DateTime(2016, 01, 01);
    DateTime until = new DateTime(2016, 02, 01);
    
    // NOTICE: As the add methods return a new DateTime you have
    // to overwrite dt in the iterator like dt = dt.Add()
    for (DateTime dt = start; dt < until; dt = dt.AddDays(1))
    {
        Console.WriteLine("Added {0} days. Resulting DateTime: {1}", 
                          (dt - start).Days, dt.ToString());
    }

*L'itération sur un `TimeSpan` fonctionne de la même manière.*

## DateTime ToString, ToShortDateString, ToLongDateString et ToString formatés
    using System;
                        
    public class Program
    {
        public static void Main()
        {
            var date = new DateTime(2016,12,31);
            
            Console.WriteLine(date.ToString());        //Outputs: 12/31/2016 12:00:00 AM
            Console.WriteLine(date.ToShortDateString()); //Outputs: 12/31/2016
            Console.WriteLine(date.ToLongDateString()); //Outputs: Saturday, December 31, 2016
            Console.WriteLine(date.ToString("dd/MM/yyyy"));    //Outputs: 31/12/2016
        }
    }

## Date actuelle
Pour obtenir la date actuelle, vous utilisez la propriété `DateTime.Today`. Cela renvoie un objet `DateTime` avec la date d'aujourd'hui. Lorsque cela est ensuite converti `.ToString()`, cela se fait par défaut dans la localité de votre système.

Par exemple:

    Console.WriteLine(DateTime.Today);

Écrit la date d'aujourd'hui, dans votre format local sur la console.

## DateTime.ParseExact(String, String, IFormatProvider)
Convertit la représentation sous forme de chaîne spécifiée d'une date et d'une heure en son équivalent DateTime à l'aide du format spécifié et des informations de format spécifiques à la culture. Le format de la représentation sous forme de chaîne doit correspondre exactement au format spécifié.

**Convertir une chaîne de format spécifique en DateTime équivalent**

Disons que nous avons une chaîne DateTime spécifique à la culture `08-07-2016 23:30:12 PM` au format `MM-jj-aaaa hh:mm:ss tt` et nous voulons qu'elle soit convertie en objet `DateTime` équivalent

    string str = "08-07-2016 11:30:12 PM";
    DateTime date = DateTime.ParseExact(str, "MM-dd-yyyy hh:mm:ss tt", CultureInfo.CurrentCulture);

**Convertir une chaîne de date et d'heure en objet équivalent `DateTime` sans aucun format de culture spécifique**

Disons que nous avons une chaîne DateTime au format `dd-MM-yy hh:mm:ss tt` et que nous voulons qu'elle soit convertie en un objet `DateTime` équivalent, sans aucune information de culture spécifique

    string str = "17-06-16 11:30:12 PM";
    DateTime date = DateTime.ParseExact(str, "dd-MM-yy hh:mm:ss tt", CultureInfo.InvariantCulture);

** Convertir une chaîne de date et d'heure en objet DateTime équivalent sans format de culture spécifique avec un format différent **

Supposons que nous ayons une chaîne Date , par exemple '23-12-2016' ou '12/23/2016' et que nous voulons qu'elle soit convertie en objet `DateTime` équivalent, sans aucune information de culture spécifique

 

       string date =  '23-12-2016' or date = 12/23/2016';
       string[] formats = new string[] {"dd-MM-yyyy","MM/dd/yyyy"}; // even can add more possible formats.
       DateTime date = DateTime.ParseExact(date,formats, CultureInfo.InvariantCulture,DateTimeStyles.None);
        

**REMARQUE : `System.Globalization` doit être ajouté pour la classe CultureInfo**

