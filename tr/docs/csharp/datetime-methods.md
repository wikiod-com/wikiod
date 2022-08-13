---
title: "TarihSaat Yöntemleri"
slug: "tarihsaat-yontemleri"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

## Tarih Saat Biçimlendirme

**Standart Tarih Saat Biçimlendirme**
 
DateTimeFormatInfo, basit tarih ve saat biçimlendirmesi için bir dizi belirtici belirtir. Her belirtici, belirli bir DateTimeFormatInfo biçim düzenine karşılık gelir.
    
        
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

**Özel Tarih Saat Biçimlendirme**

Aşağıdaki özel biçim belirteçleri vardır:

- `y` (yıl)
- `M` (ay)
- `d` (gün)
- "s" (saat 12)
- `H` (24 saat)
- `m` (dakika)
- `s` (saniye)
- `f` (ikinci kesir)
- `F` (ikinci kesir, sondaki sıfırlar kırpılır)
- `t` (P.M veya A.M)
- "z" (zaman dilimi).


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

Ayrıca tarih ayırıcı `/` (eğik çizgi) ve saat ayırıcı `:` (iki nokta üst üste) kullanabilirsiniz.

[Kod örneği için](https://dotnetfiddle.net/rcovMN)

Daha fazla bilgi için [MSDN](https://msdn.microsoft.com/en-us/library/system.globalization.datetimeformatinfo.aspx).

## DateTime.AddDays(Çift)
Bir dateTime nesnesine günler ekleyin.

    DateTime today = DateTime.Now;
    DateTime answer = today.AddDays(36);
    Console.WriteLine("Today: {0:dddd}", today);
    Console.WriteLine("36 days from today: {0:dddd}", answer);

Negatif bir değer geçen günleri de çıkarabilirsiniz:

    DateTime today = DateTime.Now;
    DateTime answer = today.AddDays(-3);
    Console.WriteLine("Today: {0:dddd}", today);
    Console.WriteLine("-3 days from today: {0:dddd}", answer);


## DateTime.AddHours(Çift)
    double[] hours = {.08333, .16667, .25, .33333, .5, .66667, 1, 2, 
                            29, 30, 31, 90, 365};
    DateTime dateValue = new DateTime(2009, 3, 1, 12, 0, 0);
    
    foreach (double hour in hours)
      Console.WriteLine("{0} + {1} hour(s) = {2}", dateValue, hour, 
                               dateValue.AddHours(hour));

## DateTime.Parse(Dize)
    // Converts the string representation of a date and time to its DateTime equivalent

    var dateTime = DateTime.Parse("14:23 22 Jul 2016");
    
    Console.WriteLine(dateTime.ToString());

## DateTime.TryParse(Dize, DateTime)
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

## DateTime.AddMillisaniye(Çift)
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

## DateTime.Compare(DateTime t1, DateTime t2 )
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
dateTime nesnesine yıllar ekleyin:

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

## DateTime ile uğraşırken saf işlevler uyarısı
Wikipedia şu anda saf bir işlevi şu şekilde tanımlar:

1. İşlev, aynı argüman değeri/değerleri verilen aynı sonuç değerini her zaman değerlendirir. İşlev sonuç değeri, program yürütmesi devam ederken veya programın farklı yürütmeleri arasında değişebilecek herhangi bir gizli bilgiye veya duruma bağlı olamaz ve G/Ç aygıtlarından herhangi bir harici girişe bağlı olamaz.
2. Sonucun değerlendirilmesi, değiştirilebilir nesnelerin mutasyonu veya I/O cihazlarına çıkış gibi anlamsal olarak gözlemlenebilir herhangi bir yan etkiye veya çıktıya neden olmaz.

Bir geliştirici olarak saf yöntemlerin farkında olmanız gerekir ve bunlara birçok alanda rastlarsınız. Birçok genç geliştiricinin ısırdığını gördüğüm biri DateTime sınıf yöntemleriyle çalışıyor. Bunların çoğu saftır ve bunlardan habersizseniz bir sürpriz içinde olabilirsiniz. Bir örnek:

            DateTime sample = new DateTime(2016, 12, 25);
            sample.AddDays(1);
            Console.WriteLine(sample.ToShortDateString());

Yukarıdaki örnek göz önüne alındığında, konsola yazdırılan sonucun '26/12/2016' olmasını bekleyebilir, ancak gerçekte aynı tarihle sonuçlanırsınız. Bunun nedeni, AddDays'in saf bir yöntem olması ve orijinal tarihi etkilememesidir. Beklenen çıktıyı elde etmek için AddDays çağrısını aşağıdaki şekilde değiştirmeniz gerekir:

            sample = sample.AddDays(1);




## DateTime.TryParseExact(String, String, IFormatProvider, DateTimeStyles, DateTime)
Belirtilen biçimi, kültüre özgü biçim bilgilerini ve stili kullanarak bir tarih ve saatin belirtilen dize temsilini DateTime eşdeğerine dönüştürür. Dize gösteriminin formatı, belirtilen formatla tam olarak eşleşmelidir. Yöntem, dönüştürmenin başarılı olup olmadığını gösteren bir değer döndürür.

Örneğin


    CultureInfo enUS = new CultureInfo("en-US");
    string dateString;
    System.DateTime dateValue;

Stil bayrakları olmadan tarihi ayrıştırın.

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


M ve MM ile özel formatlar kullanın.

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


Bir dizeyi saat dilimi bilgileriyle ayrıştırın.

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


UTC'yi temsil eden bir dizeyi ayrıştırın.

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


çıktılar

    ' 5/01/2009 8:30 AM' is not in an acceptable format.
    Converted ' 5/01/2009 8:30 AM' to 5/1/2009 8:30:00 AM (Unspecified).
    Converted '5/01/2009 09:00' to 5/1/2009 9:00:00 AM (Unspecified).
    '5/01/2009 09:00' is not in an acceptable format.
    Converted '05/01/2009 01:30:42 PM -05:00' to 5/1/2009 11:30:42 AM (Local).
    Converted '05/01/2009 01:30:42 PM -05:00' to 5/1/2009 6:30:42 PM (Utc).
    Converted '2008-06-11T16:11:20.0904778Z' to 6/11/2008 9:11:20 AM (Local).
    Converted '2008-06-11T16:11:20.0904778Z' to 6/11/2008 4:11:20 PM (Utc).

## DateTime.Add(TimeSpan)
    // Calculate what day of the week is 36 days from this instant.
    System.DateTime today = System.DateTime.Now;
    System.TimeSpan duration = new System.TimeSpan(36, 0, 0, 0);
    System.DateTime answer = today.Add(duration);
    System.Console.WriteLine("{0:dddd}", answer);

## Kültür bilgisi ile Ayrıştırma ve TryParse
Aşağıdaki örnek Hollanda tarihini ayrıştırır.

    DateTime dateResult;
    var dutchDateString = "31 oktober 1999 04:20";
    var dutchCulture = CultureInfo.CreateSpecificCulture("nl-NL");
    DateTime.TryParse(dutchDateString, dutchCulture, styles, out dateResult);
    // output {31/10/1999 04:20:00}

Ayrıştırma Örneği:

    DateTime.Parse(dutchDateString, dutchCulture)
    // output {31/10/1999 04:20:00}


[1]: https://msdn.microsoft.com/en-gb/library/ee825488(v=cs.20).aspx

## for döngüsünde başlatıcı olarak DateTime
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

*'TimeSpan' üzerinde yineleme yapmak aynı şekilde çalışır.*

## DateTime ToString, ToShortDateString, ToLongDateString ve ToString biçimlendirilmiş
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

## Geçerli tarih
Geçerli tarihi almak için `DateTime.Today` özelliğini kullanırsınız. Bu, bugünün tarihini içeren bir "DateTime" nesnesi döndürür. Bu daha sonra `.ToString()` dönüştürüldüğünde, varsayılan olarak sisteminizin bulunduğu yerde yapılır.

Örneğin:

    Console.WriteLine(DateTime.Today);

Bugünün tarihini yerel biçimde konsola yazar.

## DateTime.ParseExact(String, String, IFormatProvider)
Belirtilen biçimi ve kültüre özgü biçim bilgilerini kullanarak bir tarih ve saatin belirtilen dize temsilini DateTime eşdeğerine dönüştürür. Dize gösteriminin formatı, belirtilen formatla tam olarak eşleşmelidir.

**Belirli bir biçim dizesini eşdeğer DateTime'a dönüştürün**

Diyelim ki 'AA-gg-yyyy hh:mm:ss tt' biçiminde '08-07-2016 11:30:12 PM' kültüre özgü bir DateTime dizgemiz var ve bunun eşdeğer 'DateTime' nesnesine dönüştürülmesini istiyoruz.

    string str = "08-07-2016 11:30:12 PM";
    DateTime date = DateTime.ParseExact(str, "MM-dd-yyyy hh:mm:ss tt", CultureInfo.CurrentCulture);

**Belirli bir kültür biçimi olmadan bir tarih saat dizesini eşdeğer 'DateTime' nesnesine dönüştürün**

Diyelim ki 'dd-MM-yy hh:mm:ss tt' biçiminde bir DateTime dizgimiz var ve herhangi bir spesifik kültür bilgisi olmadan eşdeğer 'DateTime' nesnesine dönüştürülmesini istiyoruz.

    string str = "17-06-16 11:30:12 PM";
    DateTime date = DateTime.ParseExact(str, "dd-MM-yy hh:mm:ss tt", CultureInfo.InvariantCulture);

**Bir tarih saat dizesini, farklı biçime sahip herhangi bir belirli kültür biçimi olmadan eşdeğer DateTime nesnesine dönüştürün**

Diyelim ki '23-12-2016' veya '12/23/2016' gibi bir Date dizemiz var ve herhangi bir özel kültür bilgisi olmadan eşdeğer 'DateTime' nesnesine dönüştürülmesini istiyoruz.

 

       string date =  '23-12-2016' or date = 12/23/2016';
       string[] formats = new string[] {"dd-MM-yyyy","MM/dd/yyyy"}; // even can add more possible formats.
       DateTime date = DateTime.ParseExact(date,formats, CultureInfo.InvariantCulture,DateTimeStyles.None);
        

**NOT : CultureInfo Class için `System.Globalization` eklenmesi gerekiyor**

