---
title: "DateTime ayrıştırma"
slug: "datetime-ayrstrma"
draft: false
images: []
weight: 9816
type: docs
toc: true
---

## AyrıştırmaTam
    var dateString = "2015-11-24";

    var date = DateTime.ParseExact(dateString, "yyyy-MM-dd", null);
    Console.WriteLine(date);

> 24.11.2015 12:00:00

Üçüncü parametre olarak "CultureInfo.CurrentCulture" geçirmenin "null" iletmeyle aynı olduğunu unutmayın. Veya belirli bir kültürü geçebilirsiniz.

**Biçim Dizeleri**

*Giriş dizesi, biçim dizesiyle eşleşen herhangi bir biçimde olabilir*

    var date = DateTime.ParseExact("24|201511", "dd|yyyyMM", null);
    Console.WriteLine(date);

> 24.11.2015 12:00:00

*Biçim belirteci olmayan tüm karakterler değişmez değer olarak kabul edilir*

    var date = DateTime.ParseExact("2015|11|24", "yyyy|MM|dd", null);
    Console.WriteLine(date);

> 24.11.2015 12:00:00

*Biçim belirteçleri için büyük/küçük harf önemlidir*

    var date = DateTime.ParseExact("2015-01-24 11:11:30", "yyyy-mm-dd hh:MM:ss", null);
    Console.WriteLine(date);

> 24.11.2015 11:01:30

Ay ve dakika değerlerinin yanlış hedeflere ayrıştırıldığını unutmayın.

*Tek karakterli biçim dizeleri standart biçimlerden biri olmalıdır*

    var date = DateTime.ParseExact("11/24/2015", "d", new CultureInfo("en-US"));
    var date = DateTime.ParseExact("2015-11-24T10:15:45", "s", null);
    var date = DateTime.ParseExact("2015-11-24 10:15:45Z", "u", null);

**İstisnalar**

*ArgumentNullException*

    var date = DateTime.ParseExact(null, "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", null, null);

*Formatİstisnası*

    var date = DateTime.ParseExact("", "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", "", null);
    var date = DateTime.ParseExact("2015-0C-24", "yyyy-MM-dd", null);
    var date = DateTime.ParseExact("2015-11-24", "yyyy-QQ-dd", null);

    // Single-character format strings must be one of the standard formats
    var date = DateTime.ParseExact("2015-11-24", "q", null);

    // Format strings must match the input exactly* (see next section)
    var date = DateTime.ParseExact("2015-11-24", "d", null); // Expects 11/24/2015 or 24/11/2015 for most cultures

**Birden çok olası biçimi işleme**

    var date = DateTime.ParseExact("2015-11-24T10:15:45", 
      new [] { "s", "t", "u", "yyyy-MM-dd" }, // Will succeed as long as input matches one of these
      CultureInfo.CurrentCulture, DateTimeStyles.None);

**Kültür farklılıklarını ele alma**

    var dateString = "10/11/2015";
    var date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-US"));
    Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

> Gün: 11; Ay: 10

    date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-GB"));
    Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

> Gün: 10; Ay: 11


## Deneyin Ayrıştırma
Bu yöntem bir dizgiyi girdi olarak kabul eder, onu bir 'DateTime' olarak ayrıştırmaya çalışır ve başarıyı veya başarısızlığı gösteren bir Boolean sonucu döndürür. Çağrı başarılı olursa, "out" parametresi olarak iletilen değişken, ayrıştırılan sonuçla doldurulur.

Ayrıştırma başarısız olursa, "out" parametresi olarak iletilen değişken, "DateTime.MinValue" varsayılan değerine ayarlanır.

**TryParse(dize, DateTime çıkışı)**

    DateTime parsedValue;

    if (DateTime.TryParse("monkey", out parsedValue))
    {
       Console.WriteLine("Apparently, 'monkey' is a date/time value. Who knew?");
    }

Bu yöntem, sistem bölgesel ayarlarına ve ISO 8601 gibi bilinen biçimlere ve diğer yaygın biçimlere dayalı olarak giriş dizesini ayrıştırmaya çalışır.

    DateTime.TryParse("11/24/2015 14:28:42", out parsedValue); // true
    DateTime.TryParse("2015-11-24 14:28:42", out parsedValue); // true
    DateTime.TryParse("2015-11-24T14:28:42", out parsedValue); // true
    DateTime.TryParse("Sat, 24 Nov 2015 14:28:42", out parsedValue); // true

Bu yöntem kültür bilgisini kabul etmediği için sistem yerel ayarını kullanır. Bu beklenmedik sonuçlara yol açabilir.

    // System set to en-US culture
    bool result = DateTime.TryParse("24/11/2015", out parsedValue);
    Console.WriteLine(result);

> Yanlış

    // System set to en-GB culture
    bool result = DateTime.TryParse("11/24/2015", out parsedValue);
    Console.WriteLine(result);

> Yanlış

    // System set to en-GB culture
    bool result = DateTime.TryParse("10/11/2015", out parsedValue);
    Console.WriteLine(result);

> Doğru

ABD'deyseniz, çözümlenen sonucun 11 Ekim değil 10 Kasım olduğuna şaşırabilirsiniz.

**TryParse(string, IFormatProvider, DateTimeStyles, DateTime dışında)**

    if (DateTime.TryParse(" monkey ", new CultureInfo("en-GB"),
        DateTimeStyles.AllowLeadingWhite | DateTimeStyles.AllowTrailingWhite, out parsedValue)
    {
        Console.WriteLine("Apparently, ' monkey ' is a date/time value. Who knew?");
    }

Kardeş yönteminin aksine, bu aşırı yükleme, belirli bir kültür ve stil(ler)in belirtilmesine izin verir. "IFormatProvider" parametresi için "null" iletmek, sistem kültürünü kullanır.

*İstisnalar*

Bu yöntemin belirli koşullar altında bir istisna oluşturmasının mümkün olduğunu unutmayın. Bunlar, bu aşırı yükleme için tanıtılan parametrelerle ilgilidir: "IFormatProvider" ve "DateTimeStyles".

* `NotSupportedException`: `IFormatProvider` tarafsız bir kültürü belirtir
* "ArgumentException": "DateTimeStyles" geçerli bir seçenek değil veya "AssumeLocal" ve "AssumeUniversal" gibi uyumsuz bayraklar içeriyor.

## TryParseExact
Bu yöntem 'TryParse' ve 'ParseExact' kombinasyonu gibi davranır: Özel biçim(ler)in belirtilmesine izin verir ve ayrıştırma başarısız olursa bir istisna atmak yerine başarı veya başarısızlığı gösteren bir Boole sonucu döndürür.

**TryParseExact(string, string, IFormatProvider, DateTimeStyles, Out DateTime)**

Bu aşırı yükleme, giriş dizesini belirli bir biçime karşı ayrıştırmaya çalışır. Giriş dizesinin ayrıştırılabilmesi için bu biçimle eşleşmesi gerekir.

    DateTime.TryParseExact("11242015", "MMddyyyy", null, DateTimeStyles.None, out parsedValue); // true

**TryParseExact(string, string[], IFormatProvider, DateTimeStyles, Out DateTime)**

Bu aşırı yükleme, giriş dizesini bir dizi biçime karşı ayrıştırmaya çalışır. Giriş dizesinin ayrıştırılabilmesi için en az bir biçimle eşleşmesi gerekir.

    DateTime.TryParseExact("11242015", new [] { "yyyy-MM-dd", "MMddyyyy" }, null, DateTimeStyles.None, out parsedValue); // true


