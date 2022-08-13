---
title: "String.Format"
slug: "stringformat"
draft: false
images: []
weight: 9519
type: docs
toc: true
---

"Format" yöntemleri, nesneleri belirli dize temsillerinde birleştiren dizeler oluşturmak için kullanılan [`System.String`][2] sınıfındaki bir [aşırı yüklemeler][1] kümesidir. Bu bilgi, [`String.Format`][1], çeşitli `WriteLine` yöntemlerinin yanı sıra .NET çerçevesindeki diğer yöntemlere uygulanabilir.

[1]: https://msdn.microsoft.com/en-us/library/system.string.format(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx

## Sözdizimi
- string.Format(dize biçimi, params nesnesi[] argümanlar)
- string.Format(IFormatProvider sağlayıcı, dize formatı, params nesnesi[] argümanlar)
- $"string {text} blablabla" // C#6'dan beri

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| biçim | *args*'ın bir dizgede nasıl birleştirilmesi gerektiğini tanımlayan bir [bileşik biçim dizgisi][1]. |
| argümanlar | Bir dizede birleştirilecek nesne dizisi. Bu bir [`params`][2] bağımsız değişkeni kullandığından, virgülle ayrılmış bir bağımsız değişken listesi veya gerçek bir nesne dizisi kullanabilirsiniz. |
| sağlayıcı | Nesneleri dizelere biçimlendirme yollarının bir koleksiyonu. Tipik değerler arasında [CultureInfo.InvariantCulture][3] ve [CultureInfo.CurrentCulture][4] bulunur. |


[1]: https://msdn.microsoft.com/en-us/library/txafckwd(v=vs.110).aspx
[2]: https://www.wikiod.com/tr/docs/c%23/26/keywords/2513/params#t=201607212143476676934
[3]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.invariantculture(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx

Notlar:

- `String.Format()`, bir istisna atmadan `boş` argümanları işler.
- "args" parametresini bir, iki veya üç nesne parametresiyle değiştiren aşırı yüklemeler var.



## C# 6.0'dan beri
<!-- eğer [gte 6.0] versiyonu -->

C# 6.0'dan beri `String.Format` yerine string enterpolasyonu kullanmak mümkündür.

    string name = "John";
    string lastname = "Doe";
    Console.WriteLine($"Hello {name} {lastname}!");

> Merhaba John Doe!

<sup>C# 6.0 özellikleri başlığı altında bunun için daha fazla örnek: https://www.wikiod.com/tr/docs/c%23/24/c-sharp-6-0-features/49/string-interpolation#t=201607220912379524818. </sup>

<!-- eğer --> son sürüm
 

## String.Format'ın çerçeveye 'gömülü' olduğu yerler
`String.Format` *dolaylı olarak* kullanabileceğiniz birkaç yer vardır: İşin sırrı, `string format, params object[] args` imzasıyla aşırı yüklemeyi aramaktır, örn.:

    Console.WriteLine(String.Format("{0} - {1}", name, value));

Daha kısa versiyonla değiştirilebilir:

    Console.WriteLine("{0} - {1}", name, value);

`String.Format` örneğini kullanan başka yöntemler de vardır:

    Debug.WriteLine(); // and Print()
    StringBuilder.AppendFormat();

## Özel bir biçim sağlayıcı oluşturun
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

Kullanım:

    String.Format(new CustomFormat(), "-> {0:Reverse} <-", "Hello World");

Çıktı:

    -> dlroW olleH <-

## Tarih Biçimlendirme
    DateTime date = new DateTime(2016, 07, 06, 18, 30, 14);
    // Format: year, month, day hours, minutes, seconds

    Console.Write(String.Format("{0:dd}",date)); 

    //Format by Culture info
    String.Format(new System.Globalization.CultureInfo("mn-MN"),"{0:dddd}",date);

<!-- eğer [gte 6.0] versiyonu -->
    Console.Write($"{date:ddd}");
<!-- eğer --> son sürüm

çıktı :

    06
    Лхагва
    06

| Tanımlayıcı| Anlamı| Örnek| Sonuç|
| ------ | ------ | ------ | ------ |
|d| Tarih |`{0:d}`|7/6/2016|
|dd| Gün, sıfır dolgulu |`{0:dd}`|06|
|ddd|Kısa gün adı|`{0:ddd}`|Çrş|
|dddd|Tam gün adı|`{0:dddd}`|Çarşamba|
|D|Uzun tarih|`{0:D}`|6 Temmuz 2016 Çarşamba|
|f|Tam tarih ve saat, kısa|`{0:f}`|6 Temmuz 2016 Çarşamba 18:30|
|ff|İkinci kesir, 2 basamak|`{0:ff}`|20|
|fff|İkinci kesir, 3 basamak|`{0:fff}`|201|
|ffff|İkinci kesir, 4 basamak|`{0:ffff}`|2016|
|F|Tam tarih ve saat, uzun|`{0:F}`|6 Temmuz 2016 Çarşamba 18:30:14|
|g|Varsayılan tarih ve saat|`{0:g}`|7/6/2016 18:30|
|gg|Dönem|`{0:gg}`|A.D|
|hh|Saat (2 basamak, 12H)|`{0:hh}`|06|
|SS|Saat (2 basamak, 24H)|`{0:HH}`|18|
|A|Ay ve gün|`{0:M}`|6 Temmuz|
|mm|Dakika, sıfır dolgulu|`{0:mm}`|30|
|AA|Ay, sıfır dolgulu|`{0:MM}`|07|
|MMM|3 harfli ay adı|`{0:MMM}`|Tem|
|MMMM|Tam ay adı|`{0:MMMM}`|Temmuz|
|ss|Saniye|`{0:ss}`|14|
|r| RFC1123 tarih|`{0:r}`|06 Temmuz 2016 Çar 18:30:14 GMT|
|s| Sıralanabilir tarih dizesi|`{0:s}`|2016-07-06T18:30:14|
|t| Kısa süre |`{0:t}`|18:30|
|T|Uzun süre|`{0:T}`|6:30:14 PM|
|ht|AM/PM|`{0:ht}`|PM|
|u|Evrensel sıralanabilir yerel saat|`{0:u}`|2016-07-06 18:30:14Z|
|U| Evrensel GMT|`{0:U}`|6 Temmuz 2016 Çarşamba 09:30:14|
|E| Ay ve yıl|`{0:Y}`|Temmuz 2016|
|yy|2 basamaklı yıl|`{0:yy}`|16|
|yyyy|4 basamaklı yıl|`{0:yyyy}`|2016|
|zz|2 haneli saat dilimi farkı|`{0:zz}`|+09|
|zzz|tam saat dilimi farkı|`{0:zzz}`|+09:00|

## Para Birimi Biçimlendirme
"c" (veya para birimi) biçim belirteci, bir sayıyı para birimi miktarını temsil eden bir dizeye dönüştürür.

    string.Format("{0:c}", 112.236677) // $112.23 - defaults to system

## Kesinlik ##
Varsayılan 2'dir. Kesinliği kontrol etmek için c1, c2, c3 ve benzerlerini kullanın.

    string.Format("{0:C1}", 112.236677) //$112.2
    string.Format("{0:C3}", 112.236677) //$112.237
    string.Format("{0:C4}", 112.236677) //$112.2367
    string.Format("{0:C9}", 112.236677) //$112.236677000

## Para Birimi Sembolü ##

1. Özel kültür sembolünü kullanmak için "CultureInfo" örneğini iletin.


    string.Format(new CultureInfo("en-US"), "{0:c}", 112.236677); //$112.24
    string.Format(new CultureInfo("de-DE"), "{0:c}", 112.236677); //112,24 €
    string.Format(new CultureInfo("hi-IN"), "{0:c}", 112.236677); //₹ 112.24


2. Herhangi bir dizeyi para birimi simgesi olarak kullanın. Para birimi simgesini özelleştirmek için `NumberFormatInfo` kullanın.


    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;
    nfi = (NumberFormatInfo) nfi.Clone();
    nfi.CurrencySymbol = "?";
    string.Format(nfi, "{0:C}", 112.236677); //?112.24
    nfi.CurrencySymbol = "?%^&";
    string.Format(nfi, "{0:C}", 112.236677); //?%^&112.24

## Para Birimi Sembolünün Konumu ##

Pozitif değerler için [CurrencyPositivePattern][1] ve negatif değerler için [CurrencyNegativePattern][2] kullanın.

    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
    nfi.CurrencyPositivePattern = 0;
    string.Format(nfi, "{0:C}", 112.236677); //$112.24 - default
    nfi.CurrencyPositivePattern = 1;
    string.Format(nfi, "{0:C}", 112.236677); //112.24$
    nfi.CurrencyPositivePattern = 2;
    string.Format(nfi, "{0:C}", 112.236677); //$ 112.24
    nfi.CurrencyPositivePattern = 3; 
    string.Format(nfi, "{0:C}", 112.236677); //112.24 $

Negatif kalıp kullanımı, pozitif kalıpla aynıdır. Çok daha fazla kullanım durumu, lütfen orijinal bağlantıya bakın.

## Özel Ondalık Ayırıcı ##

    NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
    nfi.CurrencyPositivePattern = 0;
    nfi.CurrencyDecimalSeparator = "..";
    string.Format(nfi, "{0:C}", 112.236677); //$112..24

[1]: https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencypositivepattern(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencynegativepattern(v=vs.110).aspx

## Özel sayı biçimini kullanma
`NumberFormatInfo` hem tamsayı hem de kayan sayıları biçimlendirmek için kullanılabilir.

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



## Sola/sağa hizalayın, boşluklarla doldurun
Kıvrımlı parantezlerdeki ikinci değer, değiştirme dizisinin uzunluğunu belirler.
İkinci değeri pozitif veya negatif olacak şekilde ayarlayarak dizenin hizalaması değiştirilebilir.

    string.Format("LEFT:  string: ->{0,-5}<- int: ->{1,-5}<-", "abc", 123);
    string.Format("RIGHT: string: ->{0,5}<- int: ->{1,5}<-", "abc", 123);

Çıktı:

    LEFT:  string: ->abc  <- int: ->123  <-
    RIGHT: string: ->  abc<- int: ->  123<-


## Sayısal biçimler
    // Integral types as hex
    string.Format("Hexadecimal: byte2: {0:x2}; byte4: {0:X4}; char: {1:x2}", 123, (int)'A');

    // Integers with thousand separators
    string.Format("Integer, thousand sep.: {0:#,#}; fixed length: >{0,10:#,#}<", 1234567);

    // Integer with leading zeroes
    string.Format("Integer, leading zeroes: {0:00}; ", 1);

    // Decimals
    string.Format("Decimal, fixed precision: {0:0.000}; as percents: {0:0.00%}", 0.12);

Çıktı:

    Hexadecimal: byte2: 7b; byte4: 007B; char: 41
    Integer, thousand sep.: 1,234,567; fixed length: > 1,234,567<
    Integer, leading zeroes: 01; 
    Decimal, fixed precision: 0.120; as percents: 12.00%


## Bir String.Format() ifadesi içinde küme parantezlerinden kaçma
    string outsidetext = "I am outside of bracket";
    string.Format("{{I am in brackets!}} {0}", outsidetext);

    //Outputs "{I am in brackets!} I am outside of bracket"

## ToString()
ToString() yöntemi, tüm başvuru nesne türlerinde bulunur. Bunun nedeni, üzerinde ToString() yöntemine sahip olan Object'ten türetilen tüm referans türleridir. Nesne temel sınıfındaki ToString() yöntemi, tür adını döndürür. Aşağıdaki parça konsola "Kullanıcı" yazdıracaktır.

    public class User
    {
        public string Name { get; set; }
        public int Id { get; set; }
    }

    ...

    var user = new User {Name = "User1", Id = 5};
    Console.WriteLine(user.ToString());


Ancak, User sınıfı, döndürdüğü dizeyi değiştirmek için ToString() öğesini de geçersiz kılabilir. Aşağıdaki kod parçası konsola "Id: 5, Name: User1" yazdırır.

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


## ToString() ile İlişki
'String.Format()' yöntemi, verileri dizeler olarak biçimlendirmede kesinlikle yararlı olsa da, özellikle aşağıda görüldüğü gibi tek bir nesneyle uğraşırken, genellikle biraz abartılı olabilir:

    String.Format("{0:C}", money);  // yields "$42.00"

Daha kolay bir yaklaşım, C# içindeki tüm nesnelerde bulunan `ToString()` yöntemini kullanmak olabilir. Aynı [standart ve özel biçimlendirme dizelerinin](https://msdn.microsoft.com/en-us/library/dwhawy9k(v=vs.110).aspx) tümünü destekler, ancak gerekli parametreyi gerektirmez yalnızca tek bir argüman olacağından eşleme:

    money.ToString("C");  // yields "$42.00"

**Uyarılar ve Biçimlendirme Kısıtlamaları**
---

Bu yaklaşım bazı senaryolarda daha basit olsa da, 'ToString()' yaklaşımı, 'String.Format()' yönteminde yaptığınız gibi sola veya sağa dolgu ekleme konusunda sınırlıdır:

    String.Format("{0,10:C}", money);  // yields "    $42.00"

Aynı davranışı `ToString()` yöntemiyle gerçekleştirmek için sırasıyla `PadLeft()` veya `PadRight()` gibi başka bir yöntem kullanmanız gerekir:

    money.ToString("C").PadLeft(10);  // yields "    $42.00"

