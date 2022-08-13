---
title: "Adlandırma Kuralları"
slug: "adlandrma-kurallar"
draft: false
images: []
weight: 9868
type: docs
toc: true
---

Bu konu, C# dilinde yazarken kullanılan bazı temel adlandırma kurallarını özetlemektedir. Tüm kurallar gibi, bunlar da derleyici tarafından uygulanmaz, ancak geliştiriciler arasında okunabilirliği sağlar.

Kapsamlı .NET çerçeve tasarım yönergeleri için bkz. [docs.microsoft.com/dotnet/standard/design-guidelines](https://docs.microsoft.com/dotnet/standard/design-guidelines/).

## Kolayca okunabilen tanımlayıcı adları seçin
Örneğin, HorizontalAlignment adlı bir özellik İngilizce'de AlignmentHorizontal'dan daha okunabilirdir.

## Kısalık yerine okunabilirliği tercih edin
"CanScrollHorizontally" özellik adı, "ScrollableX"den (X eksenine belirsiz bir referans) daha iyidir.

Alt çizgi, kısa çizgi veya diğer alfasayısal olmayan karakterler kullanmaktan kaçının.

## Macar notasyonunu **kullanmayın**
Macar notasyonu, tanımlayıcının veri türü gibi parametre hakkında bazı meta verileri kodlamak için tanımlayıcılara bir önek ekleme uygulamasıdır, ör. `string strName`.

Ayrıca, C# içinde zaten kullanılan anahtar sözcüklerle çakışan tanımlayıcıları kullanmaktan kaçının.

## Kelime Kısaltmaları ve Baş Harf Kısaltmaları
Genel olarak kısaltmalar veya akronimler kullanmamalısınız; bunlar adlarınızı daha az okunabilir hale getirir. Benzer şekilde, bir kısaltmanın geniş çapta tanındığını varsaymanın ne zaman güvenli olduğunu bilmek zordur.

## Büyük harf kullanımı kuralları
Aşağıdaki terimler, vaka tanımlayıcılarının farklı yollarını açıklar.
## Pascal Muhafazası
Tanımlayıcıdaki ilk harf ve sonraki her bir birleştirilmiş kelimenin ilk harfi büyük harfle yazılır. Üç veya daha fazla karakterden oluşan tanımlayıcılar için Pascal durumunu kullanabilirsiniz. Örneğin: "Arka Renk"

## Deve Muhafazası
Bir tanımlayıcının ilk harfi küçük harftir ve sonraki her bir birleştirilmiş kelimenin ilk harfi büyük harfle yazılır. Örneğin: "backColor"

## Büyük harf
Tanımlayıcıdaki tüm harfler büyük harfle yazılır. Örneğin: "IO"

---

## Tüzük
Bir tanımlayıcı birden çok kelimeden oluştuğunda, kelimeler arasında alt çizgi ("_") veya kısa çizgi ("-") gibi ayırıcılar kullanmayın. Bunun yerine, her kelimenin başlangıcını belirtmek için büyük/küçük harf kullanın.

Aşağıdaki tabloda, tanımlayıcılar için büyük harf kullanımı kuralları özetlenir ve farklı tanımlayıcı türleri için örnekler sağlanır:

tanımlayıcı | Kasa | Örnek
---------------------- | ------ | -------
Yerel değişken | deve | araba adı
sınıf | paskal | UygulamaAlanı
Numaralandırma türü | paskal | HataSeviyesi
Numaralandırma değerleri | paskal | Ölümcül hata
Etkinlik | paskal | Değer Değiştirildi
İstisna sınıfı | paskal | Webİstisnası
Salt okunur statik alan | paskal | Kırmızı Değer
arayüz | paskal | Tek kullanımlık
Yöntem | paskal | ToString
Ad alanı | paskal | Sistem.Çizim
parametre | deve | türAdı
Mülk | paskal | Arka plan rengi

Daha fazla bilgi [MSDN][1]'de bulunabilir.


[1]: https://msdn.microsoft.com/library/ms229043(v=vs.110).aspx

## Numaralandırmalar
## Çoğu Enum için tekil bir ad kullanın

    public enum Volume
    {
       Low,
       Medium,
       High
    }

## Bit alanları olan Enum türleri için çoğul bir ad kullanın

    [Flags]
    public enum MyColors
    {
        Yellow = 1,
        Green = 2,
        Red = 4,
        Blue = 8
    }
*Not: Her zaman [`FlagsAttribute`][1]'i bir bit alanına Enum type ekleyin.*

## Son ek olarak 'enum' eklemeyin**

    public enum VolumeEnum // Incorrect

## Her girişte numaralandırma adını **kullanmayın**

    public enum Color
    {
        ColorBlue, // Remove Color, unnecessary
        ColorGreen,
    }


[1]: https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx

## Arayüzler
Arayüzler, isimlerle veya isim öbekleriyle veya davranışı tanımlayan sıfatlarla adlandırılmalıdır. Örneğin, "IComponent" tanımlayıcı bir isim kullanır, "ICustomAttributeProvider" bir isim tamlaması kullanır ve "IPersistable" bir sıfat kullanır.

Arayüz adlarının önüne, tipin bir arayüz olduğunu belirtmek için 'I' harfi gelmeli ve Pascal durumu kullanılmalıdır.

Aşağıda doğru şekilde adlandırılmış arayüzler bulunmaktadır:

    public interface IServiceProvider
    public interface IFormatable

## İstisnalar
## Son ek olarak 'istisna' ekle
Özel istisna adlarının sonuna "-İstisna" eklenmelidir.

Aşağıda doğru şekilde adlandırılmış istisnalar bulunmaktadır:

    public class MyCustomException : Exception
    public class FooException : Exception

## Özel alanlar
Özel alanlar için iki yaygın kural vardır: "camelCase" ve "_camelCaseWithLeadingUnderscore".

## Deve çantası

    public class Rational
    {
        private readonly int numerator;
        private readonly int denominator;

        public Rational(int numerator, int denominator)
        {
            // "this" keyword is required to refer to the class-scope field
            this.numerator = numerator;
            this.denominator = denominator;
        }
    }

## Alt çizgili deve çantası

    public class Rational
    {
        private readonly int _numerator;
        private readonly int _denominator;

        public Rational(int numerator, int denominator)
        {
            // Names are unique, so "this" keyword is not required
            _numerator = numerator;
            _denominator = denominator;
        }
    }

## Ad alanları
Ad alanları için genel biçim:

    <Company>.(<Product>|<Technology>)[.<Feature>][.<Subnamespace>].

Örnekler şunları içerir:

    Fabrikam.Math
    Litware.Security

Ad alanı adlarının önüne bir şirket adı eklenmesi, farklı şirketlere ait ad alanlarının aynı ada sahip olmasını engeller.



