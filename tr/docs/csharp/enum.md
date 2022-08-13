---
title: "Sıralama"
slug: "sralama"
draft: false
images: []
weight: 9420
type: docs
toc: true
---

Bir numaralandırma şu türlerden herhangi birinden türetilebilir: byte, sbyte, short, ushort, int, uint, long, ulong. Varsayılan int'dir ve enum tanımında tür belirtilerek değiştirilebilir:

public enum Weekday : byte { Pazartesi = 1, Salı = 2, Çarşamba = 3, Perşembe = 4, Cuma = 5 }

Bu, yerel koda P/Çağırma, veri kaynaklarıyla eşleme ve benzer durumlarda kullanışlıdır. Genel olarak, varsayılan int kullanılmalıdır, çünkü çoğu geliştirici bir numaralandırmanın int olmasını bekler.

## Sözdizimi
- enum Renkler { Kırmızı, Yeşil, Mavi } // Enum bildirimi
- enum Renkler : byte { Red, Green, Blue } // Belirli tipte bildirim
- enum Renkler { Kırmızı = 23, Yeşil = 45, Mavi = 12 } // Tanımlanmış değerlerle beyan
- Colors.Red // Enum'un bir öğesine erişin
- int değeri = (int)Colors.Red // Enum öğesinin int değerini alın
- Colors color = (Colors)intValue // int'den bir enum öğesi alın

Bir Numaralandırma ("numaralandırılmış tür"ün kısaltması), türe özgü bir tanımlayıcıyla temsil edilen bir dizi adlandırılmış sabitten oluşan bir türdür.

Numaralandırmalar, (genellikle az) olası ayrık değerlere sahip kavramları temsil etmek için en kullanışlıdır. Örneğin, haftanın bir gününü veya yılın bir ayını temsil etmek için kullanılabilirler. Bitsel işlemler kullanılarak birleştirilebilen veya kontrol edilebilen bayraklar olarak da kullanılabilirler.

## Bayrak olarak numaralandır
"FlagsAttribute", "ToString()"in davranışını numaralandırmanın doğasına uyacak şekilde değiştirerek bir numaralandırmaya uygulanabilir:

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

"FlagsAttribute", numaralandırma sabitlerinin ikinin (ya da bunların birleşimlerinin) kuvvetlerine dayandığından ve numaralandırma değerleri nihai olarak sayısal değerler olduğundan, temel alınan sayısal türün boyutuyla sınırlandırılırsınız. Kullanabileceğiniz en büyük sayısal tür, 64 farklı (birleştirilmemiş) bayrak numaralandırma sabiti belirtmenize izin veren 'UInt64'tür. "enum" anahtar sözcüğü varsayılan olarak "Int32" olan temel "int" türünü kullanır. Derleyici, 32 bitten daha geniş değerlerin bildirilmesine izin verecektir. Bunlar bir uyarı olmadan sarılacak ve aynı değere sahip iki veya daha fazla numaralandırma üyesiyle sonuçlanacaktır. Bu nedenle, bir enum 32'den fazla bayraktan oluşan bir bit kümesini barındıracaksa, açıkça daha büyük bir tür belirtmeniz gerekir:

    public enum BigEnum : ulong
    {
        BigValue = 1 << 63
    }

Bayraklar genellikle tek bir bit olmasına rağmen, daha kolay kullanım için adlandırılmış "kümeler" halinde birleştirilebilirler.

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

İkinin kuvvetlerinin ondalık değerlerini hecelemekten kaçınmak için, [sol kaydırma operatörü (<<)](https://msdn.microsoft.com/en-gb/library/a1sway8w.aspx) bildirmek için de kullanılabilir aynı numara

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

C# 7.0 ile başlayarak, [ikili değişmez değerler](https://www.wikiod.com/tr/docs/c%23/1936/c-sharp-7-0-features/6327/binary-literals#t=201705181538117083427) de kullanılabilir .

Enum değişkeninin değerinin belirli bir bayrak kümesine sahip olup olmadığını kontrol etmek için [`HasFlag`][1] yöntemi kullanılabilir. Diyelim ki elimizde

    [Flags]
    enum MyEnum
    {
        One = 1,
        Two = 2,
        Three = 4
    }

Ve bir "değer"
    
    var value = MyEnum.One | MyEnum.Two;

`HasFlag` ile bayraklardan herhangi birinin ayarlanıp ayarlanmadığını kontrol edebiliriz.
    
    if(value.HasFlag(MyEnum.One))
        Console.WriteLine("Enum has One");

    if(value.HasFlag(MyEnum.Two))
        Console.WriteLine("Enum has Two");

    if(value.HasFlag(MyEnum.Three))
        Console.WriteLine("Enum has Three");

Ayrıca, ayarlanan tüm bayrakları almak için tüm enum değerlerini yineleyebiliriz.

    var type = typeof(MyEnum);
    var names = Enum.GetNames(type);

    foreach (var name in names)
    {
        var item = (MyEnum)Enum.Parse(type, name);

        if (value.HasFlag(item))
            Console.WriteLine("Enum has " + name);
    }
    
Veya

    foreach(MyEnum flagToCheck in Enum.GetValues(typeof(MyEnum)))
    {
        if(value.HasFlag(flagToCheck))
        {
             Console.WriteLine("Enum has " + flagToCheck);
        }
    }

Üç örnek de yazdırılacaktır:

    Enum has One
    Enum has Two


[1]: https://msdn.microsoft.com/en-us/library/system.enum.hasflag(v=vs.110).aspx

## Enum ile ilgili temel bilgiler

[MSDN][1]'den:
> Bir numaralandırma türü (ayrıca numaralandırma veya numaralandırma olarak da adlandırılır), **bir değişkene** atanabilecek** bir dizi adlandırılmış **integral sabitler** tanımlamanın etkili bir yolunu sağlar.

Esasen, bir numaralandırma, yalnızca bir dizi sonlu seçeneğe izin veren bir türdür ve her seçenek bir sayıya karşılık gelir. Varsayılan olarak, bu sayılar, sıfırdan başlayarak değerlerin bildirildiği sırayla artar. Örneğin, haftanın günleri için bir numaralandırma bildirilebilir:

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

Bu numaralandırma şu şekilde kullanılabilir:

    // Define variables with values corresponding to specific days
    Day myFavoriteDay = Day.Friday;
    Day myLeastFavoriteDay = Day.Monday;
    
    // Get the int that corresponds to myFavoriteDay
    // Friday is number 4
    int myFavoriteDayIndex = (int)myFavoriteDay;
    
    // Get the day that represents number 5
    Day dayFive = (Day)5;

Varsayılan olarak, "enum"daki her öğenin temel türü "int"tir, ancak "byte", "sbyte", "short", "ushort", "uint", "long" ve "ulong" şu şekilde kullanılabilir: kuyu. 'int' dışında bir tür kullanırsanız, türü numaralandırma adından sonra iki nokta üst üste kullanarak belirtmelisiniz:

    public enum Day : byte 
    {
        // same as before 
    }

Addan sonraki sayılar artık tamsayılar yerine baytlardır. Enum'un temel türünü aşağıdaki gibi alabilirsiniz:

    Enum.GetUnderlyingType(typeof(Days)));

Çıktı:

<!-- dil: yok -->
    System.Byte

Demo: [.NET keman][2]

[1]: https://msdn.microsoft.com/en-us/library/cc138362.aspx

[2]: https://dotnetfiddle.net/EGi301

## Bayraklar için << gösterimini kullanma
Sol kaydırma operatörü (`<<`), bayrakların olması gerektiği gibi, her bir bayrağın ikili gösterimde tam olarak bir "1"e sahip olmasını sağlamak için bayrak numaralandırma bildirimlerinde kullanılabilir.

Bu ayrıca, içinde çok sayıda bayrak bulunan büyük numaralandırmaların okunabilirliğini artırmaya da yardımcı olur.


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

Artık 'MyEnum'un yalnızca uygun bayraklar içerdiği ve uygun olmayan 'Flag30 = 1073741822' (veya ikili olarak 111111111111111111111111111110) gibi dağınık şeyleri içermediği açıktır.

## Bitsel mantıkla bayrak stili numaralandırma değerlerini test edin
Bayrak stili bir numaralandırma değeri, herhangi bir tek değerle eşleşmeyebileceğinden, bitsel mantıkla test edilmelidir.

    [Flags]
    enum FlagsEnum
    {
        Option1 = 1,
        Option2 = 2,
        Option3 = 4,
        Option2And3 = Option2 | Option3;
    
        Default = Option1 | Option3,
    }
    
"Varsayılan" değer, aslında, bit düzeyinde VEYA ile _birleştirilmiş_ iki diğer değerin birleşimidir. Bu nedenle, bir bayrağın varlığını test etmek için bitsel AND kullanmamız gerekir.

    var value = FlagsEnum.Default;

    bool isOption2And3Set = (value & FlagsEnum.Option2And3) == FlagsEnum.Option2And3;

    Assert.True(isOption2And3Set);



## Dizeye ve geriye doğru numaralandır
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

## İşaretli numaralandırmadan değerler ekleyin ve kaldırın
Bu kod, işaretli bir numaralandırma örneğinden bir değer eklemek ve kaldırmak içindir:

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


## enum için varsayılan değer == SIFIR
** Bir numaralandırma için varsayılan değer sıfırdır**. Bir numaralandırma, sıfır değerine sahip bir öğe tanımlamıyorsa, varsayılan değeri sıfır olacaktır.
    
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

Örnek:
https://dotnetfiddle.net/l5Rwie

## Bir numaralandırma değerine ek açıklama bilgisi ekleme
Bazı durumlarda, örneğin enum değerinin kendisi kullanıcıya göstermek isteyebileceğinizden daha az okunabilir olduğunda, bir numaralandırma değerine ek bir açıklama eklemek isteyebilirsiniz. Bu gibi durumlarda [`System.ComponentModel.DescriptionAttribute`](https://msdn.microsoft.com/en-us/library/system.componentmodel.descriptionattribute(v=vs.110).aspx) sınıfını kullanabilirsiniz.

Örneğin:

    public enum PossibleResults
    {
        [Description("Success")]
        OK = 1,
        [Description("File not found")]
        FileNotFound = 2,
        [Description("Access denied")]
        AccessDenied = 3
    }

Şimdi, belirli bir enum değerinin açıklamasını döndürmek isterseniz aşağıdakileri yapabilirsiniz:

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

Bu, tüm numaralandırmalar için kolayca bir uzatma yöntemine dönüştürülebilir:

    static class EnumExtensions
    {
        public static string GetDescription(this Enum enumValue)
        {
            return ((DescriptionAttribute)Attribute.GetCustomAttribute((enumValue.GetType().GetField(enumValue.ToString())), typeof(DescriptionAttribute))).Description;
        }
    }

Ve sonra kolayca bu şekilde kullanılır:
`Console.WriteLine(result.GetDescription());`


## Numaralandırmalar beklenmeyen değerlere sahip olabilir
Bir numaralandırma, temel alınan integral türünden ve bu türe dönüştürülebildiğinden, değer, numaralandırma türünün tanımında verilen değer aralığının dışında kalabilir.

Aşağıdaki enum türü 'DaysOfWeek' yalnızca 7 tanımlı değere sahip olsa da, yine de herhangi bir 'int' değerini tutabilir.

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

Şu anda bu davranışa sahip olmayan bir numaralandırma tanımlamanın bir yolu yoktur.

Ancak, tanımsız enum değerleri `Enum.IsDefined` yöntemi kullanılarak tespit edilebilir. Örneğin,

    DaysOfWeek d = (DaysOfWeek)31;
    Console.WriteLine(Enum.IsDefined(typeof(DaysOfWeek),d)); // prints False

## Bir numaralandırmanın tüm üye değerlerini alın
    enum MyEnum
    {
        One,
        Two,
        Three
    }
    
    foreach(MyEnum e in Enum.GetValues(typeof(MyEnum)))
        Console.WriteLine(e);

Bu yazdıracak:

    One
    Two
    Three

## Numaralandırma kullanarak Bitsel Manipülasyon
[FlagsAttribute][1], numaralandırılabilir tek bir değer yerine bir bayrak koleksiyonunu temsil ettiğinde kullanılmalıdır.
Her bir numaralandırma değerine atanan sayısal değer, bitsel operatörler kullanılarak numaralandırmalar işlenirken yardımcı olur.


**Örnek 1: [Bayraklar] ile**

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

> Kırmızı, Mavi yazdırır

    

****Örnek 2: [Bayraklar] olmadan****

  
    enum Colors
    {
        Red=1,
        Blue=2,
        Green=4,
        Yellow=8
    }
    var color = Colors.Red | Colors.Blue;
    Console.WriteLine(color.ToString());

> 3 yazdırır


[1]: https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx

