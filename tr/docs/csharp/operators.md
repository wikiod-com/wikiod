---
title: "operatörler"
slug: "operatorler"
draft: false
images: []
weight: 9543
type: docs
toc: true
---

C#'ta bir [operatör](https://docs.microsoft.com/en-us/dotnet/csharp/program), bir ifade veya ifadedeki bir veya daha fazla işlenene uygulanan bir program öğesidir. Arttırma operatörü (++) veya yeni gibi bir işlenen alan operatörlere birli operatörler denir. Aritmetik operatörler (+,-,*,/) gibi iki işlenen alan operatörlere ikili operatörler denir. Bir operatör, koşullu operatör (?:), üç işlenen alır ve C#'daki tek üçlü operatördür.



## Sözdizimi
- genel statik OperandType operatör operatörüSymbol(OperandType operand1)
- genel statik OperandType operatör operatörüSymbol(OperandType operand1, OperandType2 operand2)

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| operatörSembol | Operatör aşırı yükleniyor, örn. +, -, /, * |
| OperandType | Aşırı yüklenmiş operatör tarafından döndürülecek tür.
| işlenen1 | İşlemi gerçekleştirirken kullanılacak ilk işlenen.
| işlenen2 | İkili işlemler yapılırken işlemi gerçekleştirirken kullanılacak ikinci işlenen.
| ifadeler | Sonucu döndürmeden önce işlemi gerçekleştirmek için isteğe bağlı kod gerekir.

Tüm operatörler 'statik yöntemler' olarak tanımlanır ve 'sanal' değildirler ve kalıtsal değildirler.
### Operatör Önceliği

Tüm operatörlerin, operatörün hangi gruba dahil olduğuna bağlı olarak belirli bir "önceliği" vardır (aynı grubun operatörleri eşit önceliğe sahiptir). Yani bazı operatörler diğerlerinden önce uygulanacaktır. Aşağıda, önceliğe göre sıralanmış (ilk önce en yüksek) grupların (ilgili operatörlerini içeren) bir listesi bulunmaktadır:

* **Birincil Operatörler**
* `a.b` - Üye erişimi.
* `a?.b` - Boş koşullu üye erişimi.
* `->` - Üye erişimiyle birleştirilmiş işaretçi referansı kaldırma.
* `f(x)` - İşlev çağırma.
* "a[x]" - Dizin Oluşturucu.
* `a?[x]` - Boş koşullu dizin oluşturucu.
* `x++` - Sonek artışı.
* `x--` - Son ek azaltma.
* "yeni" - Örnekleme yazın.
* `default(T)` - `T` türünün varsayılan başlatılmış değerini döndürür.
* 'typeof' - İşlenenin 'Type' nesnesini döndürür.
* `checked` - Sayısal taşma kontrolünü etkinleştirir.
* "işaretsiz" - Sayısal taşma denetimini devre dışı bırakır.
* "delegate" - Bir temsilci örneği bildirir ve döndürür.
* `sizeof` - İşlenen türünün bayt cinsinden boyutunu döndürür.

* **Birli Operatörler**
* `+x` - `x` değerini döndürür.
* `-x` - Sayısal olumsuzlama.
* `!x` - Mantıksal olumsuzlama.
* `~x' - Bit düzeyinde tamamlayıcı/yıkıcıları bildirir.
* `++x` - Ön ek artışı.
* `--x` - Ön ek azaltma.
* `(T)x` - Tip döküm.
* 'bekliyor' - Bir 'Görev' Bekliyor.
* `&x` - `x` adresini (işaretçi) döndürür.
* `*x` - İşaretçi referansını kaldırma.

* **Çarpıcı Operatörler**
* `x * y' - Çarpma.
* `x / y' - Bölüm.
* `x % y` - Modül.

* **Katkı Operatörleri**
* `x + y` - Toplama.
* `x – y' - Çıkarma.

* **Bitsel Kaydırma Operatörleri**
* `x << y' - Bitleri sola kaydır.
* `x >> y` - Bitleri sağa kaydır.

* **İlişkisel/Tip Test Operatörleri**
* `x < y' - Şundan küçük.
* `x > y' - Şundan büyüktür.
* `x <= y' - Küçüktür veya eşittir.
* `x >= y` - Büyüktür veya eşittir.
* `is` - Tür uyumluluğu.
* `as` - Tür dönüştürme.

* **Eşitlik Operatörleri**
* `x == y' - Eşitlik.
* `x != y` - Eşit değil.

* **Mantıksal VE Operatör**
* `x & y` - Mantıksal/bit düzeyinde VE.

* **Mantıksal XOR Operatörü**
* `x ^ y' - Mantıksal/bit düzeyinde XOR.

* **Mantıksal VEYA Operatör**
* `x | y` - Mantıksal/bit düzeyinde VEYA.

* **Koşullu VE Operatör**
* `x && y' - Mantıksal VE'yi kısa devre yapar.

* **Koşullu VEYA Operatör**
* `x || y` - Kısa devre mantıksal VEYA.

* **Boş birleştirici Operatör**
* `x ?? y` - Boş değilse "x" değerini döndürür; aksi takdirde, 'y' döndürür.

* **Koşullu Operatör**
* `x ? y : z` - "x" doğruysa "y"yi değerlendirir/döndürür; aksi takdirde, "z"yi değerlendirir.


---

**İlgili İçerik**

- [Boş Birleştirme Operatörü][1]
- [Boş-Koşullu Operatör][2]
- [Operatör adı][3]

[1]: https://www.wikiod.com/tr/docs/c%23/37/null-coalescing-operator#t=2015112322329424573937
[2]: https://www.wikiod.com/tr/docs/c%23/41/the-null-conditional-operator#t=201511232329445644147
[3]: https://www.wikiod.com/tr/docs/c%23/80/nameof-operator#t=201608081725023270827

## Aşırı Yüklenebilir Operatörler
C#, 'operatör' anahtar sözcüğünü kullanarak statik üye işlevleri tanımlayarak kullanıcı tanımlı türlerin operatörleri aşırı yüklemesine izin verir.
Aşağıdaki örnek, "+" operatörünün bir uygulamasını göstermektedir.

Karmaşık bir sayıyı temsil eden bir "Karmaşık" sınıfımız varsa:

    public struct Complex
    {
        public double Real { get; set; }
        public double Imaginary { get; set; }
    }

Ve bu sınıf için `+` operatörünü kullanma seçeneğini eklemek istiyoruz. yani:

    Complex a = new Complex() { Real = 1, Imaginary = 2 };
    Complex b = new Complex() { Real = 4, Imaginary = 8 };
    Complex c = a + b;

Sınıf için '+' operatörünü aşırı yüklememiz gerekecek. Bu, statik bir işlev ve "operatör" anahtar sözcüğü kullanılarak yapılır:

    public static Complex operator +(Complex c1, Complex c2)
    {
       return new Complex 
       { 
           Real = c1.Real + c2.Real,
           Imaginary = c1.Imaginary + c2.Imaginary 
       };
    }

`+`, `-`, `*`, `/` gibi operatörlerin tümü aşırı yüklenebilir. Bu, aynı türü döndürmeyen Operatörleri de içerir (örneğin, `==` ve `!=` boolean döndürmesine rağmen aşırı yüklenebilir) Çiftlerle ilgili aşağıdaki kural burada da uygulanır.

Karşılaştırma operatörleri çiftler halinde aşırı yüklenmelidir (örneğin, "<" aşırı yüklenmişse, ">" de aşırı yüklenmelidir).

Aşırı yüklenebilen operatörlerin tam listesi (ayrıca aşırı yüklenemeyen operatörler ve bazı aşırı yüklenebilen operatörlere getirilen kısıtlamalar) [MSDN - Aşırı Yüklenebilir Operatörler (C# Programlama Kılavuzu)][1]'da görülebilir.

<!-- eğer sürüm [gte 7.0] -->
"operatör is"in aşırı yüklenmesi, C# 7.0'ın model eşleştirme mekanizmasıyla tanıtıldı. Ayrıntılar için bkz. [Desen Eşleştirme][2]

Aşağıdaki gibi tanımlanmış bir "Kartezyen" türü verildi

    public class Cartesian
    {
        public int X { get; }
        public int Y { get; }
    }   

Aşırı yüklenebilir bir "operatör" ör. 'Kutup' koordinatları için tanımlanmalıdır

    public static class Polar
    {
        public static bool operator is(Cartesian c, out double R, out double Theta)
        {
            R = Math.Sqrt(c.X*c.X + c.Y*c.Y);
            Theta = Math.Atan2(c.Y, c.X);
            return c.X != 0 || c.Y != 0;
        }
    }

hangi böyle kullanılabilir

    var c = Cartesian(3, 4);
    if (c is Polar(var R, *))
    {
        Console.WriteLine(R);
    }

(Örnek [Roslyn Pattern Matching Documentation][3]'den alınmıştır)
<!-- eğer --> son sürüm

[1]: https://msdn.microsoft.com/en-us/library/8edha89s.aspx
[2]: https://www.wikiod.com/tr/docs/c%23/1936/c-sharp-7-0-features/13323/pattern-matching#t=201608081959042378203
[3]: https://github.com/dotnet/roslyn/blob/future/docs/features/patterns.md

## Eşitlik operatörlerini aşırı yükleme
Sadece eşitlik operatörlerini aşırı yüklemek yeterli değildir. Farklı koşullar altında, aşağıdakilerin tümü çağrılabilir:

1. "object.Equals" ve "object.GetHashCode"
2. `IEquatable<T>.Equals` (isteğe bağlı, bokstan kaçınmaya izin verir)
3. `operatör ==` ve `operatör !=` (isteğe bağlı, operatörlerin kullanımına izin verir)

"Equals" geçersiz kılınırken, "GetHashCode" da geçersiz kılınmalıdır. "Eşittir"i uygularken birçok özel durum vardır: farklı türdeki nesnelerle karşılaştırma, kendiyle karşılaştırma vb.

'Equals' yöntemi ve '==' operatörü geçersiz kılınmadığı zaman, sınıflar ve yapılar için farklı davranır. Sınıflar için sadece referanslar karşılaştırılır ve yapılar için özelliklerin değerleri, performansı olumsuz yönde etkileyebilecek yansıma yoluyla karşılaştırılır. `==`, geçersiz kılınmadıkça yapıları karşılaştırmak için kullanılamaz.

Genel olarak eşitlik işlemi aşağıdaki kurallara uymalıdır:

- *İstisnalar atılmamalıdır*.
- Yansıma: "A" her zaman "A"ya eşittir (bazı sistemlerde "NULL" değerleri için doğru olmayabilir).
- Geçişlilik: "A", "B"ye eşitse ve "B", "C"ye eşitse, o zaman "A", "C"ye eşittir.
- "A", "B"ye eşitse, "A" ve "B" eşit karma kodlara sahiptir.
- Kalıtım ağacı bağımsızlığı: "B" ve "C", "Sınıf1"den devralınan "Sınıf2" örnekleriyse:
`Class1.Equals(A,B)` her zaman `Class2.Equals(A,B)` çağrısıyla aynı değeri döndürmelidir.
      

    class Student : IEquatable<Student>
    {
        public string Name { get; set; } = "";
    
        public bool Equals(Student other)
        {
            if (ReferenceEquals(other, null)) return false;
            if (ReferenceEquals(other, this)) return true;
            return string.Equals(Name, other.Name);
        }
    
        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;

            return Equals(obj as Student);
        }
    
        public override int GetHashCode()
        {
            return Name?.GetHashCode() ?? 0;
        }
    
        public static bool operator ==(Student left, Student right)
        {
            return Equals(left, right);
        }
    
        public static bool operator !=(Student left, Student right)
        {
            return !Equals(left, right);
        }
    }

## İlişkisel Operatörler
**Eşittir**

Sağlanan işlenenlerin (argümanların) eşit olup olmadığını kontrol eder

    "a" == "b"     // Returns false.
    "a" == "a"     // Returns true.
    1 == 0         // Returns false.
    1 == 1         // Returns true.
    false == true  // Returns false.
    false == false // Returns true.

Java'dan farklı olarak, eşitlik karşılaştırma operatörü dizelerle yerel olarak çalışır.

Eşitlik karşılaştırma operatörü, birinden diğerine örtük bir döküm varsa, farklı türlerdeki işlenenlerle çalışacaktır. Uygun bir örtük atama yoksa, açık bir atama çağırabilir veya uyumlu bir türe dönüştürmek için bir yöntem kullanabilirsiniz.

    1 == 1.0              // Returns true because there is an implicit cast from int to double.
    new Object() == 1.0   // Will not compile.
    MyStruct.AsInt() == 1 // Calls AsInt() on MyStruct and compares the resulting int with 1.

Visual Basic.NET'ten farklı olarak, eşitlik karşılaştırma operatörü, eşitlik atama operatörü ile aynı değildir.

    var x = new Object();
    var y = new Object();
    x == y // Returns false, the operands (objects in this case) have different references.
    x == x // Returns true, both operands have the same reference.

<sup>*Atama operatörüyle (`=`) karıştırılmamalıdır.*</sup>

Değer türleri için, her iki işlenenin de değeri eşitse operatör "true" değerini döndürür.
Referans türleri için, her iki işlenen de *referans*'ta (değer değil) eşitse, operatör 'true' değerini döndürür. Bir istisna, dize nesnelerinin değer eşitliği ile karşılaştırılmasıdır.

**Eşit değil**

Sağlanan işlenenlerin *eşit* olup olmadığını kontrol eder.

    "a" != "b"     // Returns true.
    "a" != "a"     // Returns false.
    1 != 0         // Returns true.
    1 != 1         // Returns false.
    false != true  // Returns true.
    false != false // Returns false.

    var x = new Object();
    var y = new Object();
    x != y // Returns true, the operands have different references.
    x != x // Returns false, both operands have the same reference.

Bu operatör, eşittir (`==`) operatörünün sonucunun tersini etkin bir şekilde döndürür

**Büyüktür**

İlk işlenenin ikinci işlenenden büyük olup olmadığını kontrol eder.

    3 > 5    //Returns false.
    1 > 0    //Returns true.
    2 > 2    //Return false.
    
    var x = 10;
    var y = 15;
    x > y    //Returns false.
    y > x    //Returns true.

**Daha az**

İlk işlenenin ikinci işlenenden küçük olup olmadığını kontrol eder.

    2 < 4     //Returns true.
    1 < -3    //Returns false.
    2 < 2     //Return false.
    
    var x = 12;
    var y = 22;
    x < y    //Returns true.
    y < x    //Returns false.

**Eşitten Büyük**

İlk işlenenin ikinci işlenenden büyük olup olmadığını kontrol eder.

    7 >= 8    //Returns false.
    0 >= 0    //Returns true.
    
**Eşitten Az**

İlk işlenenin ikinci işlenenden küçük olup olmadığını kontrol eder.

    2 <= 4    //Returns true.
    1 <= -3    //Returns false.
    1 <= 1     //Returns true. 
    
  
  

## Örtülü Döküm ve Açık Döküm Operatörleri
C#, kullanıcı tanımlı türlerin "açık" ve "örtük" anahtar sözcüklerin kullanımı yoluyla atama ve yayınlamayı kontrol etmesine olanak tanır. Yöntemin imzası şu şekildedir:

    public static <implicit/explicit> operator <ResultingType>(<SourceType> myType)

Yöntem daha fazla argüman alamaz ve bir örnek yöntemi olamaz. Ancak, içinde tanımlandığı türden herhangi bir özel üyeye erişebilir.

Hem "örtülü" hem de "açık" oyuncu seçimine bir örnek:

    public class BinaryImage 
    {
        private bool[] _pixels;

        public static implicit operator ColorImage(BinaryImage im)
        {
            return new ColorImage(im);
        }

        public static explicit operator bool[](BinaryImage im)
        {
            return im._pixels;
        }
    }

Aşağıdaki döküm sözdizimine izin vermek:

    var binaryImage = new BinaryImage();
    ColorImage colorImage = binaryImage; // implicit cast, note the lack of type 
    bool[] pixels = (bool[])binaryImage; // explicit cast, defining the type

Cast operatörleri, sizin türünüzden *gidip* sizin türünüze *giderek her iki şekilde de çalışabilir:

    public class BinaryImage
    {
        public static explicit operator ColorImage(BinaryImage im)
        {
            return new ColorImage(im);
        }

        public static explicit operator BinaryImage(ColorImage cm)
        {
            return new BinaryImage(cm);
        }
    }

Son olarak, bir tür hiyerarşisi içinde yayınlamaya dahil olabilecek `as` anahtar sözcüğü bu durumda **değildir**. "Açık" veya "örtük" bir atama tanımladıktan sonra bile şunları yapamazsınız:

    ColorImage cm = myBinaryImage as ColorImage;

Derleme hatası üretecektir.

## Kısa devre yapan Operatörler
*Tanım olarak, kısa devre yapan boole işleçleri, yalnızca ilk işlenen ifadenin genel sonucunu belirleyemiyorsa ikinci işleneni değerlendirecektir.*

Bunun anlamı, && operatörünü *firstCondition && secondCondition* olarak kullanıyorsanız, *secondCondition*'ı yalnızca *firstCondition* doğru olduğunda değerlendireceği ve elbette genel sonucun yalnızca *firstOperand* ve *secondOperand*'ın her ikisi de değerlendirilirse doğru olacağı anlamına gelir. doğru. Bu, birçok senaryoda yararlıdır, örneğin, listenizde üçten fazla öğe varken kontrol etmek istediğinizi, ancak listenin *NullReferenceException* ile çalışmayacak şekilde başlatılıp başlatılmadığını da kontrol etmeniz gerektiğini hayal edin. Bunu aşağıdaki gibi başarabilirsiniz:

    bool hasMoreThanThreeElements = myList != null && mList.Count > 3;

*mList.Count > 3* myList != null karşılanana kadar kontrol edilmeyecektir.

**Mantıksal VE**

'&&', standart boolean AND ('&') operatörünün kısa devre yapan karşılığıdır.

    var x = true;
    var y = false;

    x && x // Returns true.
    x && y // Returns false (y is evaluated).
    y && x // Returns false (x is not evaluated).
    y && y // Returns false (right y is not evaluated).

**Mantıksal VEYA**

`||`, standart boolean OR (`|`) operatörünün kısa devre yapan karşılığıdır.

    var x = true;
    var y = false;

    x || x // Returns true (right x is not evaluated).
    x || y // Returns true (y is not evaluated).
    y || x // Returns true (x and y are evaluated).
    y || y // Returns false (y and y are evaluated).

**Örnek kullanım**

    if(object != null && object.Property)
    // object.Property is never accessed if object is null, because of the short circuit.
        Action1();
    else
        Action2();

## ? : Üçlü operatör
Boolean ifadesinin değerine bağlı olarak iki değerden birini döndürür.

Sözdizimi:

    condition ? expression_if_true : expression_if_false;

Örnek:

    string name = "Frank";
    Console.WriteLine(name == "Frank" ? "The name is Frank" : "The name is not Frank");

Üçlü operatör, bileşik üçlü ifadelerin kullanılmasına izin veren sağ ilişkiseldir. Bu, bir ana üçlü denklemin doğru veya yanlış konumuna ek üçlü denklemler eklenerek yapılır. Okunabilirliği sağlamak için özen gösterilmelidir, ancak bu, bazı durumlarda kısa yoldan yararlı olabilir.

Bu örnekte, bir bileşik üçlü işlem bir "kelepçe" işlevini değerlendirir ve aralık içindeyse geçerli değeri, aralığın altındaysa "min" değerini veya aralığın üstündeyse "maks" değerini döndürür.

    light.intensity = Clamp(light.intensity, minLight, maxLight);

    public static float Clamp(float val, float min, float max)
    {
        return (val < min) ? min : (val > max) ? max : val;
    }

Üçlü operatörler de iç içe yerleştirilebilir, örneğin:

    a ? b ? "a is true, b is true" : "a is true, b is false" : "a is false"
    
    // This is evaluated from left to right and can be more easily seen with parenthesis:
    
    a ? (b ? x : y) : z

    // Where the result is x if a && b, y if a && !b, and z if !a

Bileşik üçlü ifadeler yazarken, okunabilirliği artırmak için parantez veya girinti kullanmak yaygındır.

*expression_if_true* ve *expression_if_false* türleri aynı olmalıdır veya birinden diğerine örtük bir dönüşüm olmalıdır.

    condition ? 3 : "Not three"; // Doesn't compile because `int` and `string` lack an implicit conversion.

    condition ? 3.ToString() : "Not three"; // OK because both possible outputs are strings.

    condition ? 3 : 3.5; // OK because there is an implicit conversion from `int` to `double`. The ternary operator will return a `double`.

    condition ? 3.5 : 3; // OK because there is an implicit conversion from `int` to `double`. The ternary operator will return a `double`.

Tür ve dönüştürme gereksinimleri kendi sınıflarınız için de geçerlidir.

    public class Car
    {}

    public class SportsCar : Car
    {}

    public class SUV : Car
    {}

    condition ? new SportsCar() : new Car(); // OK because there is an implicit conversion from `SportsCar` to `Car`. The ternary operator will return a reference of type `Car`.

    condition ? new Car() : new SportsCar(); // OK because there is an implicit conversion from `SportsCar` to `Car`. The ternary operator will return a reference of type `Car`.

    condition ? new SportsCar() : new SUV(); // Doesn't compile because there is no implicit conversion from `SportsCar` to SUV or `SUV` to `SportsCar`. The compiler is not smart enough to realize that both of them have an implicit conversion to `Car`.

    condition ? new SportsCar() as Car : new SUV() as Car; // OK because both expressions evaluate to a reference of type `Car`. The ternary operator will return a reference of type `Car`.


##?. (Boş Koşullu Operatör)
<!-- eğer [gte 6.0] versiyonu -->

[C# 6.0'da tanıtılmıştır][1], Null Koşullu İşleç `?.`, sol tarafındaki ifade bir `NullReferenceException` atmak yerine `null` olarak değerlendirilirse hemen `null` döndürür. Sol tarafı "boş" olmayan bir değer olarak değerlendirilirse, normal bir "." operatörü gibi işlem görür. 'null' döndürebileceğinden, dönüş türünün her zaman null yapılabilir bir tür olduğuna dikkat edin. Bu, bir yapı veya ilkel tür için bir 'Nullable<T>' içine sarıldığı anlamına gelir.

    var bar = Foo.GetBar()?.Value; // will return null if GetBar() returns null
    var baz = Foo.GetBar()?.IntegerValue; // baz will be of type Nullable<int>, i.e. int?
  
Bu, olayları tetiklerken kullanışlı olur. Normalde olay çağrısını 'null' için kontrol eden bir if ifadesine sarmanız ve daha sonra olayı yükseltmeniz gerekir, bu da bir yarış koşulu olasılığını ortaya çıkarır. Null koşul operatörü kullanılarak bu, aşağıdaki şekilde düzeltilebilir:

    event EventHandler<string> RaiseMe;
    RaiseMe?.Invoke("Event raised");

<!-- eğer --> son sürüm


[1]: https://www.wikiod.com/tr/docs/c%23/24/c-sharp-6-0-features/51/null-propagation#t=201607301051500162149

## boyutu
Bayt cinsinden<sup>*</sup> türünün boyutunu tutan bir "int" döndürür.

    sizeof(bool)    // Returns 1.
    sizeof(byte)    // Returns 1.
    sizeof(sbyte)   // Returns 1.
    sizeof(char)    // Returns 2.
    sizeof(short)   // Returns 2.
    sizeof(ushort)  // Returns 2.
    sizeof(int)     // Returns 4.
    sizeof(uint)    // Returns 4.
    sizeof(float)   // Returns 4.
    sizeof(long)    // Returns 8.
    sizeof(ulong)   // Returns 8.
    sizeof(double)  // Returns 8.
    sizeof(decimal) // Returns 16.

<sup>**Yalnızca belirli ilkel türleri güvenli bağlamda destekler.*</sup>

Güvenli olmayan bir bağlamda, diğer ilkel türlerin ve yapıların boyutunu döndürmek için "sizeof" kullanılabilir.

    public struct CustomType
    {
        public int value;
    }

    static void Main()
    {
        unsafe
        {
            Console.WriteLine(sizeof(CustomType)); // outputs: 4
        }
    }

## Sınıf Üyesi Operatörler: Null Koşullu Üye Erişimi
    var zipcode = myEmployee?.Address?.ZipCode;
    //returns null if the left operand is null.  
    //the above is the equivalent of:
    var zipcode = (string)null;
    if (myEmployee != null && myEmployee.Address != null)
        zipcode = myEmployee.Address.ZipCode;

## Sınıf Üyesi Operatörler: Null Koşullu İndeksleme
    var letters = null;
    char? letter = letters?[1];
    Console.WriteLine("Second Letter is {0}",letter);
    //in the above example  rather than throwing an error because letters is null
    //letter is assigned the value null

## "Özel veya" Operatör
Bir "exclusive or" (kısaca XOR için) operatörü: ^

Bu operatör, sağlanan boollerden biri, ancak yalnızca biri doğru olduğunda true değerini döndürür.

    true ^ false   // Returns true
    false ^ true   // Returns true
    false ^ false  // Returns false
    true ^ true    // Returns false

## Bit Kaydırma Operatörleri
Kaydırma operatörleri, programcıların tüm bitlerini sola veya sağa kaydırarak bir tamsayıyı ayarlamasına izin verir. Aşağıdaki şema, bir değeri bir basamak sola kaydırmanın etkisini göstermektedir.

**Sol shift**

    uint value = 15;              // 00001111
     
    uint doubled = value << 1;    // Result = 00011110 = 30
    uint shiftFour = value << 4;  // Result = 11110000 = 240

**Sağa kaydırma**

    uint value = 240;             // 11110000
     
    uint halved = value >> 1;     // Result = 01111000 = 120
    uint shiftFour = value >> 4;  // Result = 00001111 = 15

## varsayılan Operatör
Değer Türü (burada T : struct)
---
"char", "int" ve "float" gibi yerleşik ilkel veri türlerinin yanı sıra "struct" veya "enum" ile bildirilen kullanıcı tanımlı türler. Varsayılan değerleri "new T()" dir:

    default(int)            // 0
    default(DateTime)       // 0001-01-01 12:00:00 AM
    default(char)           // '\0' This is the "null character", not a zero or a line break.
    default(Guid)           // 00000000-0000-0000-0000-000000000000
    default(MyStruct)       // new MyStruct()

    // Note: default of an enum is 0, and not the first *key* in that enum
    // so it could potentially fail the Enum.IsDefined test
    default(MyEnum)         // (MyEnum)0

Referans Türü (burada T : sınıf)
---

Herhangi bir "sınıf", "arayüz", dizi veya temsilci türü. Varsayılan değerleri "null"dur:

    default(object)         // null
    default(string)         // null
    default(MyClass)        // null
    default(IDisposable)    // null
    default(dynamic)        // null

## Sonek ve Önek artırma ve azaltma
Son düzeltme artışı "X++", "x"e "1" ekleyecektir

    var x = 42;
    x++;
    Console.WriteLine(x); // 43

Son ek azaltma 'X--' bir çıkarır

    var x = 42
    x--; 
    Console.WriteLine(x); // 41



'++x' önek artışı olarak adlandırılır, x'in değerini artırır ve sonra x'i döndürür
"x++", x'in değerini döndürür ve ardından artar

    var x = 42;
    Console.WriteLine(++x); // 43
    System.out.println(x); // 43

süre

    var x = 42;
    Console.WriteLine(x++); // 42
    System.out.println(x); // 43

her ikisi de for döngüsünde yaygın olarak kullanılır

    for(int i = 0; i < 10; i++)
    {
    }


## => Lambda operatörü
<!-- eğer sürüm [gte 3.0] -->

*`=>` operatörü, `=` atama operatörü ile aynı önceliğe sahiptir ve sağ ilişkiseldir.*

Lambda ifadelerini bildirmek için kullanılır ve ayrıca [LINQ Sorguları](https://www.wikiod.com/tr/docs/c%23/68/linq-queries/4735/basics#t=201607251514251028068) ile yaygın olarak kullanılır:

    string[] words = { "cherry", "apple", "blueberry" };

    int shortestWordLength = words.Min((string w) => w.Length); //5

LINQ uzantılarında veya sorgularında kullanıldığında, derleyici tarafından çıkarıldığı için nesnelerin türü genellikle atlanabilir:

    int shortestWordLength = words.Min(w => w.Length); //also compiles with the same result

Lambda operatörünün genel formu şu şekildedir:

    (input parameters) => expression

Lambda ifadesinin parametreleri `=>` operatöründen önce belirtilir ve yürütülecek gerçek ifade/ifade/blok operatörün sağındadır:

    // expression
    (int x, string s) => s.Length > x

    // expression
    (int x, int y) => x + y

    // statement
    (string x) => Console.WriteLine(x)

    // block
    (string x) => {
            x += " says Hello!";
            Console.WriteLine(x);
        }

Bu operatör, açık bir yöntem yazmadan delegeleri kolayca tanımlamak için kullanılabilir:

    delegate void TestDelegate(string s);
    
    TestDelegate myDelegate = s => Console.WriteLine(s + " World");

    myDelegate("Hello");

onun yerine

    void MyMethod(string s)
    {
        Console.WriteLine(s + " World");
    }
    
    delegate void TestDelegate(string s);

    TestDelegate myDelegate = MyMethod;

    myDelegate("Hello");

<!-- eğer --> son sürüm

## Atama operatörü '='
Atama operatörü `=`, sol taraftaki işlenenin değerini sağ taraftaki işlenenin değerine ayarlar ve bu değeri döndürür:

    int a = 3;     // assigns value 3 to variable a
    int b = a = 5; // first assigns value 5 to variable a, then does the same for variable b
    Console.WriteLine(a = 3 + 4); // prints 7


## ?? Null-Coalescing Operatörü
Null-Coalescing operatörü `??` null olmadığında sol tarafa dönecektir. Eğer null ise, sağ tarafa dönecektir.

    object foo = null;
    object bar = new object();
    
    var c = foo ?? bar;
    //c will be bar since foo was null

`??` operatörü, `if` kontrollerinin kaldırılmasına izin veren zincirlenebilir.

    //config will be the first non-null returned.
    var config = RetrieveConfigOnMachine() ??
                 RetrieveConfigFromService() ??
                 new DefaultConfiguration();





## Sınıf Üye Operatörleri: Üye Erişimi
    var now = DateTime.UtcNow;
    //accesses member of a class.  In this case the UtcNow property.

## Sınıf Üyesi Operatörler: İşlev Çağırma
    var age = GetAge(dateOfBirth);
    //the above calls the function GetAge passing parameter dateOfBirth.

## Sınıf Üyesi Operatörler: Toplu Nesne Dizin Oluşturma
    var letters = "letters".ToCharArray();
    char letter = letters[1];
    Console.WriteLine("Second Letter is {0}",letter);
    //in the above example we take the second character from the array
    //by calling letters[1]
    //NB: Array Indexing starts at 0; i.e. the first letter would be given by letters[0].

## Atamalı ikili operatörler
C#, operatörün sonucunu değerlendirmek ve ardından sonucu orijinal değişkene atamak için bir `=` işaretiyle birleştirilebilen birkaç operatöre sahiptir.

Örnek:

    x += y

aynıdır

    x = x + y

Atama operatörleri:

 - `+=`
 - `-=`
 - `*=`
 - `/=`
 - `%=`
 - `&=`
 - `|=`
 - `^=`
 - `<<=`
 - `>>=`

## bir çeşit
Bir tür için `System.Type' nesnesini alır.
     
    System.Type type = typeof(Point)        //System.Drawing.Point      
    System.Type type = typeof(IDisposable)  //System.IDisposable
    System.Type type = typeof(Colors)       //System.Drawing.Color
    System.Type type = typeof(List<>)       //System.Collections.Generic.List`1[T]

Çalışma zamanı türünü almak için, geçerli örneğin 'System.Type'ını elde etmek için 'GetType' yöntemini kullanın.

Operatör 'typeof', derleme zamanında belirtilen parametre olarak bir tür adı alır.

    public class Animal {} 
    public class Dog : Animal {}
    
    var animal = new Dog();

    Assert.IsTrue(animal.GetType() == typeof(Animal)); // fail, animal is typeof(Dog) 
    Assert.IsTrue(animal.GetType() == typeof(Dog));    // pass, animal is typeof(Dog)
    Assert.IsTrue(animal is Animal);                   // pass, animal implements Animal


## Operatörün adı
Bir "değişken", "tür" veya "üye"nin niteliksiz adını temsil eden bir dize döndürür.

    int counter = 10;
    nameof(counter); // Returns "counter"
    Client client = new Client();
    nameof(client.Address.PostalCode)); // Returns "PostalCode"

'nameof' operatörü C# 6.0'da tanıtıldı. Derleme zamanında değerlendirilir ve döndürülen dize değeri derleyici tarafından satır içi olarak eklenir, bu nedenle sabit dizenin kullanılabileceği çoğu durumda kullanılabilir (örneğin, bir "switch" ifadesindeki "case" etiketleri, nitelikler , vb...). İstisnaları, nitelikleri, MVC Eylem bağlantılarını vb. yükseltme ve günlüğe kaydetme gibi durumlarda yararlı olabilir...

