---
title: "C# 7.0 Özellikleri"
slug: "c-70-ozellikleri"
draft: false
images: []
weight: 1717
type: docs
toc: true
---

C# 7.0, C#'ın yedinci sürümüdür. Bu sürüm bazı yeni özellikler içerir: Tuple'lar için dil desteği, yerel işlevler, 'out var' bildirimleri, rakam ayırıcılar, ikili değişmezler, kalıp eşleştirme, atma ifadeleri, 'başvuru dönüşü' ve 'başvuru yerel' ve genişletilmiş ifade gövdeli üye listesi.

Resmi referans: [C# 7'deki yenilikler](https://docs.microsoft.com/en-us/dotnet/articles/csharp/csharp-7)

## Tuples için dil desteği
# Temel bilgiler

Bir **tuple**, sıralı, sonlu bir eleman listesidir. Tanımlama grupları, programlamada, tanımlama grubunun öğelerinin her biri ile ayrı ayrı çalışmak yerine tek bir varlıkla toplu olarak çalışmak ve ilişkisel bir veritabanında tek tek satırları (yani "kayıtları") temsil etmek için bir araç olarak yaygın olarak kullanılır.

C# 7.0'da yöntemlerin birden çok dönüş değeri olabilir. Perde arkasında, derleyici yeni [ValueTuple][1] yapısını kullanacaktır.

    public (int sum, int count) GetTallies() 
    {
        return (1, 2);
    }

_Yan not_: Bunun Visual Studio 2017'de çalışması için ```System.ValueTuple`` paketini almanız gerekir.

Tuple döndürme yöntemi sonucu tek bir değişkene atanmışsa, üyelere yöntem imzasında tanımlı adlarıyla erişebilirsiniz:

    var result = GetTallies();
    // > result.sum
    // 1
    // > result.count
    // 2

# Tuple Dekonstrüksiyonu

Tuple yapısökümü, bir demeti parçalarına ayırır.

Örneğin, "GetTallies"i çağırmak ve dönüş değerini iki ayrı değişkene atamak, tanımlama grubunu bu iki değişkene dönüştürür:

    (int tallyOne, int tallyTwo) = GetTallies();

'var' da çalışır:

    (var s, var c) = GetTallies();

"()" dışında "var" ile daha kısa sözdizimi de kullanabilirsiniz:

    var (s, c) = GetTallies();

Ayrıca mevcut değişkenleri de yapılandırabilirsiniz:

    int s, c;
    (s, c) = GetTallies();

Değiştirme artık çok daha basit (geçici değişkene gerek yok):

    (b, a) = (a, b);

İlginç bir şekilde, herhangi bir nesne, sınıfta bir "Deconstruct" yöntemi tanımlanarak yapısız hale getirilebilir:

    class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }

        public void Deconstruct(out string firstName, out string lastName)
        {
            firstName = FirstName;
            lastName = LastName;
        }
    }

    var person = new Person { FirstName = "John", LastName = "Smith" };
    var (localFirstName, localLastName) = person;

Bu durumda, `(localFirstName, localLastName) = person` sözdizimi, `person` üzerinde `Deconstruct`ı çağırıyor.

Yapısızlaştırma, bir uzatma yönteminde bile tanımlanabilir. Bu, yukarıdakine eşdeğerdir:

    public static class PersonExtensions
    {
        public static void Deconstruct(this Person person, out string firstName, out string lastName)
        {
            firstName = person.FirstName;
            lastName = person.LastName;
        }
    }
    
    var (localFirstName, localLastName) = person;

'Person' sınıfı için alternatif bir yaklaşım, 'Name'in kendisini bir 'Tuple' olarak tanımlamaktır. Aşağıdakileri göz önünde bulundur:

    class Person
    {
        public (string First, string Last) Name { get; }

        public Person((string FirstName, string LastName) name)
        {
            Name = name;
        }
    }

Sonra bir kişiyi şöyle başlatabilirsiniz (bir argüman olarak bir Tuple alabiliriz):

    var person = new Person(("Jane", "Smith"));

    var firstName = person.Name.First; // "Jane"
    var lastName = person.Name.Last;   // "Smith"

# Tuple Başlatma
Ayrıca kodda keyfi olarak demetler oluşturabilirsiniz:

    var name = ("John", "Smith");
    Console.WriteLine(name.Item1);
    // Outputs John

    Console.WriteLine(name.Item2);
    // Outputs Smith

# 

Bir demet oluştururken, demetin üyelerine geçici öğe adları atayabilirsiniz:

    var name = (first: "John", middle: "Q", last: "Smith");
    Console.WriteLine(name.first);
    // Outputs John

# Tür çıkarımı

Aynı imzayla (eşleşen türler ve sayımlar) tanımlanan çoklu demetler, eşleşen türler olarak çıkarılacaktır. Örneğin:

    public (int sum, double average) Measure(List<int> items)
    {
        var stats = (sum: 0, average: 0d);
        stats.sum = items.Sum();
        stats.average = items.Average();
        return stats;
    }

'stats' değişkeninin bildirimi ve yöntemin dönüş imzası bir eşleşme olduğundan, 'stats' döndürülebilir.

# Yansıma ve Tuple Alan Adları
Üye adları çalışma zamanında mevcut değildir. Yansıma, üye adları eşleşmese bile aynı sayıda ve türde üyeye sahip demetleri aynı kabul edecektir. Bir tanımlama grubunu bir "nesneye" ve ardından aynı üye türlerine, ancak farklı adlara sahip bir tanımlama grubuna dönüştürmek de bir istisnaya neden olmaz.

ValueTuple sınıfının kendisi üye adları için bilgileri korumazken, bilgilere bir TupleElementNamesAttribute içinde yansıma yoluyla erişilebilir. Bu öznitelik, demetin kendisine değil, yöntem parametrelerine, dönüş değerlerine, özelliklere ve alanlara uygulanır. Bu, demet öğe adlarının derlemeler arasında korunmasına izin verir, yani bir yöntem döndürürse (dize adı, int sayısı), ad adı ve sayı, başka bir derlemede yöntemin arayanlar için kullanılabilir olacaktır, çünkü dönüş değeri, değerleri içeren TupleElementNameAttribute ile işaretlenecektir. "isim" ve "say".

# Jenerikler ve 'async' ile kullanın

Yeni tanımlama grubu özellikleri (temeldeki 'ValueTuple' türünü kullanarak) jenerikleri tamamen destekler ve genel tür parametresi olarak kullanılabilir. Bu onları "async"/"await" kalıbıyla kullanmayı mümkün kılar:

    public async Task<(string value, int count)> GetValueAsync()
    {
        string fooBar = await _stackoverflow.GetStringAsync();
        int num = await _stackoverflow.GetIntAsync();

        return (fooBar, num);
    }

# Koleksiyonlarla kullanın

Kod dallanmasını önlemek için koşullara göre eşleşen bir demet bulmaya çalıştığınız bir senaryoda (örnek olarak) bir demet koleksiyonuna sahip olmak faydalı olabilir.

Örnek:

    private readonly List<Tuple<string, string, string>> labels = new List<Tuple<string, string, string>>()
    {
        new Tuple<string, string, string>("test1", "test2", "Value"),
        new Tuple<string, string, string>("test1", "test1", "Value2"),
        new Tuple<string, string, string>("test2", "test2", "Value3"),
    };

    public string FindMatchingValue(string firstElement, string secondElement)
    {
        var result = labels
            .Where(w => w.Item1 == firstElement && w.Item2 == secondElement)
            .FirstOrDefault();

        if (result == null)
            throw new ArgumentException("combo not found");

        return result.Item3;
    }

Yeni demetler ile şunlar olabilir:

    private readonly List<(string firstThingy, string secondThingyLabel, string foundValue)> labels = new List<(string firstThingy, string secondThingyLabel, string foundValue)>()
    {
        ("test1", "test2", "Value"),
        ("test1", "test1", "Value2"),
        ("test2", "test2", "Value3"),
    }

    public string FindMatchingValue(string firstElement, string secondElement)
    {
        var result = labels
            .Where(w => w.firstThingy == firstElement && w.secondThingyLabel == secondElement)
            .FirstOrDefault();

        if (result == null)
            throw new ArgumentException("combo not found");

        return result.foundValue;
    }

Yukarıdaki örnek demetindeki adlandırma oldukça genel olsa da, ilgili etiketler fikri, "item1", "item2" ve "item3" referans alınarak kodda ne yapılmaya çalışıldığı hakkında daha derin bir anlayışa izin verir.

# ValueTuple ve Tuple arasındaki farklar

'ValueTuple' tanıtımının birincil nedeni performanstır.

| Tür adı | 'ValueTuple' | 'Demet' |
|---|---|---|
| Sınıf veya yapı | "yapı" | 'sınıf' |
| Değişebilirlik (oluşturulduktan sonra değerlerin değiştirilmesi) | değişken | değişmez |
| Üyeleri adlandırma ve diğer dil desteği | evet | hayır ([TBD][2]) |

# Referanslar

- [GitHub'da Orijinal Tuples dil özelliği önerisi][3]
- [C# 7.0 özellikleri için çalıştırılabilir bir VS 15 çözümü][4]
- [NuGet Tuple Paketi][5]


[1]: https://github.com/dotnet/corefx/blob/master/src/System.ValueTuple/src/System/ValueTuple/ValueTuple.cs
[2]: https://github.com/dotnet/roslyn/issues/11031
[3]: https://github.com/dotnet/roslyn/issues/347
[4]: https://code.msdn.microsoft.com/Introduce-new-C-70-features-c639ed88
[5]: https://www.nuget.org/packages/System.ValueTuple/

## Yerel işlevler
Yerel işlevler bir yöntem içinde tanımlanır ve bunun dışında kullanılamaz. Tüm yerel değişkenlere erişimleri vardır ve yineleyicileri, 'async'/'await' ve lambda sözdizimini desteklerler. Bu sayede bir fonksiyona özgü tekrarlar sınıfı kalabalıklaştırmadan işlevsel hale getirilebilir. Bir yan etki olarak bu, intellisense öneri performansını iyileştirir.

# Örnek

    double GetCylinderVolume(double radius, double height)
    {
        return getVolume();
  
        double getVolume()
        {
            // You can declare inner-local functions in a local function 
            double GetCircleArea(double r) => Math.PI * r * r;

            // ALL parents' variables are accessible even though parent doesn't have any input. 
            return GetCircleArea(radius) * height;
        }
    }

Yerel işlevler, LINQ operatörleri için kodu önemli ölçüde basitleştirir; burada, yineleme başlayana kadar gecikmeden bağımsız değişken denetimlerini anında yapmak için genellikle bağımsız değişken denetimlerini gerçek mantıktan ayırmanız gerekir.

# Örnek

    public static IEnumerable<TSource> Where<TSource>(
        this IEnumerable<TSource> source, 
        Func<TSource, bool> predicate)
    {
        if (source == null) throw new ArgumentNullException(nameof(source));
        if (predicate == null) throw new ArgumentNullException(nameof(predicate));
    
        return iterator();

        IEnumerable<TSource> iterator()
        {
            foreach (TSource element in source)
                if (predicate(element))
                    yield return element;
        }
    }

Yerel işlevler ayrıca "async" ve "await" anahtar sözcüklerini de destekler.

# Örnek

    async Task WriteEmailsAsync()
    {
        var emailRegex = new Regex(@"(?i)[a-z0-9_.+-]+@[a-z0-9-]+\.[a-z0-9-.]+");
        IEnumerable<string> emails1 = await getEmailsFromFileAsync("input1.txt");
        IEnumerable<string> emails2 = await getEmailsFromFileAsync("input2.txt");
        await writeLinesToFileAsync(emails1.Concat(emails2), "output.txt");

        async Task<IEnumerable<string>> getEmailsFromFileAsync(string fileName)
        {
            string text;

            using (StreamReader reader = File.OpenText(fileName))
            {
                text = await reader.ReadToEndAsync();
            }

            return from Match emailMatch in emailRegex.Matches(text) select emailMatch.Value;
        }

        async Task writeLinesToFileAsync(IEnumerable<string> lines, string fileName)
        {
            using (StreamWriter writer = File.CreateText(fileName))
            {
                foreach (string line in lines)
                {
                    await writer.WriteLineAsync(line);
                }
            }
        }
    }

Fark etmiş olabileceğiniz önemli bir şey, yerel işlevlerin 'return' ifadesinin altında tanımlanabileceğidir, bunların üzerinde tanımlanması **gerekmez**. Ek olarak, yerel işlevler, kendilerini sınıf kapsamı işlevlerinden daha kolay ayırt etmek için tipik olarak "lowerCamelCase" adlandırma kuralına uyar.

## çıkış var bildirimi
C#'daki yaygın bir kalıp, nesneleri güvenli bir şekilde ayrıştırmak için `bool TryParse(nesne girişi, nesne değeri dışında)` kullanmaktır.

'out var' bildirimi, okunabilirliği artırmak için basit bir özelliktir. Bir değişkenin out parametresi olarak iletilirken aynı anda bildirilmesine izin verir.

Bu şekilde bildirilen bir değişken, bildirildiği noktada gövdenin geri kalanına dahil edilir.

# Örnek

C# 7.0'dan önce 'TryParse' kullanarak, işlevi çağırmadan önce değeri almak için bir değişken bildirmelisiniz:

<!-- eğer sürüm [lt 7.0] -->
    int value;
    if (int.TryParse(input, out value)) 
    {
        Foo(value); // ok
    }
    else
    {
        Foo(value); // value is zero
    }

    Foo(value); // ok
<!-- eğer --> son sürüm

C# 7.0'da, "out" parametresine iletilen değişkenin bildirimini satır içi olarak yapabilir ve ayrı bir değişken bildirimine olan ihtiyacı ortadan kaldırabilirsiniz:

<!-- eğer sürüm [gte 7.0] -->
    if (int.TryParse(input, out var value)) 
    {
        Foo(value); // ok
    }
    else
    {
        Foo(value); // value is zero
    }

    Foo(value); // still ok, the value in scope within the remainder of the body
<!-- eğer --> son sürüm

Bir işlevin 'out' içinde döndürdüğü bazı parametrelere gerek yoksa _discard_ operatörünü '_' kullanabilirsiniz.

    p.GetCoordinates(out var x, out _); // I only care about x

Bir "out var" bildirimi, halihazırda "out" parametreleri olan herhangi bir mevcut işlevle kullanılabilir. İşlev bildirimi sözdizimi aynı kalır ve işlevi bir "out var" bildirimiyle uyumlu hale getirmek için ek gereksinimlere gerek yoktur. Bu özellik sadece sözdizimsel şekerdir.

`out var` bildiriminin bir diğer özelliği de anonim tiplerle kullanılabilmesidir.

<!-- eğer sürüm [gte 7.0] -->
    var a = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    var groupedByMod2 = a.Select(x => new
                                      {
                                          Source = x,
                                          Mod2 = x % 2
                                      })
                         .GroupBy(x => x.Mod2)
                         .ToDictionary(g => g.Key, g => g.ToArray());
    if (groupedByMod2.TryGetValue(1, out var oddElements))
    {
        Console.WriteLine(oddElements.Length);
    }
<!-- eğer --> son sürüm
    
Bu kodda, 'int' anahtarlı bir 'Sözlük' ve anonim tip değeri dizisi oluşturuyoruz. C#'ın önceki sürümünde, 'out' değişkenini (anonim türdendir!) bildirmenizi gerektirdiğinden, burada 'TryGetValue' yöntemini kullanmak imkansızdı. Ancak, "out var" ile "out" değişkeninin türünü açıkça belirtmemize gerek yoktur.

# Sınırlamalar

İfadeler ifade lambda gövdeleri olarak yorumlandığından, LINQ sorgularında out var bildirimlerinin sınırlı kullanımının olduğunu ve bu nedenle tanıtılan değişkenlerin kapsamının bu lambdalarla sınırlı olduğunu unutmayın. Örneğin, aşağıdaki kod çalışmayacaktır:

    var nums = 
        from item in seq
        let success = int.TryParse(item, out var tmp)
        select success ? tmp : 0; // Error: The name 'tmp' does not exist in the current context



# Referanslar

* [GitHub'da orijinal çıkış var beyanı teklifi](https://github.com/dotnet/roslyn/issues/6183)

## Desen Eşleştirme
C# için kalıp eşleştirme uzantıları, işlevsel dillerden kalıp eşleştirmenin birçok avantajını sağlar, ancak bu, temel dilin hissi ile sorunsuz bir şekilde bütünleşir.

**`switch` ifadesi**
-----
Model eşleştirme, türleri açmak için "switch" ifadesini genişletir:

    class Geometry {} 

    class Triangle : Geometry
    {
        public int Width { get; set; }
        public int Height { get; set; }
        public int Base { get; set; }
    }

    class Rectangle : Geometry
    {
        public int Width { get; set; }
        public int Height { get; set; }
    }

    class Square : Geometry
    {
        public int Width { get; set; }
    }

    public static void PatternMatching()
    {
        Geometry g = new Square { Width = 5 }; 
        
        switch (g)
        {
            case Triangle t:
                Console.WriteLine($"{t.Width} {t.Height} {t.Base}");
                break;
            case Rectangle sq when sq.Width == sq.Height:
                Console.WriteLine($"Square rectangle: {sq.Width} {sq.Height}");
                break;
            case Rectangle r:
                Console.WriteLine($"{r.Width} {r.Height}");
                break;
            case Square s:
                Console.WriteLine($"{s.Width}");
                break;
            default:
                Console.WriteLine("<other>");
                break;
        }
    }


**`is` ifadesi**
---- 

Model eşleştirme, bir türü kontrol etmek ve aynı anda yeni bir değişken bildirmek için 'is' operatörünü genişletir.

# Örnek

<!-- eğer sürüm [lt 7.0] -->
    string s = o as string;
    if(s != null)
    {
        // do something with s
    }
<!-- eğer --> son sürüm

şu şekilde yeniden yazılabilir:

<!-- eğer sürüm [gte 7.0] -->
    if(o is string s)
    {
        //Do something with s
    };
<!-- eğer --> son sürüm

Ayrıca, "s" model değişkeninin kapsamının, çevreleyen kapsamın sonuna ulaşan "if" bloğunun dışına genişletildiğini unutmayın, örneğin:

    if(someCondition)
    {
       if(o is string s)
       {
          //Do something with s
       }
       else
       {
         // s is unassigned here, but accessible 
       }
    
       // s is unassigned here, but accessible 
    }
    // s is not accessible here

## Rakam ayırıcılar
Alt çizgi "_" rakam ayırıcı olarak kullanılabilir. Rakamları büyük sayısal değişmezlerde gruplayabilmenin okunabilirlik üzerinde önemli bir etkisi vardır.

Alt çizgi, aşağıda belirtilenler dışında sayısal bir hazır bilgide herhangi bir yerde olabilir. Farklı senaryolarda veya farklı sayısal temellerde farklı gruplamalar anlamlı olabilir.

Herhangi bir rakam dizisi bir veya daha fazla alt çizgi ile ayrılabilir. "_" işaretine ondalık sayılarda olduğu kadar üslerde de izin verilir. Ayırıcıların anlamsal etkisi yoktur - basitçe yok sayılırlar.

    int bin = 0b1001_1010_0001_0100;
    int hex = 0x1b_a0_44_fe;
    int dec = 33_554_432;
    int weird = 1_2__3___4____5_____6______7_______8________9;
    double real = 1_000.111_1e-1_000;

**`_` rakam ayırıcısının kullanılamayacağı durumlarda:**
- değerin başında (`_121`)
- değerin sonunda (`121_` veya `121.05_`)
- ondalık sayının yanında (`10_.0`)
- üs karakterinin yanında (`1.1e_1`)
- tür belirtecinin (`10_f`) yanında
- ikili ve onaltılık değişmezlerde "0x" veya "0b"nin hemen ardından ([örneğin 0b_1001_1000][1]'e izin verecek şekilde değiştirilebilir)

[1]: https://github.com/dotnet/roslyn/issues/12680

## İkili değişmez değerler
**0b** öneki, İkili değişmezleri temsil etmek için kullanılabilir.

İkili değişmezler, sıfırlardan ve birlerden sayılar oluşturmaya izin verir, bu da bir sayının ikili gösteriminde hangi bitlerin ayarlandığını görmeyi çok daha kolaylaştırır. Bu, ikili bayraklarla çalışmak için yararlı olabilir.

Aşağıdakiler, "34" (=2<sup>5</sup> + 2<sup>1</sup>) değerine sahip bir "int" belirtmenin eşdeğer yollarıdır:

    // Using a binary literal:
    //   bits: 76543210
    int a1 = 0b00100010;          // binary: explicitly specify bits

    // Existing methods:
    int a2 = 0x22;                // hexadecimal: every digit corresponds to 4 bits
    int a3 = 34;                  // decimal: hard to visualise which bits are set
    int a4 = (1 << 5) | (1 << 1); // bitwise arithmetic: combining non-zero bits

# İşaretler numaralandırma

Daha önce, bir "enum" için bayrak değerleri belirtmek, bu örnekteki üç yöntemden yalnızca biri kullanılarak yapılabilirdi:

    [Flags]
    public enum DaysOfWeek
    {
        // Previously available methods:
        //          decimal        hex       bit shifting
        Monday    =  1,    //    = 0x01    = 1 << 0
        Tuesday   =  2,    //    = 0x02    = 1 << 1
        Wednesday =  4,    //    = 0x04    = 1 << 2
        Thursday  =  8,    //    = 0x08    = 1 << 3
        Friday    = 16,    //    = 0x10    = 1 << 4
        Saturday  = 32,    //    = 0x20    = 1 << 5
        Sunday    = 64,    //    = 0x40    = 1 << 6
    
        Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
        Weekends = Saturday | Sunday
    }

İkili değişmezlerde hangi bitlerin ayarlandığı daha açıktır ve bunları kullanmak onaltılık sayıları ve bitsel aritmetiği anlamayı gerektirmez:

    [Flags]
    public enum DaysOfWeek
    {
        Monday    = 0b00000001,
        Tuesday   = 0b00000010,
        Wednesday = 0b00000100,
        Thursday  = 0b00001000,
        Friday    = 0b00010000,
        Saturday  = 0b00100000,
        Sunday    = 0b01000000,
    
        Weekdays = Monday | Tuesday | Wednesday | Thursday | Friday,
        Weekends = Saturday | Sunday
    }

## atma ifadeleri
C# 7.0, belirli yerlerde ifade olarak atmaya izin verir:

    class Person
    {
        public string Name { get; }

        public Person(string name) => Name = name ?? throw new ArgumentNullException(nameof(name));

        public string GetFirstName()
        {
            var parts = Name.Split(' ');
            return (parts.Length > 0) ? parts[0] : throw new InvalidOperationException("No name!");
        }

        public string GetLastName() => throw new NotImplementedException();
    }


C# 7.0'dan önce, bir ifade gövdesinden bir istisna atmak istiyorsanız şunları yapmanız gerekir:

    var spoons = "dinner,desert,soup".Split(',');

    var spoonsArray = spoons.Length > 0 ? spoons : null;

    if (spoonsArray == null) 
    {
        throw new Exception("There are no spoons");
    }

Veya

    var spoonsArray = spoons.Length > 0 
        ? spoons 
        : new Func<string[]>(() => 
          {
              throw new Exception("There are no spoons");
          })();

C# 7.0'da yukarıdakiler şimdi şu şekilde basitleştirilmiştir:

    var spoonsArray = spoons.Length > 0 ? spoons : throw new Exception("There are no spoons");



## Genişletilmiş ifade gövdeli üye listesi
C# 7.0, ifade gövdelerine sahip olabilecek şeyler listesine erişimciler, oluşturucular ve sonlandırıcılar ekler:

    class Person
    {
        private static ConcurrentDictionary<int, string> names = new ConcurrentDictionary<int, string>();

        private int id = GetId();

        public Person(string name) => names.TryAdd(id, name); // constructors

        ~Person() => names.TryRemove(id, out _);              // finalizers

        public string Name
        {
            get => names[id];                                 // getters
            set => names[id] = value;                         // setters
        }
    }

Ayrıca, atma operatörü için [out var bildirimi][1] bölümüne bakın.

[1]: https://www.wikiod.com/tr/docs/c%23/1936/c-sharp-7-0-features/6326/out-var-declaration

## ref dönüş ve ref yerel
Başvuru dönüşleri ve başvuru yerelleri, güvenli olmayan işaretçilere başvurmadan belleği kopyalamak yerine başvuruları değiştirmek ve bellek bloklarına döndürmek için kullanışlıdır.

# Referans Dönüşü

    public static ref TValue Choose<TValue>(
        Func<bool> condition, ref TValue left, ref TValue right)
    {
        return condition() ? ref left : ref right;
    }

Bununla, bir koşula göre döndürülen biri ile referans olarak iki değeri iletebilirsiniz:

    Matrix3D left = …, right = …;
    Choose(chooser, ref left, ref right).M20 = 1.0;


# Yerel Referans

    public static ref int Max(ref int first, ref int second, ref int third)
    {
        ref int max = first > second ? ref first : ref second;
        return max > third ? ref max : ref third;
    }
    …
    int a = 1, b = 2, c = 3;
    Max(ref a, ref b, ref c) = 4;
    Debug.Assert(a == 1); // true
    Debug.Assert(b == 2); // true
    Debug.Assert(c == 4); // true

# Güvenli Olmayan Ref İşlemleri
"System.Runtime.CompilerServices.Unsafe" içinde, "başvuru" değerlerini temel olarak işaretçilermiş gibi değiştirmenize izin veren bir dizi güvenli olmayan işlem tanımlanmıştır.

Örneğin, bir bellek adresini (`ref`) farklı bir tür olarak yeniden yorumlamak:

    byte[] b = new byte[4] { 0x42, 0x42, 0x42, 0x42 };
    
    ref int r = ref Unsafe.As<byte, int>(ref b[0]);
    Assert.Equal(0x42424242, r);
    
    0x0EF00EF0;
    Assert.Equal(0xFE, b[0] | b[1] | b[2] | b[3]);

Bunu yaparken [endianness][1]'e dikkat edin, ör. Gerekirse `BitConverter.IsLittleEndian`ı kontrol edin ve buna göre işleyin.

Veya bir diziyi güvenli olmayan bir şekilde yineleyin:

    int[] a = new int[] { 0x123, 0x234, 0x345, 0x456 };
    
    ref int r1 = ref Unsafe.Add(ref a[0], 1);
    Assert.Equal(0x234, r1);

    ref int r2 = ref Unsafe.Add(ref r1, 2);
    Assert.Equal(0x456, r2);

    ref int r3 = ref Unsafe.Add(ref r2, -3);
    Assert.Equal(0x123, r3);

Veya benzer "Çıkar":

    string[] a = new string[] { "abc", "def", "ghi", "jkl" };
    
    ref string r1 = ref Unsafe.Subtract(ref a[0], -2);
    Assert.Equal("ghi", r1);
    
    ref string r2 = ref Unsafe.Subtract(ref r1, -1);
    Assert.Equal("jkl", r2);
    
    ref string r3 = ref Unsafe.Subtract(ref r2, 3);
    Assert.Equal("abc", r3);

Ek olarak, iki "ref" değerinin aynı olup olmadığı, yani aynı adres olup olmadığı kontrol edilebilir:

    long[] a = new long[2];
    
    Assert.True(Unsafe.AreSame(ref a[0], ref a[0]));
    Assert.False(Unsafe.AreSame(ref a[0], ref a[1]));

# Bağlantılar
[Roslyn Github Sorunu][2]

[System.Runtime.CompilerServices.Github'da Güvensiz][3]


[1]: https://en.wikipedia.org/wiki/Endianness
[2]: https://github.com/dotnet/roslyn/issues/118
[3]: https://github.com/dotnet/corefx/tree/master/src/System.Runtime.CompilerServices.Unsafe

## DeğerGörevi<T>
`Task<T>` bir **sınıftır** ve sonuç hemen mevcut olduğunda, tahsisinin gereksiz ek yüküne neden olur.

'ValueTask<T>' bir **yapıdır** ve bekleme anında **async** işleminin sonucunun zaten mevcut olması durumunda bir 'Task' nesnesinin tahsis edilmesini önlemek için tanıtıldı.

Böylece `ValueTask<T>` iki fayda sağlar:

# 1. Performans artışı

İşte bir "Task<T>" örneği:
- Yığın tahsisi gerektirir
- JIT ile 120ns alır


    async Task<int> TestTask(int d)
    {
        await Task.Delay(d);
        return 10;
    }

İşte analog "ValueTask<T>" örneği:
- Sonuç eşzamanlı olarak biliniyorsa yığın tahsisi yapılmaz (bu durumda 'Task.Delay' nedeniyle değildir, ancak çoğu gerçek dünya 'async'/'bekleme' senaryolarındadır)
- JIT ile 65ns alır


    async ValueTask<int> TestValueTask(int d)
    {
        await Task.Delay(d);
        return 10;
    }

# 2. Artan uygulama esnekliği

Senkronize olmak isteyen bir zaman uyumsuz arabirimin uygulamaları, aksi takdirde 'Task.Run' veya 'Task.FromResult' (yukarıda tartışılan performans cezasıyla sonuçlanır) kullanmaya zorlanır. Bu nedenle, senkronize uygulamalara karşı bir miktar baskı var.

Ancak 'ValueTask<T>' ile uygulamalar, arayanları etkilemeden eşzamanlı veya eşzamansız olmak arasında seçim yapmakta daha özgürdür.

Örneğin, asenkron metoda sahip bir arayüz aşağıda verilmiştir:

    interface IFoo<T>
    {
        ValueTask<T> BarAsync();
    }

... ve işte bu yöntemin nasıl çağrılabileceği:

    IFoo<T> thing = getThing();
    var x = await thing.BarAsync();

"ValueTask" ile, yukarıdaki kod **eşzamanlı veya eşzamansız uygulamalar** ile çalışır:

## Senkronize uygulama:

    class SynchronousFoo<T> : IFoo<T>
    {
        public ValueTask<T> BarAsync()
        {
            var value = default(T);
            return new ValueTask<T>(value);
        }
    }

## Eşzamansız uygulama

    class AsynchronousFoo<T> : IFoo<T>
    {
        public async ValueTask<T> BarAsync()
        {
            var value = default(T);
            await Task.Delay(1);
            return value;
        }
    }

# Notlar

`ValueTask` yapısının [C# 7.0][1]'e eklenmesi planlanmakla birlikte, şimdilik başka bir kütüphane olarak tutulmuştur.
https://www.wikiod.com/tr/docs/c%23/1936/c-sharp-7-0-features/28612/valuetaskt#
"System.Threading.Tasks.Extensions" paketi [Nuget Gallery](https://www.nuget.org/packages/System.Threading.Tasks.Extensions/) adresinden indirilebilir.

[1]: https://blogs.msdn.microsoft.com/dotnet/2016/08/24/whats-new-in-csharp-7-0/

