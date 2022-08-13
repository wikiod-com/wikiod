---
title: "Fonksiyonel Programlama"
slug: "fonksiyonel-programlama"
draft: false
images: []
weight: 9866
type: docs
toc: true
---

## İşlev ve Eylem
**Func** parametreli anonim işlevler için bir tutucu sağlar. Baştaki türler girdilerdir ve son tür her zaman dönüş değeridir.

    // square a number.
    Func<double, double> square = (x) => { return x * x; };

    // get the square root.
    // note how the signature matches the built in method.
    Func<double, double> squareroot = Math.Sqrt;

    // provide your workings.
    Func<double, double, string> workings = (x, y) => 
        string.Format("The square of {0} is {1}.", x, square(y))
        
**Action** nesneleri, void yöntemleri gibidir, bu nedenle yalnızca bir girdi türüne sahiptirler. Değerlendirme yığınına hiçbir sonuç yerleştirilmez.

    // right-angled triangle.
    class Triangle
    {
        public double a;
        public double b;
        public double h;
    }

    // Pythagorean theorem.
    Action<Triangle> pythagoras = (x) => 
        x.h = squareroot(square(x.a) + square(x.b));
    
    Triangle t = new Triangle { a = 3, b = 4 };
    pythagoras(t);
    Console.WriteLine(t.h); // 5.



## Boş Referanslardan Kaçının
C# geliştiricileri, başa çıkmak için çok sayıda boş referans istisnası alır. F# geliştiricileri, Seçenek türüne sahip oldukları için yapmazlar. Bir Option<> türü (bazıları ad olarak Belki<>'yi tercih eder), Some ve None döndürme türü sağlar. Bir yöntemin boş bir kayıt döndürmek üzere olabileceğini açıkça belirtir.

Örneğin, aşağıdakileri okuyamazsınız ve boş bir değerle uğraşmanız gerekip gerekmediğini bilemezsiniz.

    var user = _repository.GetUser(id);

Olası null hakkında bilginiz varsa, bununla başa çıkmak için bazı ortak kodlar sunabilirsiniz.

    var username = user != null ? user.Name : string.Empty;

Ya bunun yerine bir Option<> döndürülürse?

    Option<User> maybeUser = _repository.GetUser(id);

Kod şimdi, Hiçbiri kaydının döndürülebileceğini ve Bazı veya Hiçbiri olup olmadığını kontrol etmek için ortak kod kodunun gerekli olduğunu açıkça ortaya koyuyor:

    var username = maybeUser.HasValue ? maybeUser.Value.Name : string.Empty;

Aşağıdaki yöntem, bir Seçeneğin <> nasıl döndürüleceğini gösterir.

    public Option<User> GetUser(int id)
    {
        var users = new List<User>
        {
            new User { Id = 1, Name = "Joe Bloggs" },
            new User { Id = 2, Name = "John Smith" }
        };
    
        var user = users.FirstOrDefault(user => user.Id == id);
    
        return user != null ? new Option<User>(user) : new Option<User>();
    }

İşte Seçenek<>'nin minimal bir uygulaması.

    public struct Option<T>
    {
        private readonly T _value;
    
        public T Value
        {
            get
            {
                if (!HasValue)
                    throw new InvalidOperationException();

                return _value;
            }
        }

        public bool HasValue
        {
            get { return _value != null; }
        }
    
        public Option(T value)
        {
            _value = value;
        }
    
        public static implicit operator Option<T>(T value)
        {
            return new Option<T>(value);
        }
    }

Yukarıdakileri göstermek için [avoidNull.csx][1] C# REPL ile çalıştırılabilir.

Belirtildiği gibi, bu minimal bir uygulamadır. ["Belki" NuGet paketleri[2] araması, bir dizi iyi kitaplık ortaya çıkaracaktır.


[1]: https://Gist.github.com/Boggin/d53660f32aeaa35e0b028919ddc465e3
[2]: https://www.nuget.org/packages?q=belki

## Üst Düzey İşlevler
Daha yüksek dereceli bir işlev, argüman olarak başka bir işlevi alan veya bir işlev (veya her ikisini) döndüren bir işlevdir.

Bu genellikle lambdalarla yapılır, örneğin bir yüklemi bir LINQ Where yan tümcesine aktarırken:

    var results = data.Where(p => p.Items == 0);

Where() yan tümcesi, ona önemli ölçüde esneklik sağlayan birçok farklı yüklem alabilir.

Strateji tasarım desenini uygularken bir yöntemi başka bir yönteme geçirmek de görülür. Örneğin, çalışma zamanındaki gereksinimlere bağlı olarak çeşitli sıralama yöntemleri seçilebilir ve bir nesne üzerinde bir Sort yöntemine geçirilebilir.

## Değişmezlik
Değişmezlik, işlevsel programlamada yaygındır ve nesne yönelimli programlamada nadirdir.

Örneğin, değişken durumu olan bir adres türü oluşturun:

    public class Address () 
    {
        public string Line1 { get; set; }
        public string Line2 { get; set; }
        public string City  { get; set; }
    }

Herhangi bir kod parçası, yukarıdaki nesnedeki herhangi bir özelliği değiştirebilir.

Şimdi değişmez adres türünü oluşturun:

    public class Address () 
    {
        public readonly string Line1;
        public readonly string Line2;
        public readonly string City;

        public Address(string line1, string line2, string city) 
        {
            Line1 = line1;
            Line2 = line2;
            City  = city;
        }
    }

Salt okunur koleksiyonlara sahip olmanın değişmezliğe saygı göstermediğini unutmayın. Örneğin,

    public class Classroom
    {
        public readonly List<Student> Students;
        
        public Classroom(List<Student> students)
        {
            Students = students;
        }
    }

değişmez değildir, çünkü nesnenin kullanıcısı koleksiyonu değiştirebilir (öğeleri ekleyebilir veya kaldırabilir). Değişmez hale getirmek için, ya ekleme yöntemlerini göstermeyen IEnumerable<Student> gibi bir arabirim kullanmak ya da onu ReadOnlyCollection<Student> yapmak gerekir.

    public class Classroom
    {
        public readonly ReadOnlyCollection<Student> Students;

        public Classroom(ReadOnlyCollection<Student> students)
        {
            Students = students;
        }
    }

    List<Students> list = new List<Student>();
    // add students
    Classroom c = new Classroom(list.AsReadOnly());   


Değişmez nesne ile aşağıdaki avantajlara sahibiz:

- Bilinen bir durumda olacaktır (diğer kod bunu değiştiremez).
- İplik güvenlidir.
- Yapıcı doğrulama için tek bir yer sunar.
- Nesnenin değiştirilemeyeceğini bilmek kodun anlaşılmasını kolaylaştırır.

## Değişmez koleksiyonlar
[`System.Collections.Immutable`][1] NuGet paketi değişmez koleksiyon sınıfları sağlar.

# Öğe oluşturma ve ekleme

    var stack = ImmutableStack.Create<int>();
    var stack2 = stack.Push(1); // stack is still empty, stack2 contains 1
    var stack3 = stack.Push(2); // stack2 still contains only one, stack3 has 2, 1

# Oluşturucuyu kullanarak oluşturma

Bazı değişmez koleksiyonlar, büyük değişmez örnekleri ucuza oluşturmak için kullanılabilecek bir "Oluşturucu" iç sınıfına sahiptir:

    var builder = ImmutableList.CreateBuilder<int>(); // returns ImmutableList.Builder
    builder.Add(1);
    builder.Add(2);
    var list = builder.ToImmutable();

# Mevcut bir IEnumerable'dan oluşturma

    var numbers = Enumerable.Range(1, 5);
    var list = ImmutableList.CreateRange<int>(numbers);

Tüm değişmez koleksiyon türlerinin listesi:

- [`System.Collections.Immutable.ImmutableArray<T>`][2]
- [`System.Collections.Immutable.ImmutableDictionary<TKey,TValue>`][3]
- [`System.Collections.Immutable.ImmutableHashSet<T>`][4]
- [`System.Collections.Immutable.ImmutableList<T>`][5]
- [`System.Collections.Immutable.ImmutableQueue<T>`][6]
- [`System.Collections.Immutable.ImmutableSortedDictionary<TKey,TValue>`][7]
- [`System.Collections.Immutable.ImmutableSortedSet<T>`][8]
- [`System.Collections.Immutable.ImmutableStack<T>`][9]


[1]: https://www.nuget.org/packages/System.Collections.Immutable/
[2]: https://msdn.microsoft.com/en-us/library/dn638264(v=vs.111).aspx
[3]: https://msdn.microsoft.com/en-us/library/dn467181(v=vs.111).aspx
[4]: https://msdn.microsoft.com/en-us/library/dn467171(v=vs.111).aspx
[5]: https://msdn.microsoft.com/en-us/library/dn456077.aspx
[6]: https://msdn.microsoft.com/en-us/library/dn467186(v=vs.111).aspx
[7]: https://msdn.microsoft.com/en-us/library/dn467194(v=vs.111).aspx
[8]: https://msdn.microsoft.com/en-us/library/dn467193(v=vs.111).aspx
[9]: https://msdn.microsoft.com/en-us/library/dn467197(v=vs.111).aspx

