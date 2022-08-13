---
title: "Null-Coalescing Operatörü"
slug: "null-coalescing-operatoru"
draft: false
images: []
weight: 9486
type: docs
toc: true
---

## Sözdizimi
- var sonuç = olasıNullObject ?? varsayılan değer;

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |
| `olasıNullObject` | Boş değer için test edilecek değer. Null değilse, bu değer döndürülür. Null yapılabilir bir tür olmalıdır. |
| "varsayılanDeğer" | "possibleNullObject" null ise döndürülen değer. "possibleNullObject" ile aynı türde olmalıdır. |

Boş birleştirme operatörünün kendisi iki ardışık soru işareti karakteridir: `??`

Koşullu ifadenin kısaltmasıdır:

    possibleNullObject != null ? possibleNullObject : defaultValue

Sol taraftaki işlenen (test edilen nesne) null yapılabilir bir değer türü veya başvuru türü olmalıdır, aksi takdirde bir derleme hatası oluşur.

?? operatörü hem referans türleri hem de değer türleri için çalışır.



## Temel kullanım
[`boş birleştirme operatörü (??)`][2] kullanılması, sol taraftaki işlenen "boş" ise boş bir tür için varsayılan bir değer belirtmenize olanak tanır.

    string testString = null;
    Console.WriteLine("The specified string is - " + (testString ?? "not provided"));

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/GNosPU)

Bu mantıksal olarak şuna eşdeğerdir:

    string testString = null;
    if (testString == null)
    {
        Console.WriteLine("The specified string is - not provided");
    }
    else
    {
        Console.WriteLine("The specified string is - " + testString);
    }

veya [üçlü operatör (?:)][1] operatörünü kullanarak:

    string testString = null;
    Console.WriteLine("The specified string is - " + (testString == null ? "not provided" : testString));


[1]: https://www.wikiod.com/tr/docs/c%23/18/operators/6029/ternary-operator#t=201610101110242934481
[2]: https://msdn.microsoft.com/en-us/library/ms173224.aspx

## Boş düşüş ve zincirleme
Sağ işlenen olabilir veya olmayabilirken, sol işlenen boş olabilir. Sonuç buna göre yazılacaktır.

**Null olamaz**

    int? a = null;
    int b = 3;
    var output = a ?? b;
    var type = output.GetType();  

    Console.WriteLine($"Output Type :{type}");
    Console.WriteLine($"Output value :{output}");

**Çıktı:**
>Tür :System.Int32
>değer :3

[Demoyu Görüntüle][1]

**null yapılabilir**

    int? a = null;
    int? b = null;
    var output = a ?? b;

"çıktı", "int?" türünde olacak ve "b" veya "boş" değerine eşit olacaktır.

**Çoklu Birleştirme**

Birleştirme zincirlerde de yapılabilir:

    int? a = null;
    int? b = null;
    int c = 3;
    var output = a ?? b ?? c;

    var type = output.GetType();    
    Console.WriteLine($"Type :{type}");
    Console.WriteLine($"value :{output}");

**Çıktı:**
>Tür :System.Int32
> değer :3

[Demoyu Görüntüle][2]

**Boş Koşullu Zincirleme**

Boş birleştirme operatörü, nesnelerin özelliklerine daha güvenli erişim sağlamak için [boş yayılma operatörü][3] ile birlikte kullanılabilir.

    object o = null;
    var output = o?.ToString() ?? "Default Value";

**Çıktı:**
>Tür :Sistem.Dizesi
>değer :Varsayılan Değer

[Demoyu Görüntüle][4]


[1]: https://dotnetfiddle.net/hKHOcN
[2]: https://dotnetfiddle.net/xC8Bmc
[3]: https://www.wikiod.com/tr/docs/c%23/24/c-sharp-6-0-features/51/null-propagation#t=201607280322338995462
[4]: https://dotnetfiddle.net/nk1QRn

## Yöntem çağrılarıyla boş birleştirme operatörü
Boş birleştirme operatörü, "null" döndürebilecek bir yöntemin varsayılan bir değere geri dönmesini sağlamayı kolaylaştırır.

Boş birleştirme operatörü olmadan:

    string name = GetName();

    if (name == null)
        name = "Unknown!";

Boş birleştirme operatörü ile:

    string name = GetName() ?? "Unknown!";


## Var olanı kullanın veya yeni oluşturun
Bu özelliğin gerçekten yardımcı olduğu yaygın bir kullanım senaryosu, bir koleksiyonda bir nesne aradığınızda ve henüz mevcut değilse yeni bir tane oluşturmanız gerektiğinde ortaya çıkar.

    IEnumerable<MyClass> myList = GetMyList();
    var item = myList.SingleOrDefault(x => x.Id == 2) ?? new MyClass { Id = 2 };

## Boş birleştirme operatörüyle tembel özellik başlatma
    private List<FooBar> _fooBars;
    
    public List<FooBar> FooBars
    {
        get { return _fooBars ?? (_fooBars = new List<FooBar>()); }
    }

".FooBars" özelliğine ilk erişildiğinde, "_fooBars" değişkeni "null" olarak değerlendirilir, dolayısıyla atama deyimine düşerek elde edilen değeri atar ve değerlendirir.

İplik güvenliği
===
Bu, tembel özellikleri uygulamanın **iş parçacığı açısından güvenli değil** yoludur. İş parçacığı güvenli tembellik için, .NET Framework'te yerleşik olarak bulunan [`Lazy<T>`][1] sınıfını kullanın.

C# 6 Sözdizimsel Şeker ifade gövdelerini kullanma
====

C# 6'dan beri bu sözdiziminin, özellik için ifade gövdesi kullanılarak basitleştirilebileceğini unutmayın:

    private List<FooBar> _fooBars;
    
    public List<FooBar> FooBars => _fooBars ?? ( _fooBars = new List<FooBar>() );

Özelliğe sonraki erişimler, '_fooBars' değişkeninde depolanan değeri verecektir.

MVVM modelinde örnek
===

Bu genellikle MVVM modelinde komutlar uygulanırken kullanılır. Komutları bir görünüm modelinin oluşturulmasıyla hevesle başlatmak yerine, komutlar aşağıdaki gibi bu kalıp kullanılarak tembelce başlatılır:

    private ICommand _actionCommand = null;
    public ICommand ActionCommand =>
       _actionCommand ?? ( _actionCommand = new DelegateCommand( DoAction ) );


[1]: https://www.wikiod.com/tr/docs/c%23/1192/singleton-implementation/6795/lazy-thread-safe-singleton-using-lazyt

