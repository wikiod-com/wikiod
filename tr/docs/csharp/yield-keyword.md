---
title: "Getiri Anahtar Kelimesi"
slug: "getiri-anahtar-kelimesi"
draft: false
images: []
weight: 8758
type: docs
toc: true
---

Bir ifadede getiri anahtar sözcüğünü kullandığınızda, içinde göründüğü yöntem, işleç veya get erişimcisinin bir yineleyici olduğunu belirtirsiniz. Bir yineleyici tanımlamak için verimi kullanmak, özel bir koleksiyon türü için IEnumerable ve IEnumerator desenini uyguladığınızda, açık bir ekstra sınıfa (bir numaralandırma için durumu tutan sınıf) olan ihtiyacı ortadan kaldırır.

## Sözdizimi
- getiri getirisi [TYPE]
- verim kesintisi

'yield' anahtar sözcüğünü, 'IEnumerable', 'IEnumerable<T>', 'IEnumerator' veya 'IEnumerator<T>' dönüş tipine sahip bir yönteme koymak, derleyiciye dönüş tipinde ('IEnumerable) bir uygulama oluşturmasını söyler. ` veya `IEnumerator`), döngü yapıldığında, her sonucu almak için yöntemi her "verim"e kadar çalıştırır.

"verim" anahtar sözcüğü, teorik olarak sınırsız bir dizinin "sonraki" öğesini döndürmek istediğinizde yararlıdır, bu nedenle tüm diziyi önceden hesaplamak imkansız olacaktır veya döndürmeden önce tüm değerler dizisinin hesaplanması istenmeyen bir duraklamaya yol açacaktır. Kullanıcı.

'verim kırılması', diziyi herhangi bir zamanda sonlandırmak için de kullanılabilir.

'yield' anahtar sözcüğü, dönüş türü olarak 'IEnumerable<T>' gibi bir yineleyici arabirim türü gerektirdiğinden, bunu bir 'Task<IEnumerable<T>>' nesnesi döndürdüğü için zaman uyumsuz bir yöntemde kullanamazsınız.

**Daha fazla okuma**

- https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx

## Basit Kullanım
'yield' anahtar sözcüğü, bir arayan tarafından döndürülen koleksiyon üzerinde yinelenirken değerleri tembel bir şekilde oluşturulan bir 'IEnumerable' veya 'IEnumerator' (ve bunların türetilmiş genel varyantları) döndüren bir işlevi tanımlamak için kullanılır. [Açıklamalar bölümünde](https://www.wikiod.com/tr/docs/c%23/61/yield-keyword#remarks) amaç hakkında daha fazla bilgi edinin.

Aşağıdaki örnekte, bir "for" döngüsü içinde olan bir getiri döndürme ifadesi vardır.

    public static IEnumerable<int> Count(int start, int count)
    {
        for (int i = 0; i <= count; i++)
        {
            yield return start + i;
        }
    }

O zaman onu arayabilirsin:

    foreach (int value in Count(start: 4, count: 10))
    {
        Console.WriteLine(value);
    }

**Konsol Çıkışı**

    4
    5
    6
    ...
    14


[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/qtKObr)

'foreach' ifade gövdesinin her yinelemesi, 'Count' yineleyici işlevine bir çağrı oluşturur. Yineleyici işlevine yapılan her çağrı, "for" döngüsünün bir sonraki yinelemesi sırasında ortaya çıkan "verim dönüşü" ifadesinin bir sonraki yürütülmesine ilerler.

## Argümanları doğru kontrol etme
Dönüş değeri numaralandırılana kadar bir yineleyici yöntemi yürütülmez. Bu nedenle, yineleyicinin dışında ön koşullar ileri sürmek avantajlıdır.

    public static IEnumerable<int> Count(int start, int count)
    {
        // The exception will throw when the method is called, not when the result is iterated
        if (count < 0)
            throw new ArgumentOutOfRangeException(nameof(count));

        return CountCore(start, count);
    }

    private static IEnumerable<int> CountCore(int start, int count)
    {
        // If the exception was thrown here it would be raised during the first MoveNext()
        // call on the IEnumerator, potentially at a point in the code far away from where
        // an incorrect value was passed.
        for (int i = 0; i < count; i++)
        {
            yield return start + i;
        }
    }

**Arayan Taraf Kodu (Kullanım):**
        
    // Get the count
    var count = Count(1,10);
    // Iterate the results
    foreach(var x in count)
    {
        Console.WriteLine(x);
    }
**Çıktı:**
>1
>2
>3
>4
>5
>6
>7
>8
>9
>10

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/yIYxo6)

Bir yöntem, bir numaralandırılabilir oluşturmak için "verim"i kullandığında, derleyici, yinelendiğinde kodu "verim"e kadar çalıştıracak bir durum makinesi oluşturur. Daha sonra elde edilen öğeyi döndürür ve durumunu kaydeder.

Bu, yöntemi ilk çağırdığınızda (çünkü bu durum makinesini oluşturduğundan), yalnızca ilk öğeye erişmeye çalıştığınızda (çünkü ancak o zaman kod yapar) geçersiz argümanları ('null' geçerek vb.) bulamayacağınız anlamına gelir. yöntem içinde durum makinesi tarafından çalıştırılır). İlk önce argümanları kontrol eden normal bir metoda sararak, metod çağrıldığında onları kontrol edebilirsiniz. Bu, hızlı başarısızlığa bir örnektir.

C# 7+ kullanırken, 'CountCore' işlevi, bir _yerel işlev_ olarak 'Count' işlevine uygun bir şekilde gizlenebilir. Örneğe bakın [burada](https://www.wikiod.com/tr/docs/c%23/1936/c-sharp-7-0-features/6330/local-functions#t=201607251321358412005#t=201607251057101259341).

## Erken sonlandırma
Mevcut "verim" yöntemlerinin işlevselliğini, iç döngünün yürütülmesini durdurmak için bir "verim kesintisi" çağırarak işlev içinde bir sonlandırma koşulu tanımlayabilecek bir veya daha fazla değer veya öğe ileterek genişletebilirsiniz.

    public static IEnumerable<int> CountUntilAny(int start, HashSet<int> earlyTerminationSet)
    {
        int curr = start;

        while (true)
        {
            if (earlyTerminationSet.Contains(curr))
            {
                // we've hit one of the ending values
                yield break;
            }

            yield return curr;

            if (curr == Int32.MaxValue)
            {
                // don't overflow if we get all the way to the end; just stop
                yield break;
            }

            curr++;
        }
    }

Yukarıdaki yöntem, belirli bir "başlangıç" konumundan "earlyTerminationSet" içindeki değerlerden biriyle karşılaşılıncaya kadar yinelenir.

    // Iterate from a starting point until you encounter any elements defined as 
    // terminating elements
    var terminatingElements = new HashSet<int>{ 7, 9, 11 };
    // This will iterate from 1 until one of the terminating elements is encountered (7)
    foreach(var x in CountUntilAny(1,terminatingElements))
    {
        // This will write out the results from 1 until 7 (which will trigger terminating)
        Console.WriteLine(x);
    }
**Çıktı:**
>1
>2
>3
>4
>5
>6

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/pctiOz)

## Daha Uygun Kullanım
    public IEnumerable<User> SelectUsers()
    {
        // Execute an SQL query on a database.
        using (IDataReader reader = this.Database.ExecuteReader(CommandType.Text, "SELECT Id, Name FROM Users"))
        {
            while (reader.Read())
            {
                int id = reader.GetInt32(0);
                string name = reader.GetString(1);
                yield return new User(id, name);
            }
        }
    }

Elbette bir SQL veritabanından bir "IEnumerable<User>" almanın başka yolları da vardır - bu, "eleman dizisi" semantiğine sahip herhangi bir şeyi bir "IEnumerable<T>'ye dönüştürmek için "verim"i kullanabileceğinizi gösterir. ` birisi üzerinde yineleyebilir.

## Tembel Değerlendirme
Yalnızca "foreach" ifadesi bir sonraki öğeye geçtiğinde, yineleyici blok bir sonraki "verim" ifadesine kadar değerlendirme yapar.

Aşağıdaki örneği göz önünde bulundurun:

    private IEnumerable<int> Integers()
    {
        var i = 0;
        while(true)
        {
            Console.WriteLine("Inside iterator: " + i);
            yield return i;
            i++;
        }
    }
    
    private void PrintNumbers()
    {
        var numbers = Integers().Take(3);
        Console.WriteLine("Starting iteration");

        foreach(var number in numbers)
        {
            Console.WriteLine("Inside foreach: " + number);
        }
    }


Bu çıktı:

>Yinelemeyi başlatma
>İç yineleyici: 0
>İç foreach: 0
>İç yineleyici: 1
>Foreach içinde: 1
>İç yineleyici: 2
>İç foreach: 2

[Demoyu Görüntüle][1]

Sonuç olarak:

- `Integers().Take(3);` satırı aslında yinelemeyi başlatmadığından, satır yazdırılmadan önce yineleyici yöntemi çağrılsa bile önce "yinelemeyi başlatma" yazdırılır (`IEnumerator.MoveNext()` çağrısı yok) yapıldığı)
- Konsola yazdırılan satırlar, ilk önce yineleyici yönteminin içindeki tüm satırlar yerine, yineleyici yöntemin içindeki satır ile "foreach" içindeki satır arasında değişir.
- Bu program, yineleyici yöntemin asla kopmadığı bir "while true" olmasına rağmen, `.Take()` yöntemi nedeniyle sona erer.


[1]: https://dotnetfiddle.net/2qGV0B

## Deneyin...sonunda
Bir yineleyici yöntemin bir "try...finally" içinde bir getirisi varsa, döndürülen "IEnumerator", "Dispose" çağrıldığında, geçerli değerlendirme noktası içinde olduğu sürece "finally" ifadesini yürütür. "dene" bloğu.

Fonksiyon verildiğinde:

    private IEnumerable<int> Numbers()
    {
        yield return 1;
        try
        {
            yield return 2;
            yield return 3;
        }
        finally
        {
            Console.WriteLine("Finally executed");
        }
    }

Arama yaparken:

    private void DisposeOutsideTry()
    {
        var enumerator = Numbers().GetEnumerator();

        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.Dispose();
    }

Sonra yazdırır:
>1

[Demoyu Görüntüle][1]

Arama yaparken:

    private void DisposeInsideTry()
    {
        var enumerator = Numbers().GetEnumerator();

        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.Dispose();
    }

Sonra yazdırır:
>1
>2
>Sonunda idam edildi

[Demoyu Görüntüle][2]


[1]: https://dotnetfiddle.net/MJt7dt
[2]: https://dotnetfiddle.net/HlMroV

## IEnumerable<T> uygularken bir IEnumerator<T> oluşturmak için verimi kullanma
"IEnumerable<T>" arabiriminin, bir "IEnumerator<T>" döndüren "GetEnumerator()" adlı tek bir yöntemi vardır.

'yield' anahtar sözcüğü doğrudan bir 'IEnumerable<T>' oluşturmak için kullanılabilirken, bir 'IEnumerator<T>' oluşturmak için *ayrıca* tam olarak aynı şekilde kullanılabilir. Değişen tek şey metodun dönüş tipidir.

Bu, "IEnumerable<T>" uygulayan kendi sınıfımızı oluşturmak istiyorsak faydalı olabilir:

    public class PrintingEnumerable<T> : IEnumerable<T>
    {
        private IEnumerable<T> _wrapped;
    
        public PrintingEnumerable(IEnumerable<T> wrapped)
        {
            _wrapped = wrapped;
        }
    
        // This method returns an IEnumerator<T>, rather than an IEnumerable<T>
        // But the yield syntax and usage is identical.
        public IEnumerator<T> GetEnumerator()
        {
            foreach(var item in _wrapped)
            {
                Console.WriteLine("Yielding: " + item);
                yield return item;
            }
        }
    
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

(Bu özel örneğin yalnızca açıklayıcı olduğunu ve bir "IEnumerable<T>" döndüren tek bir yineleyici yöntemiyle daha temiz bir şekilde uygulanabileceğini unutmayın.)
    

## İstekli değerlendirme
"verim" anahtar sözcüğü, koleksiyonun tembel bir şekilde değerlendirilmesini sağlar. Tüm koleksiyonun belleğe zorla yüklenmesine **istekli değerlendirme** denir.

Aşağıdaki kod bunu gösterir:

    IEnumerable<int> myMethod()
    {
        for(int i=0; i <= 8675309; i++)
        {
            yield return i;
        }
    }
    ...
    // define the iterator
    var it = myMethod.Take(3);
    // force its immediate evaluation
    // list will contain 0, 1, 2
    var list = it.ToList();

'ToList', 'ToDictionary' veya 'ToArray' çağrılması, tüm öğeleri bir koleksiyona alarak, numaralandırmanın hemen değerlendirilmesini zorlayacaktır.

## Numaralandırılabilir döndüren bir yöntem içinde başka bir Numaralandırılabilir döndür
    public IEnumerable<int> F1()
    {
        for (int i = 0; i < 3; i++)
            yield return i;
    
        //return F2(); // Compile Error!!
        foreach (var element in F2())
            yield return element;
    }
    
    public int[] F2()
    {
        return new[] { 3, 4, 5 };
    }

## Tembel Değerlendirme Örneği: Fibonacci Sayıları
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Numerics; // also add reference to System.Numberics
    
    namespace ConsoleApplication33
    {
        class Program
        {
            private static IEnumerable<BigInteger> Fibonacci()
            {
                BigInteger prev = 0;
                BigInteger current = 1;
                while (true)
                {
                    yield return current;
                    var next = prev + current;
                    prev = current;
                    current = next;
                }
            }
    
            static void Main()
            {
                // print Fibonacci numbers from 10001 to 10010
                var numbers = Fibonacci().Skip(10000).Take(10).ToArray();
                Console.WriteLine(string.Join(Environment.NewLine, numbers));
            }
        }
    }

Başlık altında nasıl çalışır (IL Disaambler aracında ortaya çıkan .exe dosyasının derlenmesini öneririm):
1. C# derleyicisi, "IEnumerable<BigInteger>" ve "IEnumerator<BigInteger>" (ildasm'da "<Fibonacci>d__0") uygulayan bir sınıf oluşturur.
2. Bu sınıf bir durum makinesi uygular. Durum, yöntemdeki mevcut konum ve yerel değişkenlerin değerlerinden oluşur.
3. En ilginç kod `bool IEnumerator.MoveNext()` yöntemindedir. Temel olarak, "MoveNext()" ne yapar:
- Mevcut durumu geri yükler. 'prev' ve 'current' gibi değişkenler sınıfımızda alanlar haline gelir (ildasm'da '<current>5__2' ve '<prev>5__1'). Bizim yöntemimizde iki konumumuz var ('<>1__durum'): ilki açılış küme ayracında, ikincisi 'getiri getirisinde'.
- Bir sonraki "verim dönüşü" veya "verim kesintisi"/"}"ne kadar kodu yürütür.
- "Verim getirisi" için elde edilen değer kaydedilir, böylece "Current" özelliği onu döndürebilir. 'true' döndürülür. Bu noktada, bir sonraki 'MoveNext' çağrısı için mevcut durum tekrar kaydedilir.
- `verim kesintisi`/`}` yöntemi için sadece `yanlış` döndürür, yani yineleme yapılır.

Ayrıca, 10001. sayının 468 bayt uzunluğunda olduğunu unutmayın. Durum makinesi yalnızca "geçerli" ve "önceki" değişkenleri alan olarak kaydeder. Sıradaki tüm sayıları birinciden 10000'e kadar kaydetmek istersek, tüketilen bellek boyutu 4 megabaytın üzerinde olacaktır. Bu nedenle tembel değerlendirme, uygun şekilde kullanılırsa bazı durumlarda bellek ayak izini azaltabilir.

## Kesinti ve verim kesintisi arasındaki fark
"Mola" yerine "verim molası" kullanmak, sanıldığı kadar açık olmayabilir. İnternette ikisinin birbirinin yerine kullanılabildiği ve gerçekten farkı göstermediği pek çok kötü örnek var.

Kafa karıştırıcı olan kısım, her iki anahtar kelimenin (veya anahtar kelime öbeklerinin) yalnızca döngüler içinde anlam ifade etmesidir (`foreach`, `while`...) Peki ne zaman birini diğerine tercih etmeliyim?

Bir yöntemde [`yield`](https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx) anahtar sözcüğünü kullandığınızda, yöntemi etkili bir şekilde [yineleyici]( https://msdn.microsoft.com/en-us/library/mt639331.aspx). Böyle bir yöntemin tek amacı, sonlu veya sonsuz bir koleksiyon üzerinde yineleme yapmak ve öğelerini (çıktısını) vermektir. Amaç yerine getirildikten sonra, yöntemin yürütülmesine devam etmek için hiçbir neden yoktur. Bazen, `}` yönteminin son kapanış parantezinde doğal olarak olur. Ancak bazen yöntemi erken bitirmek istersiniz. Normal (yinelemeyen) bir yöntemde [`return`](https://msdn.microsoft.com/en-us/library/1h3swy84.aspx) anahtar sözcüğünü kullanırsınız. Ancak bir yineleyicide 'dönüş' kullanamazsınız, 'verim molası' kullanmanız gerekir. Başka bir deyişle, bir yineleyici için "verim kesintisi", standart bir yöntem için "dönüş" ile aynıdır. Oysa [`break`](https://msdn.microsoft.com/en-us/library/adbctzc4.aspx) ifadesi en yakın döngüyü sonlandırır.

Hadi bazı örneklere bakalım:

```
    /// <summary>
    /// Yields numbers from 0 to 9
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9}</returns>
    public static IEnumerable<int> YieldBreak()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Indicates that the iteration has ended, everything 
                // from this line on will be ignored
                yield break;
            }
        }
        yield return 10; // This will never get executed
    }
```

```
    /// <summary>
    /// Yields numbers from 0 to 10
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9,10}</returns>
    public static IEnumerable<int> Break()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Terminates just the loop
                break;
            }
        }
        // Execution continues
        yield return 10;
    }
```

