---
title: "diziler"
slug: "diziler"
draft: false
images: []
weight: 9673
type: docs
toc: true
---

## Sözdizimi
- **Bir dizi bildirme:**

    &lt;type>[] &lt;name>;

- **İki boyutlu dizi bildiriliyor:**

    &lt;type>[,] &lt;name> = new &lt;type>[&lt;value>, &lt;value>];

- **Pürüzlü Dizi Bildirme:**

    &lt;type>[][] &lt;name> = new &lt;type>[&lt;value>][];

- **Pürüzlü Dizi için bir alt dizi bildirme:**

    &lt;name>[&lt;value>]  = new &lt;type>[&lt;value>];

- **Bir diziyi değerler olmadan başlatma:**

    &lt;name> = new &lt;type>[&lt;length>];

- **Bir diziyi değerlerle başlatma:**

    &lt;name> = new &lt;type>[] {&lt;value>, &lt;value>, &lt;value>, ...};

- **İki boyutlu bir diziyi değerlerle başlatma:**

    &lt;name> = new &lt;type>[,] { {&lt;value>, &lt;value>}, {&lt;value>, &lt;value>}, ...};

- **i dizinindeki bir öğeye erişme:**

    &lt;name>[i]

- **Dizin uzunluğunu alma:**

    &lt;name>.Length



C#'ta dizi bir başvuru türüdür, bu da *null yapılabilir* olduğu anlamına gelir.

Bir dizinin sabit bir uzunluğu vardır, yani ona ".Add()" veya ".Remove()" yapamazsınız. Bunları kullanmak için dinamik bir diziye ihtiyacınız olacaktır - 'List' veya 'ArrayList'.

## Bir dizi bildirme
Bir dizi bildirilebilir ve köşeli parantez (`[]`) başlatma sözdizimi kullanılarak varsayılan değerle doldurulabilir. Örneğin, 10 tam sayıdan oluşan bir dizi oluşturmak:

    int[] arr = new int[10];

C#'daki dizinler sıfır tabanlıdır. Yukarıdaki dizinin indeksleri 0-9 olacaktır. Örneğin:

    int[] arr = new int[3] {7,9,4};
    Console.WriteLine(arr[0]); //outputs 7
    Console.WriteLine(arr[1]); //outputs 9

Bu, sistemin eleman indeksini 0'dan saymaya başladığı anlamına gelir. Ayrıca, dizilerin elemanlarına erişim **sabit zamanda** yapılır. Bu, dizinin ilk öğesine erişmenin, ikinci öğeye, üçüncü öğeye vb. erişme maliyetinin (zaman açısından) aynı olduğu anlamına gelir.

Ayrıca bir diziyi başlatmadan bir diziye çıplak başvuru bildirebilirsiniz.

    int[] arr = null;   // OK, declares a null reference to an array.
    int first = arr[0]; // Throws System.NullReferenceException because there is no actual array.

Koleksiyon başlatma sözdizimi kullanılarak özel değerlerle bir dizi de oluşturulabilir ve başlatılabilir:

    int[] arr = new int[] { 24, 2, 13, 47, 45 };

Bir dizi değişkeni bildirilirken "new int[]" kısmı atlanabilir. Bu bağımsız bir _ifade_ değildir, bu nedenle onu farklı bir çağrının parçası olarak kullanmak işe yaramaz (bunun için "yeni" olan sürümü kullanın):

    int[] arr = { 24, 2, 13, 47, 45 };  // OK
    int[] arr1;
    arr1 = { 24, 2, 13, 47, 45 };       // Won't compile

**Örtülü olarak yazılan diziler**

Alternatif olarak, "var" anahtar sözcüğüyle birlikte, belirli tür atlanabilir, böylece dizinin türü çıkarsanabilir:

    // same as int[]
    var arr = new [] { 1, 2, 3 };
    // same as string[]
    var arr = new [] { "one", "two", "three" };
    // same as double[]
    var arr = new [] { 1.0, 2.0, 3.0 };


## Tekrarlanan varsayılan olmayan bir değerle dolu bir diziyi başlatma
Bildiğimiz gibi, varsayılan değerlere sahip bir dizi bildirebiliriz:

    int[] arr = new int[10];

Bu, dizinin her bir elemanının "0" değerine sahip olduğu ('int' türünün varsayılan değeri) 10 tam sayıdan oluşan bir dizi oluşturacaktır.

Varsayılan olmayan bir değerle başlatılan bir dizi oluşturmak için [`System.Linq`][2] Ad Alanından [`Enumerable.Repeat`][1] kullanabiliriz:

1. **"true"** ile doldurulmuş 10 boyutunda bir "bool" dizisi oluşturmak için

        bool[] booleanArray = Enumerable.Repeat(true, 10).ToArray(); 

2. **"100"** ile doldurulmuş 5 boyutlu bir "int" dizisi oluşturmak için

        int[] intArray = Enumerable.Repeat(100, 5).ToArray();

3. **"C#"** ile doldurulmuş 5 boyutunda bir "dize" dizisi oluşturmak için

        string[] strArray = Enumerable.Repeat("C#", 5).ToArray();

[1]: https://msdn.microsoft.com/en-us/library/bb348899(v=vs.100).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.linq%28v=vs.100%29.aspx

## Dizileri kopyalama
Hem kaynak hem de hedefte 0 dizininden başlayarak statik 'Array.Copy()' yöntemiyle kısmi bir dizi kopyalama:

    var sourceArray = new int[] { 11, 12, 3, 5, 2, 9, 28, 17 };
    var destinationArray= new int[3];
    Array.Copy(sourceArray, destinationArray, 3);

    // destinationArray will have 11,12 and 3

Tüm diziyi, kaynağın 0 dizininden ve hedefte belirtilen dizinden başlayarak "CopyTo()" örnek yöntemiyle kopyalama:

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = new int[6];
    sourceArray.CopyTo(destinationArray, 2);

    // destinationArray will have 0, 0, 11, 12, 7 and 0

"Klon", bir dizi nesnesinin bir kopyasını oluşturmak için kullanılır.

    var sourceArray = new int[] { 11, 12, 7 };
    var destinationArray = (int)sourceArray.Clone();

    //destinationArray will be created and will have 11,12,17.

Hem 'CopyTo' hem de 'Clone' sığ kopyalama gerçekleştirir, bu da içeriğin orijinal dizideki öğelerle aynı nesneye referanslar içerdiği anlamına gelir.

## Dizileri eşitlik için karşılaştırma
LINQ, iki "IEnumerable"ın eşitliğini kontrol etmek için yerleşik bir işlev sağlar ve bu işlev dizilerde kullanılabilir.

[`SequenceEqual`][1] işlevi, diziler aynı uzunluğa sahipse ve karşılık gelen indekslerdeki değerler eşitse "true", aksi takdirde "false" değerini döndürür.

    int[] arr1 = { 3, 5, 7 };
    int[] arr2 = { 3, 5, 7 };
    bool result = arr1.SequenceEqual(arr2);
    Console.WriteLine("Arrays equal? {0}", result);

Bu yazdıracak:

<!-- dil: lang-none -->
    Arrays equal? True

[1]: https://msdn.microsoft.com/en-us/library/bb348567(v=vs.110).aspx

## Çok boyutlu diziler
Dizilerin birden fazla boyutu olabilir. Aşağıdaki örnek, on satır ve on sütundan oluşan iki boyutlu bir dizi oluşturur:

    int[,] arr = new int[10, 10];

Üç boyutlu bir dizi:

    int[,,] arr = new int[10, 10, 10];

Bildiri üzerine diziyi de başlatabilirsiniz:

    int[,] arr = new int[4, 2] { {1, 1}, {2, 2}, {3, 3}, {4, 4} };

    // Access a member of the multi-dimensional array:
    Console.Out.WriteLine(arr[3, 1]);  // 4

 

## Dizi değerlerini alma ve ayarlama
    int[] arr = new int[] { 0, 10, 20, 30}; 

    // Get 
    Console.WriteLine(arr[2]); // 20

    // Set 
    arr[2] = 100;

    // Get the updated value
    Console.WriteLine(arr[2]); // 100


## Bir dizi üzerinde yineleme
    int[] arr = new int[] {1, 6, 3, 3, 9};

    for (int i = 0; i < arr.Length; i++) 
    {
        Console.WriteLine(arr[i]);
    }

foreach kullanarak:

    foreach (int element in arr) 
    {
        Console.WriteLine(element);
    }

işaretçilerle güvenli olmayan erişimi kullanma
https://msdn.microsoft.com/en-ca/library/y31yhkeb.aspx
 

    unsafe
    {
        int length = arr.Length;
        fixed (int* p = arr)
        {
            int* pInt = p;
            while (length-- > 0)
            {
                Console.WriteLine(*pInt);
                pInt++;// move pointer to next element
            }
        }
    }

Çıktı:

> 1
> 6
> 3
> 3
> 9


## Pürüzlü diziler
Çentikli diziler, ilkel türler yerine diziler (veya diğer koleksiyonlar) içeren dizilerdir. Bir dizi dizi gibidir - her dizi öğesi başka bir dizi içerir.<br/><br/>
Çok boyutlu dizilere benzerler, ancak küçük bir farkları vardır - çok boyutlu diziler sabit sayıda satır ve sütunla sınırlı olduğundan, pürüzlü dizilerle her satırın farklı sayıda sütunu olabilir.

**Pürüzlü bir dizi bildirme**

Örneğin, 8 sütunlu pürüzlü bir dizi bildirmek:

    int[][] a = new int[8][];
İkinci "[]" sayı olmadan başlatılır. Alt dizileri başlatmak için bunu ayrı ayrı yapmanız gerekir:

    for (int i = 0; i < a.length; i++) 
    {
        a[i] = new int[10];
    }

**Değerleri Alma/Ayarlama**

Şimdi, alt dizilerden birini almak kolaydır. "a"nın 3. sütunundaki tüm sayıları yazdıralım:

    for (int i = 0; i < a[2].length; i++)
    {
        Console.WriteLine(a[2][i]);
    }
Belirli bir değer alma:

    a[<row_number>][<column_number>]
Belirli bir değerin ayarlanması:

    a[<row_number>][<column_number>] = <value>

**Unutmayın**: Her zaman çok boyutlu diziler (matrisler) yerine pürüzlü dizilerin (dizi dizileri) kullanılması önerilir. Kullanımı daha hızlı ve daha güvenlidir.

----------

**Parantezlerin sırasına dikkat edin**

"int" in tek boyutlu dizilerinin beş boyutlu dizilerinden oluşan üç boyutlu bir dizi düşünün. Bu, C# ile şu şekilde yazılmıştır:

    int[,,][,,,,][] arr = new int[8, 10, 12][,,,,][];

CLR tipi sistemde, parantezlerin sıralanması kuralı tersine çevrilir, bu nedenle yukarıdaki "arr" örneğinde:

        arr.GetType().ToString() == "System.Int32[][,,,,][,,]"

Ve aynı şekilde:

        typeof(int[,,][,,,,][]).ToString() == "System.Int32[][,,,,][,,]"

## Sıralı sayılar dizisi oluşturma
LINQ, sıralı sayılarla dolu bir koleksiyon oluşturmayı kolaylaştıran bir yöntem sağlar. Örneğin, 1 ile 100 arasındaki tam sayıları içeren bir dizi tanımlayabilirsiniz.

[`Enumerable.Range`][1] yöntemi, belirtilen bir başlangıç ​​konumundan ve bir dizi öğeden tamsayı sayıları dizisi oluşturmamıza olanak tanır.

Yöntem iki argüman alır: başlangıç ​​değeri ve oluşturulacak eleman sayısı.

    Enumerable.Range(int start, int count)

_"sayı"nın negatif olamayacağına dikkat edin._

## Kullanım:

    int[] sequence = Enumerable.Range(1, 100).ToArray();

Bu, 1'den 100'e kadar olan sayıları içeren bir dizi oluşturacaktır (`[1, 2, 3, ..., 98, 99, 100]`).

"Range" yöntemi bir "IEnumerable<int>" döndürdüğünden, bunun üzerinde diğer LINQ yöntemlerini kullanabiliriz:

    int[] squares = Enumerable.Range(2, 10).Select(x => x * x).ToArray();

Bu, "4": "[4, 9, 16, ..., 100, 121]" ile başlayan 10 tam sayı kare içeren bir dizi oluşturacaktır.


[1]: https://msdn.microsoft.com/en-us/library/system.linq.enumerable.range(v=vs.110).aspx

## Dizi kovaryansı
    string[] strings = new[] {"foo", "bar"};
    object[] objects = strings; // implicit conversion from string[] to object[]

Bu dönüştürme, tür açısından güvenli değildir. Aşağıdaki kod bir çalışma zamanı istisnası oluşturacaktır:

    string[] strings = new[] {"Foo"};
    object[] objects = strings;

    objects[0] = new object(); // runtime exception, object is not string
    string str = strings[0];   // would have been bad if above assignment had succeeded

## Bir dizinin başka bir dizi içerip içermediğini kontrol etme


## IEnumerable<> örnekleri olarak diziler
Tüm diziler, genel olmayan "IList" arabirimini uygular (ve dolayısıyla genel olmayan "ICollection" ve "IEnumerable" temel arabirimleri).

Daha da önemlisi, tek boyutlu diziler, içerdikleri veri türü için 'IList<>' ve 'IReadOnlyList<>' genel arabirimlerini (ve bunların temel arabirimlerini) uygular. Bu, genel numaralandırılabilir türler olarak ele alınabilecekleri ve önce onları dizi olmayan bir forma dönüştürmeye gerek kalmadan çeşitli yöntemlere iletilebilecekleri anlamına gelir.

    int[] arr1 = { 3, 5, 7 };
    IEnumerable<int> enumerableIntegers = arr1; //Allowed because arrays implement IEnumerable<T>
    List<int> listOfIntegers = new List<int>();
    listOfIntegers.AddRange(arr1); //You can pass in a reference to an array to populate a List.

Bu kodu çalıştırdıktan sonra, 'listOfIntegers' listesi 3, 5 ve 7 değerlerini içeren bir 'List<int>' içerecektir.

"IEnumerable<>" desteği, dizilerin LINQ ile sorgulanabileceği anlamına gelir, örneğin "arr1.Select(i => 10 * i)".

