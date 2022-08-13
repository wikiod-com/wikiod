---
title: "döngü"
slug: "dongu"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Foreach Döngüsü
foreach, "IEnumerable" öğesini uygulayan bir sınıfın herhangi bir nesnesi üzerinde yinelenir ("IEnumerable<T>" öğesinin ondan miras aldığını unutmayın). Bu tür nesneler bazı yerleşik nesneleri içerir, ancak bunlarla sınırlı değildir: "List<T>", "T[]" (her türden dizi), "Dictionary<TKey, TSource>" ve ayrıca "IQueryable" gibi arabirimler ve 'ICollection' vb.

**sözdizimi**

    foreach(ItemType itemVariable in enumerableObject)
        statement;

**notlar**

1. "ItemType" türünün, öğelerin tam türüyle eşleşmesi gerekmez, yalnızca öğelerin türünden atanabilir olması gerekir.
2. 'ItemType' yerine, alternatif olarak, 'IEnumerable' uygulamasının genel argümanını inceleyerek öğe türünü ennumerableObject'den çıkaracak olan 'var' kullanılabilir.
3. İfade bir blok, tek bir ifade veya hatta boş bir ifade (`;`) olabilir.
4. "enumerableObject", "IEnumerable"ı uygulamıyorsa, kod derlenmeyecektir.
5. Her yineleme sırasında, geçerli öğe 'ItemType'a dönüştürülür (bu belirtilmese, ancak 'var' aracılığıyla derleyici tarafından çıkarsansa bile) ve öğe dönüştürülemezse bir 'InvalidCastException' atılır.

Bu örneği düşünün:

    var list = new List<string>();
    list.Add("Ion");
    list.Add("Andrei");
    foreach(var name in list)
    {
        Console.WriteLine("Hello " + name);
    }

şuna eşittir:

    var list = new List<string>();
    list.Add("Ion");
    list.Add("Andrei");
    IEnumerator enumerator;
    try
    {
        enumerator = list.GetEnumerator();
        while(enumerator.MoveNext())
        {
            string name = (string)enumerator.Current;
            Console.WriteLine("Hello " + name);
        }
    }
    finally
    {
        if (enumerator != null)
            enumerator.Dispose();
    }

## Döngü için
For Loop, işleri belirli bir süre yapmak için harikadır. Bir Süre Döngüsü gibidir, ancak artış koşula dahildir.

Bir For Loop şu şekilde kurulur:

    for (Initialization; Condition; Increment)
    {
        // Code
    }

> Başlatma - Yalnızca döngüde kullanılabilecek yeni bir yerel değişken oluşturur.
> Koşul - Döngü yalnızca koşul doğru olduğunda çalışır.
> Artış - Döngü her çalıştığında değişkenin nasıl değiştiği.

Bir örnek:

    for (int i = 0; i < 5; i++)
    {
        Console.WriteLine(i);
    }

Çıktı:

> 0
> 1
> 2
> 3
> 4

Ayrıca For Loop'ta boşluk bırakabilirsiniz, ancak çalışması için tüm noktalı virgüllere sahip olmanız gerekir.

    int input = Console.ReadLine();    

    for ( ; input < 10; input + 2)
    {
        Console.WriteLine(input);
    }

3 için çıktı:
>3
>5
>7
>9
>11

## Do - Döngü Sürerken
Döngü gövdesinin * sonundaki* koşulu test etmesi dışında bir "while" döngüsüne benzer. Do - while döngüsü, koşulun doğru olup olmadığına bakılmaksızın döngüyü bir kez yürütür.

    int[] numbers = new int[] { 6, 7, 8, 10 };
        
    // Sum values from the array until we get a total that's greater than 10,
    // or until we run out of values.
    int sum = 0;
    int i = 0;
    do
    {
        sum += numbers[i];
        i++;
    } while (sum <= 10 && i < numbers.Length);
        
    System.Console.WriteLine(sum); // 13


## Döngü stilleri
**Süre**

En önemsiz döngü türü. Tek dezavantajı, döngüde nerede olduğunuzu bilmek için içsel bir ipucu olmamasıdır.

    /// loop while the condition satisfies
    while(condition)
    {
        /// do something
    }

**Yapmak**

"while" ile benzerdir, ancak koşul döngünün başında değil sonunda değerlendirilir. Bu, döngülerin en az bir kez yürütülmesiyle sonuçlanır.

    do
    {
        /// do something
    } while(condition) /// loop while the condition satisfies


**İçin**

Başka bir önemsiz döngü stili. Döngü yaparken bir indeks (`i`) artar ve onu kullanabilirsiniz. Genellikle dizileri işlemek için kullanılır.

    for ( int i = 0; i < array.Count; i++ )
    {
        var currentItem = array[i];
        /// do something with "currentItem"
    }

**Her biri için**

"IEnumarable" nesneler arasında döngü yapmanın modernleştirilmiş yolu. İyi bir şey, öğenin dizini veya listenin öğe sayısı hakkında düşünmek zorunda kalmamanız.

    foreach ( var item in someList )
    {
        /// do something with "item"
    }

**Foreach Yöntemi**

Diğer stiller koleksiyonlardaki öğeleri seçmek veya güncellemek için kullanılırken, bu stil genellikle bir koleksiyondaki tüm öğeler için doğrudan *yöntem çağırmak* için kullanılır.

    list.ForEach(item => item.DoSomething());

    // or
    list.ForEach(item => DoSomething(item));

    // or using a method group
    list.ForEach(Console.WriteLine);

    // using an array
    Array.ForEach(myArray, Console.WriteLine);

Bu yöntemin yalnızca 'List<T>' örneklerinde ve 'Array'de statik bir yöntem olarak mevcut olduğunu belirtmek önemlidir - Linq'in **değil** parçasıdır.

**Linq Paralel Foreach**

Tıpkı Linq Foreach gibi, bunun dışında işi paralel bir şekilde yapıyor. Bu, koleksiyondaki tüm öğelerin verilen eylemi aynı anda, aynı anda çalıştıracağı anlamına gelir.

    collection.AsParallel().ForAll(item => item.DoSomething());

    /// or
    collection.AsParallel().ForAll(item => DoSomething(item));

## İç içe geçmiş döngüler
    // Print the multiplication table up to 5s
    for (int i = 1; i <= 5; i++)
    {
        for (int j = 1; j <= 5; j++)
        {
            int product = i * j;
            Console.WriteLine("{0} times {1} is {2}", i, j, product);
        }
    }

## kırmak
Bazen döngü koşulu, döngünün ortasında kontrol edilmelidir. Birincisi, tartışmasız ikincisinden daha zarif:

    for (;;)
    {
        // precondition code that can change the value of should_end_loop expression
    
        if (should_end_loop)
            break;
    
        // do something
    }

Alternatif:

    bool endLoop = false;
    for (; !endLoop;)
    {
        // precondition code that can set endLoop flag
    
        if (!endLoop)
        {
            // do something
        }
    }

Not: İç içe döngülerde ve/veya "switch", basit bir "break"den fazlasını kullanmalıdır.

## Döngü sırasında
    int n = 0;
    while (n < 5) 
    {
        Console.WriteLine(n);
        n++;
    }

Çıktı:

> 0
> 1
> 2
> 3
> 4

IEnumerators bir while döngüsü ile yinelenebilir:

    // Call a custom method that takes a count, and returns an IEnumerator for a list
    // of strings with the names of theh largest city metro areas.
    IEnumerator<string> largestMetroAreas = GetLargestMetroAreas(4);

    while (largestMetroAreas.MoveNext())
    {
        Console.WriteLine(largestMetroAreas.Current);
    }

Örnek çıktı:

> Tokyo/Yokohama
> New York Metrosu
> Sao Paulo
> Seul/Incheon

## devam et
"Mola"ya ek olarak, "devam" anahtar kelimesi de vardır. Döngüyü tamamen kırmak yerine, mevcut yinelemeyi atlayacaktır. Belirli bir değer ayarlanmışsa, bazı kodların yürütülmesini istemiyorsanız bu yararlı olabilir.

İşte basit bir örnek:

    for (int i = 1; i <= 10; i++)
    {
        if (i < 9)
            continue;

        Console.WriteLine(i);
    }

Sonuçlanacak:

    9
    10

**Not:** "Devam" genellikle en çok while veya do-while döngülerinde kullanışlıdır. İyi tanımlanmış çıkış koşullarına sahip döngüler, çok fazla fayda sağlamayabilir.

