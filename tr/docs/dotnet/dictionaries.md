---
title: "sözlükler"
slug: "sozlukler"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

## Koleksiyon Başlatıcı ile Sözlüğü Başlatma
    // Translates to `dict.Add(1, "First")` etc.
    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

    // Translates to `dict[1] = "First"` etc.
    // Works in C# 6.0.
    var dict = new Dictionary<int, string>()
    {
        [1] = "First",
        [2] = "Second",
        [3] = "Third"
    };


## Sözlüğe Ekleme
    Dictionary<int, string> dict = new Dictionary<int, string>();
    dict.Add(1, "First");
    dict.Add(2, "Second");
   
    // To safely add items (check to ensure item does not already exist - would throw)
    if(!dict.ContainsKey(3))
    {
       dict.Add(3, "Third");
    }

Alternatif olarak, bir indeksleyici aracılığıyla eklenebilir/ayarlanabilirler. (Bir indeksleyici dahili olarak bir özellik gibi görünür, get ve set'e sahiptir, ancak parantezler arasında belirtilen herhangi bir türde bir parametre alır):

    Dictionary<int, string> dict = new Dictionary<int, string>();
    dict[1] = "First";
    dict[2] = "Second";
    dict[3] = "Third";

Bir istisna atan "Add" yönteminin aksine, sözlükte zaten bir anahtar varsa, dizin oluşturucu yalnızca mevcut değeri değiştirir.

İş parçacığı güvenli sözlük için `ConcurrentDictionary<TKey, TValue>` kullanın:

    var dict = new ConcurrentDictionary<int, string>();
    dict.AddOrUpdate(1, "First", (oldKey, oldValue) => "First");



## Sözlükten değer alma
Bu kurulum kodu verildiğinde:

    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

Girişin değerini anahtar 1 ile okumak isteyebilirsiniz. Eğer anahtar yoksa bir değer almak `KeyNotFoundException` hatası verecektir, bu yüzden önce bunu `ContainsKey` ile kontrol etmek isteyebilirsiniz:

    if (dict.ContainsKey(1))
        Console.WriteLine(dict[1]);

Bunun bir dezavantajı vardır: sözlüğünüzde iki kez arama yapacaksınız (biri varlığı kontrol etmek için, diğeri değeri okumak için). Büyük bir sözlük için bu, performansı etkileyebilir. Neyse ki her iki işlem birlikte gerçekleştirilebilir:

    string value;
    if (dict.TryGetValue(1, out value))
        Console.WriteLine(value);


## Büyük/Küçük Harfe Duyarsız tuşlarla bir Sözlük<string, T> oluşturun.
    var MyDict = new Dictionary<string,T>(StringComparison.InvariantCultureIgnoreCase)

## Bir Sözlüğü Numaralandırma
Bir Sözlük aracılığıyla 3 yoldan biriyle numaralandırabilirsiniz:

**KeyValue çiftlerini kullanma**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(KeyValuePair<int, string> kvp in dict) 
    {
       Console.WriteLine("Key : " + kvp.Key.ToString() + ", Value : " + kvp.Value);
    }

**Anahtarları Kullanma**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(int key in dict.Keys)
    {
        Console.WriteLine("Key : " + key.ToString() + ", Value : " + dict[key]);
    }

**Değerleri Kullanma**

    Dictionary<int, string> dict = new Dictionary<int, string>();
    foreach(string s in dict.Values)
    {
        Console.WriteLine("Value : " + s);
    }

## ConcurrentDictionary<TKey, TValue> (.NET 4.0'dan)
> Olabilecek anahtar/değer çiftlerinin iş parçacığı açısından güvenli bir koleksiyonunu temsil eder.
> aynı anda birden fazla iş parçacığı tarafından erişilir.

Örnek oluşturma
--------------------

Örnek oluşturmak, ```Dictionary<TKey, TValue>``` ile hemen hemen aynı şekilde çalışır, ör.:

    var dict = new ConcurrentDictionary<int, string>();

Ekleme veya Güncelleme
------------------

"Add" yönteminin olmamasına şaşırabilirsiniz, ancak bunun yerine 2 aşırı yüklemeli "AddOrUpdate" vardır:

(1) `AddOrUpdate(TKey tuşu, TValue, Func<TKey, TValue, TValue> addValue)` - *Anahtar zaten yoksa bir anahtar/değer çifti ekler veya belirtilen işlevi kullanarak bir anahtar/değer çiftini günceller anahtar zaten varsa.*

(2) `AddOrUpdate(TKey tuşu, Func<TKey, TValue> addValue, Func<TKey, TValue, TValue> updateValueFactory)` - *Anahtar zaten mevcut değilse, bir anahtar/değer çifti eklemek için belirtilen işlevleri kullanır veya anahtar zaten varsa bir anahtar/değer çiftini güncellemek için.*

Verilen anahtar (1) için zaten mevcutsa, değer ne olursa olsun bir değer ekleme veya güncelleme:

    string addedValue = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => "First");

Bir değer ekleme veya güncelleme, ancak şimdi önceki değere (1) dayalı olarak güncellemedeki değeri değiştirme:

    string addedValue2 = dict.AddOrUpdate(1, "First", (updateKey, valueOld) => $"{valueOld} Updated");

Aşırı yükü (2) kullanarak bir fabrika kullanarak da yeni değer ekleyebiliriz:

    string addedValue3 = dict.AddOrUpdate(1, (key) => key == 1 ? "First" : "Not First", (updateKey, valueOld) => $"{valueOld} Updated");

Değer elde etmek
-----------------
Bir değer almak, `Dictionary<TKey,TValue>` ile aynıdır:

    string value = null;
    bool success = dict.TryGetValue(1, out value);

Bir değer alma veya ekleme
-------------------------
İş parçacığı güvenli bir şekilde bir değer **alacak veya ekleyecek** iki yöntem aşırı yüklemesi vardır.

Anahtar 2 ile değer alın veya anahtar yoksa "İkinci" değeri ekleyin:

    string theValue = dict.GetOrAdd(2, "Second");

Değer yoksa, değer eklemek için bir fabrika kullanma:

    string theValue2 = dict.GetOrAdd(2, (key) => key == 2 ? "Second" : "Not Second." );




## Sözlüğe Numaralandırılabilir (≥ .NET 3.5)
Bir [IEnumerable&lt;T&gt;][2]'den bir [Dictionary&lt;TKey, TValue&gt;][1] oluşturun:

    using System;
    using System.Collections.Generic;
    using System.Linq;

<b></b>

    public class Fruits
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }

<b></b>

    var fruits = new[]
    { 
        new Fruits { Id = 8 , Name = "Apple" },
        new Fruits { Id = 3 , Name = "Banana" },
        new Fruits { Id = 7 , Name = "Mango" },
    };

    
    // Dictionary<int, string>                  key      value
    var dictionary = fruits.ToDictionary(x => x.Id, x => x.Name);

[1]: https://msdn.microsoft.com/en-us/library/xfhwa508(v=vs.100).aspx
[2]: https://msdn.microsoft.com/en-us/library/9eekhta0(v=vs.100).aspx



## Sözlükten Kaldırma
Bu kurulum kodu verildiğinde:

    var dict = new Dictionary<int, string>()
    {
        { 1, "First" },
        { 2, "Second" },
        { 3, "Third" }
    };

Bir anahtarı ve onunla ilişkili değeri kaldırmak için "Kaldır" yöntemini kullanın.

    bool wasRemoved = dict.Remove(2);

Bu kodu çalıştırmak, '2' anahtarını ve onun değerini sözlükten kaldırır. "Kaldır", belirtilen anahtarın bulunup bulunmadığını ve sözlükten kaldırılıp kaldırılmadığını gösteren bir boole değeri döndürür. Anahtar sözlükte yoksa, sözlükten hiçbir şey kaldırılmaz ve false döndürülür (istisna atılmaz).

Anahtarın değerini "null" olarak ayarlayarak bir anahtarı kaldırmaya çalışmak **yanlıştır**.

    dict[2] = null; // WRONG WAY TO REMOVE!

Bu, anahtarı kaldırmaz. Sadece önceki değeri 'null' değeriyle değiştirecektir.

Bir sözlükten tüm anahtarları ve değerleri kaldırmak için "Temizle" yöntemini kullanın.

    dict.Clear();

`Clear` çalıştırıldıktan sonra sözlüğün `Count` değeri 0 olacaktır, ancak dahili kapasite değişmeden kalacaktır.

## Anahtar İçerir(TKey)
Bir "Sözlük"ün belirli bir anahtarı olup olmadığını kontrol etmek için, [`ContainsKey(TKey)`][1] yöntemini çağırabilir ve TKey tipinde bir anahtar sağlayabilirsiniz. Anahtar sözlükte bulunduğunda yöntem bir "bool" değeri döndürür. Örnek için:

    var dictionary = new Dictionary<string, Customer>()
    {
       {"F1", new Customer() { FirstName = "Felipe", ... } },
       {"C2", new Customer() { FirstName = "Carl", ... } },
       {"J7", new Customer() { FirstName = "John", ... } },
       {"M5", new Customer() { FirstName = "Mary", ... } },
    };

Ve Sözlükte bir "C2" olup olmadığını kontrol edin:

    if (dictionary.ContainsKey("C2")) 
    {
       // exists
    }

ContainerKey yöntemi, [`Dictionary<TKey, TValue>`][1] genel sürümünde kullanılabilir.


[1]: https://msdn.microsoft.com/library/htszx2dy(v=vs.110).aspx

## Listelenecek Sözlük
KeyValuePair listesi oluşturma:

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<KeyValuePair<int, int>> list = new List<KeyValuePair<int, int>>();
    list.AddRange(dictionary);

Anahtar listesi oluşturma:

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<int> list = new List<int>();
    list.AddRange(dictionary.Keys);

Bir değerler listesi oluşturma:

    Dictionary<int, int> dictionary = new Dictionary<int, int>();
    List<int> list = new List<int>();
    list.AddRange(dictionary.Values);


## Lazy'1 ile güçlendirilmiş ConcurrentDictionary, yinelenen hesaplamayı azaltır
## Sorun

ConcurrentDictionary, mevcut anahtarların önbellekten anında döndürülmesi söz konusu olduğunda, çoğunlukla kilitsiz ve ayrıntılı bir düzeyde rekabet ederken parlıyor.
Ama ya nesne oluşturma gerçekten pahalıysa, bağlam değiştirmenin maliyetinden daha ağırsa ve bazı önbellek kayıpları meydana gelirse?

Birden çok iş parçacığından aynı anahtar istenirse, çarpışma işlemlerinden kaynaklanan nesnelerden biri sonunda koleksiyona eklenir ve diğerleri atılır, nesneyi oluşturmak için CPU kaynağı ve nesneyi geçici olarak depolamak için bellek kaynağı boşa harcanır. . Diğer kaynaklar da boşa harcanabilir. Bu gerçekten kötü.

## Çözüm

`ConcurrentDictionary<TKey, TValue>` ile `Lazy<TValue>` birleştirebiliriz. Buradaki fikir, ConcurrentDictionary GetOrAdd yönteminin yalnızca koleksiyona gerçekten eklenen değeri döndürebilmesidir. Kaybedilen Lazy nesneleri bu durumda da boşa harcanabilir, ancak Lazy nesnesinin kendisi nispeten ucuz olduğu için bu pek sorun olmaz. Kaybeden Tembel'in Value özelliği hiçbir zaman istenmez, çünkü yalnızca koleksiyona gerçekten eklenmiş olanın Value özelliğini - GetOrAdd yönteminden döndürülen olanı istemek konusunda akıllıyız:

    public static class ConcurrentDictionaryExtensions
    {
        public static TValue GetOrCreateLazy<TKey, TValue>(
            this ConcurrentDictionary<TKey, Lazy<TValue>> d,
            TKey key,
            Func<TKey, TValue> factory)
        {
            return
                d.GetOrAdd(
                    key,
                    key1 =>
                        new Lazy<TValue>(() => factory(key1),
                        LazyThreadSafetyMode.ExecutionAndPublication)).Value;
        }
    }

XmlSerializer nesnelerinin önbelleğe alınması özellikle pahalı olabilir ve uygulama başlangıcında da çok fazla çekişme vardır. Ve bundan daha fazlası var: Bunlar özel serileştiricilerse, süreç yaşam döngüsünün geri kalanı için de bir bellek sızıntısı olacaktır. Bu durumda ConcurrentDictionary'nin tek yararı, işlem yaşam döngüsünün geri kalanında kilit olmayacak, ancak uygulama başlatma ve bellek kullanımı kabul edilemez olacaktır. Bu, Lazy ile güçlendirilmiş ConcurrentDictionary'miz için bir iştir:

    private ConcurrentDictionary<Type, Lazy<XmlSerializer>> _serializers =
        new ConcurrentDictionary<Type, Lazy<XmlSerializer>>();
    
    public XmlSerializer GetSerialier(Type t)
    {
        return _serializers.GetOrCreateLazy(t, BuildSerializer);
    }
    
    private XmlSerializer BuildSerializer(Type t)
    {
        throw new NotImplementedException("and this is a homework");
    }

