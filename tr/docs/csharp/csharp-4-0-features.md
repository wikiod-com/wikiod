---
title: "C# 4.0 Özellikleri"
slug: "c-40-ozellikleri"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## İsteğe bağlı parametreler ve adlandırılmış bağımsız değişkenler
Bu argüman bir İsteğe Bağlı Argüman ise, çağrıdaki argümanı atlayabiliriz.
Her İsteğe Bağlı Argümanın kendi varsayılan değeri vardır
Değeri sağlamazsak varsayılan değeri alacaktır
İsteğe Bağlı Bağımsız Değişkenin varsayılan değeri,
1. Sabit ifade.
2. Enum veya struct gibi bir değer türü olmalıdır.
3. default(valueType) formunun bir ifadesi olmalıdır

Parametre listesinin sonunda ayarlanmalıdır.

Varsayılan değerlere sahip yöntem parametreleri:
 
    public void ExampleMethod(int required, string optValue = "test", int optNum = 42)
    {
        //...
    }

MSDN tarafından söylendiği gibi, Adlandırılmış bir argüman,

Parametrenin adını ilişkilendirerek argümanı işleve iletmenizi sağlar
Her zaman farkında olmadığımız parametre pozisyonunu hatırlamaya gerek yok.
Çağrılan fonksiyonun parametreler listesindeki parametrelerin sırasına bakmanıza gerek yoktur.
Adına göre her argüman için parametre belirleyebiliriz.

Adlandırılmış argümanlar:
    
    // required = 3, optValue = "test", optNum = 4
    ExampleMethod(3, optNum: 4);
    // required = 2, optValue = "foo", optNum = 42
    ExampleMethod(2, optValue: "foo");
    // required = 6, optValue = "bar", optNum = 1
    ExampleMethod(optNum: 1, optValue: "bar", required: 6);

**Adlandırılmış Argüman kullanmanın sınırlandırılması**

Adlandırılmış bağımsız değişken belirtimi, tüm sabit bağımsız değişkenler belirtildikten sonra görünmelidir.

Sabit bir argümandan önce adlandırılmış bir argüman kullanırsanız, aşağıdaki gibi bir derleme zamanı hatası alırsınız.

[![buraya resim açıklamasını girin][1]][1]

Adlandırılmış bağımsız değişken belirtimi, tüm sabit bağımsız değişkenler belirtildikten sonra görünmelidir


[1]: http://i.stack.imgur.com/pzWLh.png

## Varyans
Genel arayüzler ve temsilciler, tür parametrelerini [_covariant_](https://www.wikiod.com/tr/docs/c%23/27/generics/7362/covariance#t=201607241842437571339) veya [_contravariant_](http://) olarak işaretleyebilir. stackoverflow.com/documentation/c%23/27/generics/7372/contravariance#t=201607241842437571339) sırasıyla "out" ve "in" anahtar sözcüklerini kullanarak. Bu bildirimler daha sonra hem örtük hem de açık tür dönüşümleri ve hem derleme zamanı hem de çalışma zamanı için dikkate alınır.

Örneğin, mevcut "IEnumerable<T>" arabirimi, kovaryant olarak yeniden tanımlanmıştır:

    interface IEnumerable<out T>
    {
        IEnumerator<T> GetEnumerator();
    }

Mevcut arabirim IComparer<T>, çelişkili olarak yeniden tanımlandı:

    public interface IComparer<in T>
    {
        int Compare(T x, T y);
    }

## Dinamik üye arama
C# tipi sisteme yeni bir sözde tip 'dinamik' eklendi. "System.Object" olarak kabul edilir, ancak ek olarak, herhangi bir üye erişimine (yöntem çağrısı, alan, özellik veya dizin oluşturucu erişimi veya bir temsilci çağırma) veya bu tür bir değer üzerinde bir operatörün uygulamasına herhangi bir tür olmaksızın izin verilir. kontrol edilir ve çözümü çalışma zamanına kadar ertelenir. Bu, ördek yazma veya geç bağlama olarak bilinir. Örneğin:
 
    // Returns the value of Length property or field of any object
    int GetLength(dynamic obj)
    {
        return obj.Length;
    }
      
    GetLength("Hello, world");        // a string has a Length property,
    GetLength(new int[] { 1, 2, 3 }); // and so does an array,
    GetLength(42);                    // but not an integer - an exception will be thrown
                                      // in GetLength method at run-time

Bu durumda, daha ayrıntılı Yansımayı önlemek için dinamik tür kullanılır. Hala kaputun altında Yansıma kullanıyor, ancak önbelleğe alma sayesinde genellikle daha hızlı.

Bu özellik, öncelikle dinamik dillerle birlikte çalışabilirliği hedefler.

    // Initialize the engine and execute a file
    var runtime = ScriptRuntime.CreateFromConfiguration();
    dynamic globals = runtime.Globals;
    runtime.ExecuteFile("Calc.rb");
    
    // Use Calc type from Ruby
    dynamic calc = globals.Calc.@new();
    calc.valueA = 1337;
    calc.valueB = 666;
    dynamic answer = calc.Calculate();

Dinamik tür, çoğunlukla statik olarak yazılan kodda bile uygulamalara sahiptir; örneğin, Ziyaretçi kalıbını uygulamadan [çift gönderimi](https://en.wikipedia.org/wiki/Double_dispatch) mümkün kılar.

## COM kullanırken isteğe bağlı ref anahtar sözcüğü
COM arabirimleri tarafından sağlanan yöntemlere çağrı yapılırken, yöntemleri çağıranlar için ref anahtar sözcüğü artık isteğe bağlıdır. İmzalı bir COM yöntemi verildi

    void Increment(ref int x);
çağrı şimdi ya olarak yazılabilir

    Increment(0); // no need for "ref" or a place holder variable any more

