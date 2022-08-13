---
title: "Özel Türler"
slug: "ozel-turler"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Tipik olarak bir "yapı" yalnızca performans çok önemli olduğunda kullanılır. Değer türleri yığında yaşadığından, sınıflardan çok daha hızlı erişilebilirler. Bununla birlikte, yığın yığından çok daha az alana sahiptir, bu nedenle yapılar küçük tutulmalıdır (Microsoft, `struct`ların 16 bayttan fazla almamasını önerir).

Bir "sınıf", C#'ta en çok kullanılan türdür (bu üçünden) ve genellikle ilk olarak kullanmanız gereken şeydir.

Yalnızca bir kez (derleme zamanında) tanımlanması gereken açıkça tanımlanmış, farklı bir öğe listesine sahip olduğunuzda bir "enum" kullanılır. Numaralandırmalar, bazı değerlere hafif bir referans olarak programcılara yardımcı olur: karşılaştırılacak bir "sabit" değişkenler listesi tanımlamak yerine, bir numaralandırma kullanabilir ve yanlışlıkla yanlış bir değer kullanmadığınızdan emin olmak için Intellisense desteği alabilirsiniz.

## Yapı Tanımı
Yapılar System.ValueType öğesinden devralır, değer türleridir ve yığında yaşar. Değer türleri parametre olarak iletildiğinde, değere göre iletilir.
-------------------------------------------------- ----------------------

    Struct MyStruct
    {
        public int x;
        public int y;
    }

**Değere göre geçti**, parametre değerinin yöntem için *kopyalandığı* ve yöntemde parametrede yapılan herhangi bir değişikliğin yöntemin dışına yansıtılmadığı anlamına gelir. Örneğin, bir Değer türü olan 'int' türündeki 'a' ve 'b' değişkenlerini ileterek, 'AddNumbers' adlı bir yöntemi çağıran aşağıdaki kodu göz önünde bulundurun.

    int a = 5;
    int b = 6;
    
    AddNumbers(a,b);

    public AddNumbers(int x, int y)
    {
        int z = x + y; // z becomes 11
        x = x + 5; // now we changed x to be 10
        z = x + y; // now z becomes 16
    } 

Yöntemin içinde "x"e 5 eklesek de, "a"nın değeri değişmeden kalır, çünkü bu bir Değer türüdür ve bu, "x"in "a"nın değerinin *kopyası olduğu, ancak gerçekte olmadığı anlamına gelir. 'a'.

Unutmayın, Değer türleri yığında yaşar ve değere göre iletilir.

    

## Sınıf Tanımı
Sınıflar System.Object öğesinden devralır, başvuru türleridir ve öbek üzerinde yaşar. Referans türleri parametre olarak iletildiğinde, referans olarak iletilirler.
-------------------------------------------------- ----------------------


    public Class MyClass
    {
        public int a;
        public int b;
    }

**Başvuruyla geçti**, parametreye bir *başvurunun* yönteme iletildiği ve parametrede yapılan herhangi bir değişikliğin, geri döndüğünde yöntemin dışına yansıtılacağı anlamına gelir, çünkü başvuru, hafıza*. Şimdi öncekiyle aynı örneği kullanalım, ancak önce int'leri bir sınıfa "saracağız".

    MyClass instanceOfMyClass = new MyClass();
    instanceOfMyClass.a = 5;
    instanceOfMyClass.b = 6;
    
    AddNumbers(instanceOfMyClass);
    
    public AddNumbers(MyClass sample)
    {
        int z = sample.a + sample.b; // z becomes 11
        sample.a = sample.a + 5; // now we changed a to be 10
        z = sample.a + sample.b; // now z becomes 16
    } 

Bu sefer, 'sample.a'yı '10' olarak değiştirdiğimizde, 'instanceOfMyClass.a'nın değeri *ayrıca* değişir, çünkü *referans tarafından geçirilmiştir*. Başvuru yoluyla iletilmesi, nesnenin kendisinin bir kopyası yerine, nesneye bir *başvurunun* (bazen *işaretçi* olarak da adlandırılır) yönteme geçirildiği anlamına gelir.

Referans türlerinin öbek üzerinde yaşadığını ve referansa göre iletildiğini unutmayın.

## Numaralandırma Tanımı
Enum, özel bir sınıf türüdür. "enum" anahtar sözcüğü, derleyiciye bu sınıfın soyut System.Enum sınıfından miras aldığını söyler. Numaralandırmalar, farklı öğe listeleri için kullanılır.
-------------------------------------------------- ----------------------

    
    public enum MyEnum
    {
        Monday = 1,
        Tuesday,
        Wednesday,
        //...
    }

Bir numaralandırmayı, sabitleri bazı temel değerlere eşlemenin uygun bir yolu olarak düşünebilirsiniz. Yukarıda tanımlanan numaralandırma haftanın her günü için değerler bildirir ve '1' ile başlar. "Salı" daha sonra otomatik olarak "2", "Çarşamba" ile "3" vb.

Varsayılan olarak, numaralandırmalar temel tür olarak 'int' kullanır ve 0'dan başlar, ancak aşağıdaki *integral türlerinden*: 'byte, sbyte, short, ushort, int, uint, long veya ulong' herhangi birini kullanabilirsiniz ve herhangi bir öğe için açık değerler belirtebilir. Bazı öğeler açıkça belirtilmiş, ancak bazıları belirtilmemişse, son tanımlanan öğeden sonraki her öğe 1 artırılacaktır.

Bu örneği *MyEnum*'a başka bir değer *dökerek* kullanırdık:

    MyEnum instance = (MyEnum)3; // the variable named 'instance' gets a 
                                 //value of MyEnum.Wednesday, which maps to 3.

    int x = 2;
    instance = (MyEnum)x; // now 'instance' has a value of MyEnum.Tuesday

Daha karmaşık olmasına rağmen başka bir yararlı numaralandırma tipine "Flags" denir. 'Flags' özniteliğiyle bir numaralandırmayı *dekorasyon yaparak*, aynı anda birden fazla değere bir değişken atayabilirsiniz. Bunu yaparken, temel 2 gösterimde değerleri açıkça tanımlamanız *gerektiğini* unutmayın.

    [Flags]
    public enum MyEnum
    {
        Monday = 1,
        Tuesday = 2,
        Wednesday = 4,
        Thursday = 8,
        Friday = 16,
        Saturday = 32, 
        Sunday = 64
    }

Artık *bitsel karşılaştırmaları* kullanarak veya .NET 4.0 veya sonraki bir sürümünü kullanıyorsanız yerleşik `Enum.HasFlag` yöntemini kullanarak aynı anda birden fazla değeri karşılaştırabilirsiniz.

    MyEnum instance = MyEnum.Monday | MyEnum.Thursday; // instance now has a value of
                                                       // *both* Monday and Thursday,
                                                       // represented by (in binary) 0100. 

    if (instance.HasFlag(MyEnum.Wednesday))
    {
        // it doesn't, so this block is skipped
    }
    else if (instance.HasFlag(MyEnum.Thursday))
    {
        // it does, so this block is executed
    }



Enum sınıfı "System.ValueType" sınıfından alt sınıflandığından, bir değer türü olarak kabul edilir ve referansa göre değil değere göre iletilir. Temel nesne öbek üzerinde oluşturulur, ancak bir işlev çağrısına bir enum değeri ilettiğinizde, Enum'un temel alınan değer türünü (tipik olarak System.Int32) kullanan değerin bir kopyası yığına itilir. Derleyici, bu değer ile yığında oluşturulan temel nesne arasındaki ilişkiyi izler. Daha fazla bilgi için [ValueType Sınıfı (Sistem) (MSDN)][1]'e bakın.


[1]: https://msdn.microsoft.com/en-us/library/system.valuetype(v=vs.110).aspx

