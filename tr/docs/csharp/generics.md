---
title: "jenerik"
slug: "jenerik"
draft: false
images: []
weight: 9216
type: docs
toc: true
---

## Sözdizimi
- `public void SomeMethod <T> () { }`
- `public void SomeMethod<T, V>() { }`
- `public T SomeMethod<T>(IEnumerable<T> dizisi) { ... }`
- `public void SomeMethod<T>() burada T : new() { }`
- `public void SomeMethod<T, V>() burada T : new() burada V : struct { }`
- `public void SomeMethod<T>() burada T: IDisposable { }`
- `public void SomeMethod<T>() burada T: Foo { }`
- `public class Sınıfım<T> { public T Data {get; Ayarlamak; } }`

## Parametreler
| Parametre(ler) | Açıklama |
|---|---|
| T, V | Genel bildirimler için yer tutucuları yazın |



C#'daki jenerikler, çalışma zamanına kadar desteklenir: C# ile oluşturulan jenerik türler, [CIL][1]'e derlendikten sonra bile jenerik semantiklerini korur.


Bu, C#'ta, genel türler üzerinde düşünmenin ve bunları bildirildikleri gibi görmenin veya örneğin bir nesnenin genel bir türün örneği olup olmadığını kontrol etmenin etkili bir şekilde mümkün olduğu anlamına gelir. Bu, derleme sırasında genel tür bilgilerinin kaldırıldığı [tür silme][2] ile çelişir. Aynı zamanda, birden çok somut genel türün çalışma zamanında birden çok genel olmayan tür haline geldiği ve orijinal genel tür tanımlarını daha da somutlaştırmak için gereken herhangi bir üst verinin kaybolduğu jeneriklere yönelik şablon yaklaşımıyla da çelişir.

Bununla birlikte, genel türler üzerinde düşünürken dikkatli olun: genel türlerin adları derleme sırasında değiştirilir, açılı ayraçlar ve tür parametrelerinin adları bir ters tik ile ve ardından genel tür parametrelerinin sayısı ile değiştirilir. Böylece bir `Dictionary<TKey, Tvalue>` ``Dictionary`2``ye çevrilecektir.


[1]: https://en.wikipedia.org/wiki/Common_Intermediate_Language
[2]: https://en.wikipedia.org/wiki/Type_erasure

## Örtülü tür çıkarımı (yöntemler)
Genel bir yönteme biçimsel bağımsız değişkenler iletirken, ilgili genel tür bağımsız değişkenleri genellikle dolaylı olarak çıkarılabilir. Tüm genel türler çıkarılabiliyorsa, bunları sözdiziminde belirtmek isteğe bağlıdır.

Aşağıdaki genel yöntemi göz önünde bulundurun. Bir resmi parametresi ve bir genel tür parametresi vardır. Aralarında çok açık bir ilişki vardır -- genel tür parametresine argüman olarak iletilen tür, biçimsel parametreye iletilen argümanın derleme zamanı türüyle aynı olmalıdır.

    void M<T>(T obj)
    {
    }

Bu iki çağrı eşdeğerdir:

    M<object>(new object());
    M(new object());

Bu iki çağrı da eşdeğerdir:

    M<string>("");
    M("");

Ve bu üç çağrı da öyle:

    M<object>("");
    M((object) "");
    M("" as object);

---

En az bir tür argümanı çıkarılamıyorsa, hepsinin belirtilmesi gerektiğine dikkat edin.

Aşağıdaki genel yöntemi göz önünde bulundurun. İlk genel tür argümanı, biçimsel argümanın tipiyle aynıdır. Ancak ikinci genel tür argümanı için böyle bir ilişki yoktur. Bu nedenle, derleyicinin bu yönteme yapılan herhangi bir çağrıda ikinci genel tür bağımsız değişkenini çıkarsama yolu yoktur.

    void X<T1, T2>(T1 obj)
    {
    }

Bu artık çalışmıyor:

    X("");

Bu da çalışmaz, çünkü derleyici birinci veya ikinci genel parametreyi mi belirttiğimizden emin değildir (her ikisi de "nesne" olarak geçerli olur):

    X<object>("");

Her ikisini de şu şekilde yazmamız gerekiyor:

    X<string, object>("");

## Tür çıkarımı (sınıflar)
Geliştiriciler, yapıcılar için tür çıkarımının *çalışmadığı gerçeğiyle yakalanabilir:

    class Tuple<T1,T2>
    {
       public Tuple(T1 value1, T2 value2)
       {
       }
    }

    var x = new Tuple(2, "two");              // This WON'T work...
    var y = new Tuple<int, string>(2, "two"); // even though the explicit form will.

Açıkça tür parametreleri belirtmeden örnek oluşturmanın ilk yolu, derleme zamanı hatasına neden olur ve şöyle der:
>'Tuple<T1, T2>' genel türünü kullanmak için 2 tür bağımsız değişken gerekir

Yaygın bir geçici çözüm, statik bir sınıfa bir yardımcı yöntem eklemektir:

    static class Tuple
    {
        public static Tuple<T1, T2> Create<T1, T2>(T1 value1, T2 value2)
        {
             return new Tuple<T1, T2>(value1, value2);
        }
    }

    var x = Tuple.Create(2, "two");  // This WILL work...

## Kısıtlama türü olarak bir arabirimle genel yöntemi kullanma.
Bu, Animal sınıfındaki Eat<TFood> yönteminde TFood genel türünün nasıl kullanılacağına dair bir örnektir.

    public interface IFood
    {
        void EatenBy(Animal animal);
    }
    
    public class Grass: IFood
    {
        public void EatenBy(Animal animal)
        {
            Console.WriteLine("Grass was eaten by: {0}", animal.Name);
        }
    }
    
    public class Animal
    {
        public string Name { get; set; }
    
        public void Eat<TFood>(TFood food)
            where TFood : IFood
        {
            food.EatenBy(this);
        }
    }
    
    public class Carnivore : Animal
    {
        public Carnivore()
        {
            Name = "Carnivore";
        }
    }
    
    public class Herbivore : Animal, IFood
    {
        public Herbivore()
        {
            Name = "Herbivore";
        }
        
        public void EatenBy(Animal animal)
        {
            Console.WriteLine("Herbivore was eaten by: {0}", animal.Name);
        }
    }

Eat<TFood> yöntemini şu şekilde çağırabilirsiniz:

    var grass = new Grass();        
    var sheep = new Herbivore();
    var lion = new Carnivore();
        
    sheep.Eat(grass);
    //Output: Grass was eaten by: Herbivore

    lion.Eat(sheep);
    //Output: Herbivore was eaten by: Carnivore

Bu durumda aramayı denerseniz:
   
    sheep.Eat(lion);

Aslan nesnesi IFood arabirimini uygulamadığı için bu mümkün olmayacaktır. Yukarıdaki çağrıyı yapmaya çalışmak bir derleyici hatası üretecektir: "'Etobur' türü, 'Animal.Eat<TFood>(TFood)' genel türünde veya yönteminde 'TFood' tür parametresi olarak kullanılamaz. Örtülü bir başvuru yok 'Etobur'dan 'IFood'a dönüşüm."

## Tür kısıtlamaları (yeni anahtar kelime)
'new()' kısıtlamasını kullanarak, boş (varsayılan) bir kurucu tanımlamak için tür parametrelerini zorlamak mümkündür.

    class Foo
    {
        public Foo () { }
    }

    class Bar
    {
        public Bar (string s) { ... }
    }

    class Factory<T>
        where T : new()
    {
        public T Create()
        {
            return new T();
        }
    }

    Foo f = new Factory<Foo>().Create(); // Valid.
    Bar b = new Factory<Bar>().Create(); // Invalid, Bar does not define a default/empty constructor.

'Create()' için yapılan ikinci çağrı, aşağıdaki mesajla birlikte derleme zamanı hatası verecektir:
>'Bar', genel türde veya 'Factory<T>' yönteminde 'T' parametresi olarak kullanmak için, genel parametresiz kurucuya sahip soyut olmayan bir tür olmalıdır.

Parametreli bir kurucu için herhangi bir kısıtlama yoktur, sadece parametresiz kurucular desteklenir.

## Tür kısıtlamaları (sınıflar ve arayüzler)
Tür kısıtlamaları, bir tür parametresini belirli bir arabirimi veya sınıfı uygulamaya zorlayabilir.

    interface IType;
    interface IAnotherType;

    // T must be a subtype of IType
    interface IGeneric<T>
        where T : IType
    {
    }

    // T must be a subtype of IType
    class Generic<T>
        where T : IType
    {
    }

    class NonGeneric
    {
        // T must be a subtype of IType
        public void DoSomething<T>(T arg)
            where T : IType
        {
        }
    }

    // Valid definitions and expressions:
    class Type : IType { }
    class Sub : IGeneric<Type> { }
    class Sub : Generic<Type> { }
    new NonGeneric().DoSomething(new Type());

    // Invalid definitions and expressions:
    class AnotherType : IAnotherType { }
    class Sub : IGeneric<AnotherType> { }
    class Sub : Generic<AnotherType> { }
    new NonGeneric().DoSomething(new AnotherType());

Birden çok kısıtlama için sözdizimi:

    class Generic<T, T1>
        where T : IType 
        where T1 : Base, new()
    {
    }

Tür kısıtlamaları, kalıtımla aynı şekilde çalışır, çünkü genel türde kısıtlamalar olarak birden çok arabirim belirtmek mümkündür, ancak yalnızca bir sınıf:

    class A { /* ... */ }
    class B { /* ... */ }

    interface I1 { }
    interface I2 { }

    class Generic<T>
        where T : A, I1, I2
    {
    }

    class Generic2<T>
        where T : A, B //Compilation error
    {
    }

Diğer bir kural, sınıfın ilk kısıtlama olarak eklenmesi ve ardından arayüzlerin eklenmesi gerektiğidir:

    class Generic<T>
        where T : A, I1
    {
    }

    class Generic2<T>
        where T : I1, A //Compilation error
    {
    }

Belirli bir genel somutlaştırmanın çalışması için tüm beyan edilen kısıtlamaların aynı anda karşılanması gerekir. İki veya daha fazla alternatif kısıtlama seti belirtmenin bir yolu yoktur.

## Tür parametrelerini yansıtma
'typeof' operatörü, tür parametreleri üzerinde çalışır.

    class NameGetter<T>
    {
        public string GetTypeName()
        {
            return typeof(T).Name;
        }
    }

## Kovaryans
Bir "IEnumerable<T>" ne zaman farklı bir "IEnumerable<T1>" alt türü olur? "T", "T1"in bir alt türü olduğunda. "IEnumerable", "T" parametresinde _covariant_'tır; bu, "IEnumerable"ın alt tür ilişkisinin "T"lerle _aynı yönde_ gittiği anlamına gelir.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }

    IEnumerable<Dog> dogs = Enumerable.Empty<Dog>();
    IEnumerable<Animal> animals = dogs;  // IEnumerable<Dog> is a subtype of IEnumerable<Animal>
    // dogs = animals;  // Compilation error - IEnumerable<Animal> is not a subtype of IEnumerable<Dog>

Belirli bir tür parametresine sahip bir kovaryant genel türün bir örneği, daha az türetilmiş bir tür parametresiyle aynı genel türe örtük olarak dönüştürülebilir.

Bu ilişki, 'IEnumerable' _ 'T'leri ürettiği, ancak bunları tüketmediği için geçerlidir. 'Köpek' üreten bir nesne, 'Hayvan' üretiyormuş gibi kullanılabilir.

Parametrenin yalnızca bir _output_ olarak kullanılması gerektiğinden, kovaryant türü parametreler "out" anahtar sözcüğü kullanılarak bildirilir.

    interface IEnumerable<out T> { /* ... */ }

Kovaryant olarak bildirilen bir tür parametresi girdi olarak görünmeyebilir.

    interface Bad<out T>
    {
        void SetT(T t);  // type error
    }

İşte tam bir örnek:

    using NUnit.Framework;
    
    namespace ToyStore
    {
       enum Taste { Bitter, Sweet };
    
       interface IWidget
       {
          int Weight { get; }
       }
    
       interface IFactory<out TWidget>
           where TWidget : IWidget
       {
          TWidget Create();
       }
    
       class Toy : IWidget
       {
          public int Weight { get; set; }
          public Taste Taste { get; set; }
       }
    
       class ToyFactory : IFactory<Toy>
       {
          public const int StandardWeight = 100;
          public const Taste StandardTaste = Taste.Sweet;

          public Toy Create() { return new Toy { Weight = StandardWeight, Taste = StandardTaste }; }
       }
    
       [TestFixture]
       public class GivenAToyFactory
       {
          [Test]
          public static void WhenUsingToyFactoryToMakeWidgets()
          {
             var toyFactory = new ToyFactory();
    
             //// Without out keyword, note the verbose explicit cast:
             // IFactory<IWidget> rustBeltFactory = (IFactory<IWidget>)toyFactory;
    
             // covariance: concrete being assigned to abstract (shiny and new)
             IFactory<IWidget> widgetFactory = toyFactory;
             IWidget anotherToy = widgetFactory.Create();
             Assert.That(anotherToy.Weight, Is.EqualTo(ToyFactory.StandardWeight)); // abstract contract
             Assert.That(((Toy)anotherToy).Taste, Is.EqualTo(ToyFactory.StandardTaste)); // concrete contract
          }
       }
    }
    

## çelişki
Bir "IComparer<T>" ne zaman farklı bir "IComparer<T1>" alt türü olur? "T1", "T"nin bir alt türü olduğunda. 'IComparer', 'T' parametresinde _contravariant_'tır; bu, 'IComparer'ın alt türü ilişkisinin 'T'ler olarak _ters yönde_ gittiği anlamına gelir.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }

    IComparer<Animal> animalComparer = /* ... */;
    IComparer<Dog> dogComparer = animalComparer;  // IComparer<Animal> is a subtype of IComparer<Dog>
    // animalComparer = dogComparer;  // Compilation error - IComparer<Dog> is not a subtype of IComparer<Animal>

Belirli bir tür parametresine sahip bir karşı değişken genel türün bir örneği, daha türetilmiş bir tür parametresiyle aynı genel türe örtük olarak dönüştürülebilir.

Bu ilişki, 'IComparer' 'T'leri tükettiği, ancak bunları üretmediği için geçerlidir. Herhangi iki "Hayvan"ı karşılaştırabilen bir nesne, iki "Köpek"i karşılaştırmak için kullanılabilir.

Parametrenin yalnızca bir _input_ olarak kullanılması gerektiğinden, çelişki türü parametreler 'in' anahtar sözcüğü kullanılarak bildirilir.

    interface IComparer<in T> { /* ... */ }

Contravariant olarak bildirilen bir tür parametresi çıktı olarak görünmeyebilir.

    interface Bad<in T>
    {
        T GetT();  // type error
    }

## Değişmezlik
"IList<T>" hiçbir zaman farklı bir "IList<T1>"in alt türü değildir. "IList", tür parametresinde _invariant_ şeklindedir.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }
    
    IList<Dog> dogs = new List<Dog>();
    IList<Animal> animals = dogs;  // type error

Listeler için alt tür ilişkisi yoktur, çünkü değerleri bir listeye koyabilirsiniz _ve_ listeden değerleri alabilirsiniz.

"IList" kovaryant olsaydı, belirli bir listeye _yanlış alt türün öğelerini ekleyebilirsiniz.

    IList<Animal> animals = new List<Dog>();  // supposing this were allowed...
    animals.Add(new Giraffe());  // ... then this would also be allowed, which is bad!

"IList" çelişkili olsaydı, verilen bir listeden yanlış alt türün değerlerini çıkarabilirdiniz.

    IList<Dog> dogs = new List<Animal> { new Dog(), new Giraffe() };  // if this were allowed...
    Dog dog = dogs[1];  // ... then this would be allowed, which is bad!

Değişmez tür parametreleri, hem "in" hem de "out" anahtar sözcükleri çıkarılarak bildirilir.

    interface IList<T> { /* ... */ }

## Varyant arayüzleri
Arayüzler, değişken tipi parametrelere sahip olabilir.

    interface IEnumerable<out T>
    {
        // ...
    }
    interface IComparer<in T>
    {
        // ...
    }

ancak sınıflar ve yapılar olmayabilir

    class BadClass<in T1, out T2>  // not allowed
    {
    }
    
    struct BadStruct<in T1, out T2>  // not allowed
    {
    }

ne de genel yöntem bildirimleri

    class MyClass
    {
        public T Bad<out T, in T1>(T1 t1)  // not allowed
        {
            // ...
        }
    }

Aşağıdaki örnek, aynı arabirimde birden çok varyans bildirimini göstermektedir.

    interface IFoo<in T1, out T2, T3>
    //  T1 : Contravariant type
    //  T2 : Covariant type 
    //  T3 : Invariant type
    {
        // ...
    }
    
    IFoo<Animal, Dog, int> foo1 = /* ... */;
    IFoo<Dog, Animal, int> foo2 = foo1;  
    // IFoo<Animal, Dog, int> is a subtype of IFoo<Dog, Animal, int>



## Genel değerlerin eşitliği kontrol ediliyor.
Genel sınıf veya yöntemin mantığı, genel türe sahip değerlerin eşitliğinin kontrol edilmesini gerektiriyorsa, `EqualityComparer<TType>.Default` [özellik][1] öğesini kullanın:


    public void Foo<TBar>(TBar arg1, TBar arg2)
    {
        var comparer = EqualityComparer<TBar>.Default;
        if (comparer.Equals(arg1,arg2)
        {
            ...
        }
    }

Bu yaklaşım, yalnızca 'Object.Equals()' yöntemini çağırmaktan daha iyidir, çünkü varsayılan karşılaştırıcı uygulaması, 'TBar' türünün 'IEquatale<TBar>' [interface][2] öğesini uygulayıp uygulamadığını kontrol eder ve evetse, 'IEquatable<TBar> öğesini çağırır. .Equals(TBar diğer)` yöntemi. Bu, değer türlerinin kutulanmasını/kutunun açılmasını önlemeye izin verir.


[1]: https://msdn.microsoft.com/en-us/library/ms224763(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx

## Tür Parametreleri (Arayüzler)
Beyanname:

    interface IMyGenericInterface<T1, T2, T3, ...> { ... }

Kullanım (kalıtımda):

    class ClassA<T1, T2, T3> : IMyGenericInterface<T1, T2, T3> { ... }

    class ClassB<T1, T2> : IMyGenericInterface<T1, T2, int> { ... }

    class ClassC<T1> : IMyGenericInterface<T1, char, int> { ... }

    class ClassD : IMyGenericInterface<bool, char, int> { ... }

Kullanım (bir parametrenin türü olarak):

    void SomeMethod(IMyGenericInterface<int, char, bool> arg) { ... }

## Varyant delegeleri
Temsilciler, değişken tipi parametrelere sahip olabilir.

    delegate void Action<in T>(T t);    // T is an input
    delegate T Func<out T>();           // T is an output
    delegate T2 Func<in T1, out T2>();  // T1 is an input, T2 is an output

Bu, (diğer şeylerin yanı sıra) aşağıdaki durumlarda bir D yönteminin bir B yönteminden daha fazla türetilmiş sayılabileceğini belirten [Liskov Değiştirme İlkesi][1]'nden kaynaklanmaktadır:

- D, B'ye eşit veya daha fazla türetilmiş bir dönüş türüne sahip
- D, B'ye eşit veya daha genel karşılık gelen parametre türlerine sahiptir

Bu nedenle, aşağıdaki atamaların tümü güvenlidir:

    Func<object, string> original = SomeMethod;
    Func<object, object> d1 = original;
    Func<string, string> d2 = original;
    Func<string, object> d3 = original;

[1]: https://en.wikipedia.org/wiki/Liskov_substitution_principle

## Değişken türleri parametre ve dönüş değerleri olarak
Bir kovaryant türü çıktı olarak görünüyorsa, içeren tür kovaryanttır. Bir "T" üreticisi üretmek, "T" üretmek gibidir.

    interface IReturnCovariant<out T>
    {
        IEnumerable<T> GetTs();
    }

Bir karşı değişken türü çıktı olarak görünüyorsa, içeren tür karşı değişkendir. Bir 'T' tüketicisi üretmek, 'T'leri tüketmek gibidir.

    interface IReturnContravariant<in T>
    {
        IComparer<T> GetTComparer();
    }

Bir kovaryant türü girdi olarak görünüyorsa, içeren tür ters değişkendir. Bir 'T' üreticisini tüketmek, 'T'leri tüketmeye benzer.

    interface IAcceptCovariant<in T>
    {
        void ProcessTs(IEnumerable<T> ts);
    }

Bir karşı değişken türü girdi olarak görünüyorsa, içeren tür kovaryanttır. Bir "T" tüketicisini tüketmek, "T" üretmek gibidir.

    interface IAcceptContravariant<out T>
    {
        void CompareTs(IComparer<T> tComparer);
    }

## Tip Parametreleri (Sınıflar)
Beyanname:

    class MyGenericClass<T1, T2, T3, ...>
    {
        // Do something with the type parameters.
    }

Başlatma:

    var x = new MyGenericClass<int, char, bool>();

Kullanım (bir parametrenin türü olarak):

    void AnotherMethod(MyGenericClass<float, byte, char> arg) { ... }

## Tip Parametreleri (Yöntemler)
Beyanname:

    void MyGenericMethod<T1, T2, T3>(T1 a, T2 b, T3 c)
    {
        // Do something with the type parameters.
    }

Çağrı:

Bir genel yönteme tür argümanları sağlamaya gerek yoktur, çünkü derleyici örtük olarak türü çıkarsayabilir.
    
    int x =10;
    int y =20;
    string z = "test";
    MyGenericMethod(x,y,z);

Ancak, bir belirsizlik varsa, tür argümanları ile genel yöntemlerin şu şekilde çağrılması gerekir:

    MyGenericMethod<int, int, string>(x,y,z);



## Tür kısıtlamaları (sınıf ve yapı)
Tür argümanının bir referans tipi mi yoksa bir değer tipi mi olacağını ilgili kısıtlamalar "sınıf" veya "yapı" kullanarak belirtmek mümkündür. Bu kısıtlamalar kullanılırsa, bunlar *tanımlanmalı* _her şeyden önce_ diğer kısıtlamalar (örneğin bir ebeveyn türü veya 'yeni()') listelenebilir.

    // TRef must be a reference type, the use of Int32, Single, etc. is invalid.
    // Interfaces are valid, as they are reference types
    class AcceptsRefType<TRef>
        where TRef : class
    {
        // TStruct must be a value type.
        public void AcceptStruct<TStruct>()
            where TStruct : struct
        {
        }

        // If multiple constraints are used along with class/struct
        // then the class or struct constraint MUST be specified first
        public void Foo<TComparableClass>()
            where TComparableClass : class, IComparable
        {
        }
    }

## Açık tür parametreleri
Genel bir yöntem için tür parametrelerini Açıkça belirtmeniz gereken farklı durumlar vardır. Aşağıdaki her iki durumda da derleyici, belirtilen yöntem parametrelerinden tür parametrelerinin tümünü çıkaramaz.

Bir durum, parametre olmadığı zamandır:

    public void SomeMethod<T, V>() 
    {
       // No code for simplicity
    }

    SomeMethod(); // doesn't compile
    SomeMethod<int, bool>(); // compiles

İkinci durum, tür parametrelerinden birinin (veya daha fazlasının) yöntem parametrelerinin parçası olmadığı durumdur:

    public K SomeMethod<K, V>(V input)
    {
        return default(K);
    }

    int num1 = SomeMethod(3); // doesn't compile
    int num2 = SomeMethod<int>("3"); // doesn't compile
    int num3 = SomeMethod<int, string>("3"); // compiles.

## Genel tip döküm
        /// <summary>
        /// Converts a data type to another data type.
        /// </summary>
        public static class Cast
        {
            /// <summary>
            /// Converts input to Type of default value or given as typeparam T
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="input">Input that need to be converted to specified type</param>
            /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
            /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
            public static T To<T>(object input, T defaultValue)
            {
                var result = defaultValue;
                try
                {
                    if (input == null || input == DBNull.Value) return result;
                    if (typeof (T).IsEnum)
                    {
                        result = (T) Enum.ToObject(typeof (T), To(input, Convert.ToInt32(defaultValue)));
                    }
                    else
                    {
                        result = (T) Convert.ChangeType(input, typeof (T));
                    }
                }
                catch (Exception ex)
                {
                    Tracer.Current.LogException(ex);
                }
    
                return result;
            }
            
            /// <summary>
            /// Converts input to Type of typeparam T
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="input">Input that need to be converted to specified type</param>
            /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
            public static T To<T>(object input)
            {
                return To(input, default(T));
            }
    
            
    
        }

Kullanım Alanları:

    std.Name = Cast.To<string>(drConnection["Name"]);
    std.Age = Cast.To<int>(drConnection["Age"]);
    std.IsPassed = Cast.To<bool>(drConnection["IsPassed"]);

    
    // Casting type using default value
    //Following both ways are correct
    // Way 1 (In following style input is converted into type of default value)
    std.Name = Cast.To(drConnection["Name"], "");
    std.Marks = Cast.To(drConnection["Marks"], 0);
    // Way 2    
    std.Name = Cast.To<string>(drConnection["Name"], "");
    std.Marks = Cast.To<int>(drConnection["Marks"], 0);

## Genel tip dökümlü konfigürasyon okuyucu
        /// <summary>
        /// Read configuration values from app.config and convert to specified types
        /// </summary>
        public static class ConfigurationReader
        {
            /// <summary>
            /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="strKey">key to find value from AppSettings</param>
            /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
            /// <returns>AppSettings value against key is returned in Type of default value or given as typeparam T</returns>
            public static T GetConfigKeyValue<T>(string strKey, T defaultValue)
            {
                var result = defaultValue;
                try
                {
                    if (ConfigurationManager.AppSettings[strKey] != null)
                        result = (T)Convert.ChangeType(ConfigurationManager.AppSettings[strKey], typeof(T));
                }
                catch (Exception ex)
                {
                    Tracer.Current.LogException(ex);
                }
    
                return result;
            }
            /// <summary>
            /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="strKey">key to find value from AppSettings</param>
            /// <returns>AppSettings value against key is returned in Type given as typeparam T</returns>
            public static T GetConfigKeyValue<T>(string strKey)
            {
                return GetConfigKeyValue(strKey, default(T));
            }
    
        }

Kullanım Alanları:

    var timeOut = ConfigurationReader.GetConfigKeyValue("RequestTimeout", 2000);
    var url = ConfigurationReader.GetConfigKeyValue("URL", "www.someurl.com");
    var enabled = ConfigurationReader.GetConfigKeyValue("IsEnabled", false);

