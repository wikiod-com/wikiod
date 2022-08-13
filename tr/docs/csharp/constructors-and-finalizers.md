---
title: "Yapıcılar ve Sonlandırıcılar"
slug: "yapclar-ve-sonlandrclar"
draft: false
images: []
weight: 9573
type: docs
toc: true
---

Yapıcılar, bir sınıftaki, o sınıfın bir örneği oluşturulduğunda çağrılan yöntemlerdir. Ana sorumlulukları, yeni nesneyi kullanışlı ve tutarlı bir durumda bırakmaktır.

Yıkıcılar/Sonlandırıcılar, bir örneği yok edildiğinde çağrılan bir sınıftaki yöntemlerdir. C#'da nadiren açıkça yazılır/kullanılırlar.

C# aslında yıkıcılara sahip değildir, bunun yerine C++ stili yıkıcı sözdizimini kullanan Sonlandırıcılara sahiptir. Bir yıkıcı belirtmek, doğrudan çağrılamayan 'Object.Finalize()' yöntemini geçersiz kılar.

Benzer sözdizimine sahip diğer dillerin aksine, bu yöntemler nesneler kapsam dışına çıktığında *değmez*, ancak [belirli koşullar altında] meydana gelen Çöp Toplayıcı çalıştığında çağrılır[3]. Bu nedenle, belirli bir sırada çalışacakları * garanti edilmez*.

Sonlandırıcılar, **yalnızca** yönetilmeyen kaynakları temizlemekten sorumlu olmalıdır (Marshal sınıfı aracılığıyla edinilen, p/Invoke (sistem çağrıları) aracılığıyla alınan işaretçiler veya güvenli olmayan bloklarda kullanılan ham işaretçiler). Yönetilen kaynakları temizlemek için lütfen IDisposable'ı, Dispose modelini ve [`using`][1] ifadesini inceleyin.

(Daha fazla okuma: [Ne zaman bir yıkıcı oluşturmalıyım?][2])


[1]: https://www.wikiod.com/tr/docs/c%23/38/using-statement
[2]: http://stackoverflow.com/a/4899622
[3]: https://msdn.microsoft.com/en-us/library/ee787088(v=vs.110).aspx#conditions_for_a_garbage_collection

## Statik oluşturucu
Bir türün herhangi bir üyesi ilk kez başlatıldığında, statik bir sınıf üyesi veya statik bir yöntem çağrıldığında statik bir kurucu çağrılır.
Statik kurucu iş parçacığı güvenlidir.
Statik bir kurucu genellikle aşağıdakiler için kullanılır:
- Statik durumu başlat, yani aynı sınıfın farklı örnekleri arasında paylaşılan durum.
- Bir singleton oluşturun

**Örnek:**

    class Animal
    {
        // * A static constructor is executed only once,
        //   when a class is first accessed.
        // * A static constructor cannot have any access modifiers
        // * A static constructor cannot have any parameters
        static Animal()
        {
            Console.WriteLine("Animal initialized");
        }

        // Instance constructor, this is executed every time the class is created
        public Animal()
        {
            Console.WriteLine("Animal created");
        }

        public static void Yawn()
        {
            Console.WriteLine("Yawn!");
        }
    }

    var turtle = new Animal();
    var giraffe = new Animal();
**Çıktı:**
 
> Hayvan başlatıldı
> Hayvan yaratıldı
> Hayvan yaratıldı

[Demoyu Görüntüle][1]

İlk çağrı statik bir yönteme yapılmışsa, statik oluşturucu, örnek oluşturucu olmadan çağrılır. Bu sorun değil, çünkü statik yöntem hiçbir şekilde örnek durumuna erişemez.

    Animal.Yawn();

Bu çıktı:

> Hayvan başlatıldı
> Esne!

Ayrıca [Statik oluşturuculardaki istisnalar[2] ve [Genel Statik Oluşturucular][3]'e bakın.


[1]: https://dotnetfiddle.net/XmExII
[2]: https://www.wikiod.com/tr/docs/c%23/25/constructors-finalizers/15007/exceptions-in-static-constructors
[3]: https://www.wikiod.com/tr/docs/c%23/25/constructors-finalizers/15003/generic-static-constructors

Tekton örneği:

    public class SessionManager
    {
        public static SessionManager Instance;

        static SessionManager()
        {
            Instance = new SessionManager();
        }
    }

## Singleton yapıcı deseni
    public class SingletonClass
    {
        public static SingletonClass Instance { get; } = new SingletonClass();

        private SingletonClass()
        {
            // Put custom constructor code here
        }    
    }

Yapıcı özel olduğundan, kod tüketilerek yeni "SingletonClass" örneği oluşturulamaz. "SingletonClass"ın tek örneğine erişmenin tek yolu, "SingletonClass.Instance" statik özelliğini kullanmaktır.

"Örnek" özelliği, C# derleyicisinin oluşturduğu statik bir kurucu tarafından atanır. .NET çalışma zamanı, statik oluşturucunun en fazla bir kez çalıştırılmasını ve 'Örnek' ilk okunmadan önce çalıştırılmasını garanti eder. Bu nedenle, tüm senkronizasyon ve başlatma sorunları çalışma zamanı tarafından gerçekleştirilir.

Statik kurucu başarısız olursa, "Singleton" sınıfının AppDomain'in ömrü boyunca kalıcı olarak kullanılamaz hale geleceğini unutmayın.

Ayrıca, statik oluşturucunun "Örnek"e ilk erişim sırasında çalışması garanti edilmez. Bunun yerine, *bundan önce bir noktada* çalışacaktır. Bu, başlatmanın gerçekleştiği zamanı deterministik olmayan hale getirir. Pratik durumlarda JIT, 'Örnek'e atıfta bulunan bir yöntemin *derleme* (yürütme değil) sırasında statik kurucuyu çağırır. Bu bir performans optimizasyonudur.

Singleton modelini uygulamanın diğer yolları için [Singleton Uygulamaları][1] sayfasına bakın.


[1]: https://www.wikiod.com/tr/docs/c%23/1192/singleton-implementation#t=201607231143190778053

## Varsayılan Yapıcı
Yapıcı olmadan bir tür tanımlandığında:

    public class Animal
    {
    }

daha sonra derleyici aşağıdakine eşdeğer bir varsayılan kurucu oluşturur:

    public class Animal
    {
        public Animal() {}
    }

Tür için herhangi bir kurucunun tanımı, varsayılan kurucu oluşturmayı bastıracaktır. Tip aşağıdaki gibi tanımlansaydı:

    public class Animal
    {
        public Animal(string name) {}
    }

o zaman bir 'Hayvan' yalnızca bildirilen kurucu çağrılarak yaratılabilir.

    // This is valid
    var myAnimal = new Animal("Fluffy");
    // This fails to compile
    var unnamedAnimal = new Animal();

İkinci örnek için derleyici bir hata mesajı gösterecektir:
>'Hayvan', 0 argüman alan bir kurucu içermiyor

Bir sınıfın hem parametresiz bir kurucuya hem de bir parametre alan bir kurucuya sahip olmasını istiyorsanız, bunu her iki kurucuyu da açıkça uygulayarak yapabilirsiniz.

    public class Animal
    {
        
        public Animal() {} //Equivalent to a default constructor.
        public Animal(string name) {}
    }

Eğer sınıf parametresiz bir kurucuya sahip olmayan başka bir sınıfı genişletiyorsa, derleyici varsayılan bir kurucu oluşturamaz. Örneğin, bir "Creature" sınıfımız olsaydı:

    public class Creature
    {
        public Creature(Genus genus) {}
    }

o zaman 'sınıf Animal : Creature {}' olarak tanımlanan 'Animal' derlenmiyordu.

## Statik bir kurucuyu çağrılmaya zorlama
Statik oluşturucular her zaman bir türün ilk kullanımından önce çağrılsa da, bazen onları çağrılmaya zorlamak yararlıdır ve 'RuntimeHelpers' sınıfı bunun için bir yardımcı sağlar:

    using System.Runtime.CompilerServices;    
    // ...
    RuntimeHelpers.RunClassConstructor(typeof(Foo).TypeHandle);

***Açıklama*:** Yalnızca kurucunun kendisi değil, tüm statik başlatmalar (örneğin alan başlatıcılar) çalışacaktır.

***Olası kullanımlar*:** Bir UI uygulamasında açılış ekranı sırasında başlatmaya zorlama veya bir statik oluşturucunun birim testinde başarısız olmamasını sağlama.

## Başka bir kurucudan bir kurucu çağırma
    public class Animal
    {
        public string Name { get; set; }

        public Animal() : this("Dog")
        {
        }

        public Animal(string name)
        {
            Name = name;
        }
    }

    var dog = new Animal();      // dog.Name will be set to "Dog" by default.
    var cat = new Animal("Cat"); // cat.Name is "Cat", the empty constructor is not called.


## Temel sınıf yapıcısını çağırma
Bir temel sınıfın yapıcısı, türetilmiş bir sınıfın yapıcısı yürütülmeden önce çağrılır. Örneğin, "Memeli", "Hayvan"ı genişletiyorsa, bir "Memeli" örneği oluşturulurken ilk olarak "Hayvan" yapıcısında bulunan kod çağrılır.

Türetilmiş bir sınıf, temel sınıfın hangi kurucusunun çağrılması gerektiğini açıkça belirtmiyorsa, derleyici parametresiz kurucuyu varsayar.

    public class Animal
    {
        public Animal() { Console.WriteLine("An unknown animal gets born."); }
        public Animal(string name) { Console.WriteLine(name + " gets born"); }
    }

    public class Mammal : Animal
    {
        public Mammal(string name)
        {
            Console.WriteLine(name + " is a mammal.");
        }
    }

Bu durumda, 'new Mammal("George the Cat")' çağrılarak bir 'Memeli' örneğinin oluşturulması yazdırılacaktır.

>Bilinmeyen bir hayvan doğar.
>George the Cat bir memelidir.

[Demoyu Görüntüle][1]

Temel sınıfın farklı bir kurucusunu çağırmak, kurucunun imzası ile gövdesi arasına `: base(args)` yerleştirilerek yapılır:

    public class Mammal : Animal
    {
        public Mammal(string name) : base(name)
        {
            Console.WriteLine(name + " is a mammal.");
        }
    }

`new Mammal("George the Cat")` çağrısı yapmak şimdi şunu yazdıracak:

>George Kedi doğar.
>George the Cat bir memelidir.

[Demoyu Görüntüle][2]


[1]: https://dotnetfiddle.net/xb8Vqr
[2]: https://dotnetfiddle.net/gbdERq

## Türetilmiş sınıflarda sonlandırıcılar
Bir nesne grafiği sonlandırıldığında, sıralama yapının tersidir. Örneğin. süper tür, aşağıdaki kodun gösterdiği gibi, temel türden önce sonlandırılır:

    class TheBaseClass
    {
        ~TheBaseClass() 
        {
            Console.WriteLine("Base class finalized!");
        }
    }
    
    class TheDerivedClass : TheBaseClass
    {
        ~TheDerivedClass() 
        {
            Console.WriteLine("Derived class finalized!");
        }
    }

    //Don't assign to a variable
    //to make the object unreachable
    new TheDerivedClass();
    
    //Just to make the example work;
    //this is otherwise NOT recommended!
    GC.Collect();

    //Derived class finalized!
    //Base class finalized!

## Statik kurucularda istisnalar
Statik bir kurucu bir istisna atarsa, asla yeniden denenmez. Tür, AppDomain'in ömrü boyunca kullanılamaz. Türün diğer kullanımları, orijinal istisnanın etrafına sarılmış bir 'TypeInitializationException' oluşturacaktır.


    public class Animal
    {
        static Animal()
        {
            Console.WriteLine("Static ctor");
            throw new Exception();
        }
    
        public static void Yawn() {}
    }

    try
    {
        Animal.Yawn();
    }
    catch (Exception e)
    {
        Console.WriteLine(e.ToString());
    }

    try
    {
        Animal.Yawn();
    }
    catch (Exception e)
    {
        Console.WriteLine(e.ToString());
    }

Bu çıktı:

> Statik aktör
>
> System.TypeInitializationException: Tür başlatıcı
> 'Hayvan' için bir istisna attı. ---> System.Exception: İstisna
> 'System.Exception' yazın.

[...]

> System.TypeInitializationException: 'Hayvan' için tür başlatıcı
> bir istisna attı. ---> System.Exception: Tür istisnası
> 'System.Exception' atıldı.

burada gerçek kurucunun yalnızca bir kez yürütüldüğünü ve istisnanın yeniden kullanıldığını görebilirsiniz.

## Yapıcıda sanal yöntemleri çağırma
C#'daki C++'dan farklı olarak, sınıf yapıcısından sanal bir yöntem çağırabilirsiniz (Tamam, C++'da da yapabilirsiniz ancak ilk başta davranış şaşırtıcıdır). Örneğin:

    abstract class Base
    {
        protected Base()
        {
            _obj = CreateAnother();
        }
    
        protected virtual AnotherBase CreateAnother()
        {
            return new AnotherBase();
        }
    
        private readonly AnotherBase _obj;
    }
    
    sealed class Derived : Base
    {
        public Derived() { }
    
        protected override AnotherBase CreateAnother()
        {
            return new AnotherDerived();
        }
    }
    
    var test = new Derived();
    // test._obj is AnotherDerived

Bir C++ arka planından geliyorsanız, bu şaşırtıcıdır, temel sınıf oluşturucu zaten türetilmiş sınıf sanal yöntem tablosunu görür!

**Dikkatli olun**: türetilmiş sınıf henüz tam olarak başlatılmamış olabilir (kurucu, temel sınıf oluşturucudan sonra yürütülecektir) ve bu teknik tehlikelidir (bunun için bir StyleCop uyarısı da vardır). Genellikle bu kötü bir uygulama olarak kabul edilir.


## Genel Statik Yapıcılar
Statik oluşturucunun bildirildiği tür genel ise, statik oluşturucu, genel bağımsız değişkenlerin her benzersiz kombinasyonu için bir kez çağrılır.

    class Animal<T>
    {
        static Animal()
        {
            Console.WriteLine(typeof(T).FullName);
        }

        public static void Yawn() { }
    }

    Animal<Object>.Yawn();
    Animal<String>.Yawn();

Bu çıktı:

> Sistem.Nesne
> Sistem.Dizesi

Ayrıca bkz. [Genel türler için statik oluşturucular nasıl çalışır?][1]

[1]: http://stackoverflow.com/q/5629388

## Yapıcı ve Özellik Başlatma
Özellik değerinin ataması, sınıfın kurucusundan *önce* mi yoksa *sonra* mı yürütülecek?

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;
        
        public TestClass() 
        {
            if (TestProperty == 1) 
            {
                Console.WriteLine("Shall this be executed?");
            }

            if (TestProperty == 2) 
            {
                Console.WriteLine("Or shall this be executed");
            }
        }
    }

    var testInstance = new TestClass() { TestProperty = 1 };

Yukarıdaki örnekte, 'TestProperty' değeri sınıf kurucusunda mı yoksa sınıf kurucusundan sonra mı '1' olacak?

----

Örnek oluşturmada şu şekilde özellik değerleri atamak:

    var testInstance = new TestClass() {TestProperty = 1};

Yapıcı çalıştırıldıktan ***sonra çalıştırılacaktır. Ancak, C# 6.0'da class' özelliğindeki özellik değerini şu şekilde başlatmak:

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;

        public TestClass() 
        {
        }
    }

yapıcı çalıştırılmadan ***önce yapılacaktır.

---

Yukarıdaki iki kavramı tek bir örnekte birleştirmek:

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;
        
        public TestClass() 
        {
            if (TestProperty == 1) 
            {
                Console.WriteLine("Shall this be executed?");
            }

            if (TestProperty == 2) 
            {
                Console.WriteLine("Or shall this be executed");
            }
        }
    }

    static void Main(string[] args) 
    {
        var testInstance = new TestClass() { TestProperty = 1 };
        Console.WriteLine(testInstance.TestProperty); //resulting in 1
    }

Son sonuç:

    "Or shall this be executed"
    "1"

---

**Açıklama:**

'TestProperty' değeri önce '2' olarak atanacak, ardından 'TestClass' oluşturucusu çalıştırılacak ve bunun yazdırılmasıyla sonuçlanacaktır.

    "Or shall this be executed"
    
Ve sonra 'TestProperty', 'new TestClass() { TestProperty = 1 }' nedeniyle '1' olarak atanacak ve 'Console.WriteLine(testInstance.TestProperty)' tarafından basılan 'TestProperty' için son değer olacak.

    "1"



