---
title: "Delegeler"
slug: "delegeler"
draft: false
images: []
weight: 9752
type: docs
toc: true
---

# Özet

Bir **temsilci türü**, belirli bir yöntem imzasını temsil eden bir türdür. Bu türün bir örneği, eşleşen bir imzaya sahip belirli bir yönteme başvurur. Yöntem parametrelerinin temsilci türleri olabilir ve bu nedenle bu yöntem, daha sonra çağrılabilecek başka bir yönteme başvuru iletilir.

# Yerleşik temsilci türleri: `Action<...>`, `Predicate<T>` ve `Func<...,TResult>`

'Sistem' ad alanı, 'Action<...>', 'Predicate<T>' ve 'Func<...,TResult>' delegelerini içerir, burada "..." 0 ila 16 genel tür parametresini temsil eder ( 0 parametre için "Eylem" genel değildir).

"Func", "TResult" ile eşleşen bir dönüş tipine sahip yöntemleri temsil eder ve "Action", bir dönüş değeri (void) olmayan yöntemleri temsil eder. Her iki durumda da, ek genel tür parametreleri, sırayla, yöntem parametreleriyle eşleşir.

"Yükleme", boolean dönüş tipine sahip yöntemi temsil eder, T ise girdi parametresidir.

# Özel temsilci türleri

Adlandırılmış temsilci türleri, 'delegate' anahtar sözcüğü kullanılarak bildirilebilir.

# Temsilci çağırma

Temsilciler, yöntemlerle aynı sözdizimi kullanılarak çağrılabilir: temsilci örneğinin adı, ardından herhangi bir parametre içeren parantezler.

# Temsilcilere atama

Delegeler aşağıdaki şekillerde atanabilir:

- Adlandırılmış bir yöntem atama
- Bir lambda kullanarak anonim bir yöntem atama
- 'delegate' anahtar sözcüğünü kullanarak adlandırılmış bir yöntem atama.

# Temsilcileri birleştirmek

"+" operatörü kullanılarak bir temsilci örneğine birden çok temsilci nesnesi atanabilir. `-` operatörü, bir bileşen temsilcisini başka bir temsilciden çıkarmak için kullanılabilir.

## Bir temsilci türü bildirme
Aşağıdaki sözdizimi, bir "int" alan ve bir "int" döndüren bir yöntemi temsil eden "NumberInOutDelegate" adlı bir "temsilci" türü oluşturur.

    public delegate int NumberInOutDelegate(int input);

Bu aşağıdaki gibi kullanılabilir:

    public static class Program
    {
        static void Main()
        {
            NumberInOutDelegate square = MathDelegates.Square;
            int answer1 = square(4); 
            Console.WriteLine(answer1); // Will output 16

            NumberInOutDelegate cube = MathDelegates.Cube;
            int answer2 = cube(4);
            Console.WriteLine(answer2); // Will output 64            
        }
    }
    
    public static class MathDelegates
    {
        static int Square (int x)
        {
            return x*x;
        }

        static int Cube (int x)
        {
            return x*x*x;
        }
    }

"Örnek" temsilci örneği, "Square" yöntemiyle aynı şekilde yürütülür. Bir temsilci örneği, kelimenin tam anlamıyla arayan için bir temsilci görevi görür: arayan,
temsilci ve ardından temsilci hedef yöntemi çağırır. Bu dolaylı ayrıştırma
hedef yöntemden arayan.

----------

Bir __generic__ temsilci türü bildirebilirsiniz ve bu durumda, tür bağımsız değişkenlerinin bazılarında türün kovaryant ('out') veya contravariant ('in') olduğunu belirtebilirsiniz. Örneğin:

    public delegate TTo Converter<in TFrom, out TTo>(TFrom input);

Diğer genel türler gibi, genel temsilci türleri de 'where TFrom : struct, IConvertible where TTo : new()' gibi kısıtlamalara sahip olabilir.

Olay işleyici türleri gibi çok noktaya yayın delegeleri için kullanılması amaçlanan temsilci türleri için eş ve çelişkiden kaçının. Bunun nedeni, varyans nedeniyle çalışma zamanı türü derleme zamanı türünden farklıysa birleştirmenin ('+') başarısız olabilmesidir. Örneğin, kaçının:

    public delegate void EventHandler<in TEventArgs>(object sender, TEventArgs e);

Bunun yerine, değişmez bir genel tür kullanın:

    public delegate void EventHandler<TEventArgs>(object sender, TEventArgs e);

----------

Ayrıca, bazı parametrelerin aşağıdaki gibi "ref" veya "out" ile değiştirildiği temsilciler de desteklenir:

    public delegate bool TryParser<T>(string input, out T result);

(örnek `TryParser<decimal> örnek = decimal.TryParse;` kullanın) veya son parametrenin `params` değiştiricisine sahip olduğu temsilciler. Temsilci türleri isteğe bağlı parametrelere sahip olabilir (varsayılan değerleri sağlar). Temsilci türleri, imzalarında veya dönüş türlerinde 'int*' veya 'char*' gibi işaretçi türlerini kullanabilir ('güvenli olmayan' anahtar sözcüğünü kullanın). Bir temsilci türü ve parametreleri özel nitelikler taşıyabilir.

## Func<T, TResult>, Action<T> ve Predicate<T> temsilci türleri
System ad alanı, 0 ile 15 arasında genel parametreye sahip "Func<..., TResult>" temsilci türlerini içerir ve "TResult" türünü döndürür.

    private void UseFunc(Func<string> func)
    {
        string output = func(); // Func with a single generic type parameter returns that type
        Console.WriteLine(output);
    }

    private void UseFunc(Func<int, int, string> func)
    {
        string output = func(4, 2); // Func with multiple generic type parameters takes all but the first as parameters of that type
        Console.WriteLine(output);
    }

Sistem ad alanı ayrıca farklı sayıda genel parametreye sahip (0'dan 16'ya kadar) 'Eylem<...>' temsilci türlerini de içerir. "Func<T1, .., Tn>"ye benzer, ancak her zaman "void" döndürür.

    private void UseAction(Action action)
    {
        action(); // The non-generic Action has no parameters
    }

    private void UseAction(Action<int, string> action)
    {
        action(4, "two"); // The generic action is invoked with parameters matching its type arguments
    }

'Yüklem<T>' aynı zamanda bir 'Func' biçimidir, ancak her zaman 'bool' döndürür. Bir yüklem, özel bir ölçüt belirtmenin bir yoludur. Girdinin değerine ve yüklem içinde tanımlanan mantığa bağlı olarak, ya "doğru" ya da "yanlış" döndürür. `Predicate<T>` bu nedenle `Func<T, bool>` ile aynı şekilde davranır ve her ikisi de aynı şekilde başlatılabilir ve kullanılabilir.
    
    Predicate<string> predicate = s => s.StartsWith("a");
    Func<string, bool> func = s => s.StartsWith("a");

    // Both of these return true
    var predicateReturnsTrue = predicate("abc");
    var funcReturnsTrue = func("abc");

    // Both of these return false
    var predicateReturnsFalse = predicate("xyz");
    var funcReturnsFalse = func("xyz");

`Predicate<T>` veya `Func<T, bool>` kullanıp kullanmama seçimi gerçekten bir fikir meselesidir. 'Yüklem<T>' yazarın amacını muhtemelen daha fazla ifade ederken, 'Func<T, bool>' muhtemelen C# geliştiricilerinin daha büyük bir kısmına aşinadır.

Buna ek olarak, özellikle başka bir API ile etkileşimde bulunurken seçeneklerden yalnızca birinin mevcut olduğu bazı durumlar vardır. Örneğin, 'List<T>' ve 'Array<T>', yöntemleri için genellikle 'Predicate<T>' değerini alırken, çoğu LINQ uzantısı yalnızca 'Func<T, bool>'u kabul eder.

## Delegeleri Birleştir (Çok Noktaya Yayın Delegeleri)
Temsilci örneklerini birleştirmek için toplama '+' ve çıkarma '-' işlemleri kullanılabilir. Temsilci, atanan delegelerin bir listesini içerir.

    using System;
    using System.Reflection;
    using System.Reflection.Emit;

    namespace DelegatesExample {
        class MainClass {
            private delegate void MyDelegate(int a);

            private static void PrintInt(int a) {
                Console.WriteLine(a);
            }

            private static void PrintType<T>(T a) {
                Console.WriteLine(a.GetType());
            }

            public static void Main (string[] args)
            {
                MyDelegate d1 = PrintInt;
                MyDelegate d2 = PrintType;

                // Output:
                // 1
                d1(1);

                // Output:
                // System.Int32
                d2(1);

                MyDelegate d3 = d1 + d2;
                // Output:
                // 1
                // System.Int32
                d3(1);

                MyDelegate d4 = d3 - d2;
                // Output:
                // 1
                d4(1);

                // Output:
                // True
                Console.WriteLine(d1 == d4);
            }
        }
    }

Bu örnekte "d3", "d1" ve "d2" temsilcilerinin bir birleşimidir, bu nedenle program çağrıldığında hem "1" hem de "System.Int32" dizelerini verir.


----------

Delegeleri **void olmayan** dönüş türleriyle birleştirmek:

Çok noktaya yayın temsilcisinin "geçersiz" bir dönüş türü varsa, arayan kişi dönüş değerini alır.
çağrılacak son yöntemden. Önceki yöntemler hala çağrılır, ancak bunların
dönüş değerleri atılır.

        class Program
        {
            public delegate int Transformer(int x);

            static void Main(string[] args)
            {
                Transformer t = Square;
                t += Cube;
                Console.WriteLine(t(2));  // O/P 8 
            }

            static int Square(int x) { return x * x; }

            static int Cube(int x) { return x*x*x; }
        }

`t(2)` önce `Square` ve sonra `Cube` adını verecektir. Square'in dönüş değeri atılır ve son yöntemin, yani "Küp"ün dönüş değeri korunur.

## Güvenli çok noktaya yayın temsilcisini çağırma
Hiç bir çok noktaya yayın temsilcisini aramak istediniz, ancak zincirde herhangi bir istisna oluşsa bile tüm çağrı listesinin çağrılmasını istiyorsunuz. O zaman şanslısınız, sadece tüm listenin yürütülmesi tamamlandıktan sonra bir 'AggregateException' atarak bunu yapan bir uzatma yöntemi yarattım:

    public static class DelegateExtensions
    {
        public static void SafeInvoke(this Delegate del,params object[] args)
        {
            var exceptions = new List<Exception>();

            foreach (var handler in del.GetInvocationList())
            {
                try
                {
                    handler.Method.Invoke(handler.Target, args);
                }
                catch (Exception ex)
                {
                    exceptions.Add(ex);
                }
            }

            if(exceptions.Any())
            {
                throw new AggregateException(exceptions);
            }
        }
    }

    public class Test
    {
        public delegate void SampleDelegate();

        public void Run()
        {
            SampleDelegate delegateInstance = this.Target2;
            delegateInstance += this.Target1;

            try
            {
                delegateInstance.SafeInvoke();
            } 
            catch(AggregateException ex)
            {
                // Do any exception handling here
            }
        }

        private void Target1()
        {
            Console.WriteLine("Target 1 executed");
        }

        private void Target2()
        {
            Console.WriteLine("Target 2 executed");
            throw new Exception();
        }
    }

Bu çıktı:

    Target 2 executed
    Target 1 executed

'SaveInvoke' olmadan doğrudan çağırmak yalnızca Hedef 2'yi yürütür.

## Temsilci Eşitliği
Bir temsilci üzerinde '.Equals()' çağırmak, referans eşitliğine göre karşılaştırır:

    Action action1 = () => Console.WriteLine("Hello delegates");
    Action action2 = () => Console.WriteLine("Hello delegates");
    Action action1Again = action1;

    Console.WriteLine(action1.Equals(action1)) // True
    Console.WriteLine(action1.Equals(action2)) // False
    Console.WriteLine(action1Again.Equals(action1)) // True

Bu kurallar aynı zamanda bir çok noktaya yayın temsilcisi üzerinde `+=` veya `-=` yaparken, örneğin etkinliklere abone olurken ve abonelikten çıkarken de geçerlidir.

## Adlandırılmış yöntem delegelerinin temel referansları
Temsilcilere adlandırılmış yöntemler atarken, aşağıdaki durumlarda aynı temel nesneye başvururlar:

- Bir sınıfın aynı örneğinde aynı örnek yöntemidirler
- Bir sınıftaki aynı statik yöntemdir

       public class Greeter
       {
           public void WriteInstance()
           {
               Console.WriteLine("Instance");
           }

           public static void WriteStatic()
           {
               Console.WriteLine("Static");
           }
       }

       // ...

       Greeter greeter1 = new Greeter();
       Greeter greeter2 = new Greeter();

       Action instance1 = greeter1.WriteInstance;
       Action instance2 = greeter2.WriteInstance;
       Action instance1Again = greeter1.WriteInstance;
    
       Console.WriteLine(instance1.Equals(instance2)); // False
       Console.WriteLine(instance1.Equals(instance1Again)); // True

       Action @static = Greeter.WriteStatic;
       Action staticAgain = Greeter.WriteStatic;

       Console.WriteLine(@static.Equals(staticAgain)); // True

## Bir temsilciye adlandırılmış bir yöntem atama
Adlandırılmış yöntemler, eşleşen imzalara sahip temsilcilere atanabilir:

    public static class Example
    {
        public static int AddOne(int input)
        {
            return input + 1;
        }
    }


    Func<int,int> addOne = Example.AddOne

"Example.AddOne" bir "int" alır ve bir "int" döndürür, imzası "Func<int,int>" temsilcisiyle eşleşir. "Example.AddOne", eşleşen imzalara sahip oldukları için doğrudan "addOne"a atanabilir.

## lambda ile bir temsilciye atama
Lambda'lar, bir temsilciye atanacak anonim yöntemler oluşturmak için kullanılabilir:

    Func<int,int> addOne = x => x+1;

Bu şekilde bir değişken oluştururken açık tip bildiriminin gerekli olduğunu unutmayın:

    var addOne = x => x+1; // Does not work

## Delegeleri parametre olarak geçirme
Delegeler, yazılan işlev işaretçileri olarak kullanılabilir:

    class FuncAsParameters
    {
      public void Run()
      {
        DoSomething(ErrorHandler1);
        DoSomething(ErrorHandler2);
      }
    
      public bool ErrorHandler1(string message)
      {
        Console.WriteLine(message);
        var shouldWeContinue = ...  
        return shouldWeContinue;
      }
    
      public bool ErrorHandler2(string message)
      {
        // ...Write message to file...
        var shouldWeContinue = ...  
        return shouldWeContinue;
      }
    
      public void DoSomething(Func<string, bool> errorHandler)
      {
        // In here, we don't care what handler we got passed!
        ...
        if (...error...)
        {
          if (!errorHandler("Some error occurred!"))
          {
            // The handler decided we can't continue
            return;
          }
        }
      }
    }

## Bir temsilcinin içinde kapanış


## Dönüşümleri funcs'ta kapsülleme


