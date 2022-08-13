---
title: "yöntemler"
slug: "yontemler"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Yöntem Çağırma
Statik bir yöntem çağırmak:

    // Single argument
    System.Console.WriteLine("Hello World");  

    // Multiple arguments
    string name = "User";
    System.Console.WriteLine("Hello, {0}!", name);  

Statik bir yöntemi çağırmak ve dönüş değerini saklamak:

    string input = System.Console.ReadLine();

Bir örnek yöntemi çağırmak:

    int x = 42;
    // The instance method called here is Int32.ToString()
    string xAsString = x.ToString();

Genel bir yöntem çağırma

    // Assuming a method 'T[] CreateArray<T>(int size)'
    DateTime[] dates = CreateArray<DateTime>(8);

## Anonim yöntem
Anonim yöntemler, bir kod bloğunu temsilci parametresi olarak geçirmek için bir teknik sağlar. Bir gövdeye sahip yöntemlerdir, ancak adı yoktur.
    
    
    delegate int IntOp(int lhs, int rhs);

<!-- ayırıcı -->

    class Program
    {
        static void Main(string[] args)
        {
            // C# 2.0 definition
            IntOp add = delegate(int lhs, int rhs)
            {
                return lhs + rhs;
            };

            // C# 3.0 definition
            IntOp mul = (lhs, rhs) =>
            {
                return lhs * rhs;
            };

            // C# 3.0 definition - shorthand
            IntOp sub = (lhs, rhs) => lhs - rhs;

            // Calling each method
            Console.WriteLine("2 + 3 = " + add(2, 3));
            Console.WriteLine("2 * 3 = " + mul(2, 3));
            Console.WriteLine("2 - 3 = " + sub(2, 3));
        }
    }

## Bir Yöntem Bildirme
Her yöntemin bir erişimci ('public', 'private', ...), isteğe bağlı değiştirici ('abstract'), bir ad ve gerekirse yöntem parametrelerinden oluşan benzersiz bir imzası vardır.
Dönüş türünün imzanın bir parçası olmadığını unutmayın. Bir yöntem prototipi aşağıdakine benzer:

    AccessModifier OptionalModifier ReturnType MethodName(InputParameters)
    {
        //Method body
    }

"AccessModifier" "genel", "korumalı", "korumalı" veya varsayılan olarak "dahili" olabilir.

"OptionalModifier", "statik" "soyut" "sanal" "geçersiz kılma" "yeni" veya "mühürlü" olabilir.

'ReturnType' dönüşü olmayan 'void' olabilir veya temel sınıflardan karmaşık sınıflara 'int' gibi herhangi bir tür olabilir.

Bir Yöntemin bazı girdi parametreleri olabilir veya hiç olmayabilir. Bir metot için parametreleri ayarlamak için, her birini normal değişken bildirimleri gibi ('int a' gibi) bildirmelisiniz ve birden fazla parametre için aralarında virgül kullanmalısınız ('int a, int b' gibi).

Parametrelerin varsayılan değerleri olabilir. bunun için parametre için bir değer ayarlamalısınız (`int a = 0` gibi). bir parametrenin varsayılan bir değeri varsa, giriş değerini ayarlamak isteğe bağlıdır.

Aşağıdaki yöntem örneği, iki tamsayının toplamını döndürür:

    private int Sum(int a, int b)
    {
        return a + b;
    } 


## Parametreler ve Argümanlar
Bir yöntem herhangi bir sayıda parametre bildirebilir (bu örnekte, "i", "s" ve "o" parametrelerdir):

    static void DoSomething(int i, string s, object o) {
        Console.WriteLine(String.Format("i={0}, s={1}, o={2}", i, s, o));
    }

Parametreler, yöntemin onlarla çalışabilmesi için değerleri bir yönteme aktarmak için kullanılabilir. Bu, değerleri yazdırmak veya bir parametrenin referans verdiği nesnede değişiklik yapmak veya değerleri depolamak gibi her türlü iş olabilir.

Yöntemi çağırdığınızda, her parametre için gerçek bir değer iletmeniz gerekir. Bu noktada, yöntem çağrısına gerçekten ilettiğiniz değerlere Bağımsız Değişkenler adı verilir:

    DoSomething(x, "hello", new object());



## İade Türleri
Bir yöntem ya hiçbir şey ("void") ya da belirtilen türden bir değer döndürebilir:

    // If you don't want to return a value, use void as return type.
    static void ReturnsNothing() { 
        Console.WriteLine("Returns nothing");
    }

    // If you want to return a value, you need to specify its type.
    static string ReturnsHelloWorld() {
        return "Hello World";
    }

Yönteminiz bir dönüş değeri belirtiyorsa, yöntemin *bir değer döndürmesi gerekir. Bunu "return" deyimini kullanarak yaparsınız. Bir "return" deyimine ulaşıldığında, belirtilen değeri döndürür ve bundan sonraki herhangi bir kod artık çalıştırılmayacaktır (istisnalar, yöntem geri dönmeden önce hala yürütülecek olan "nihayet" bloklarıdır).

Metodunuz hiçbir şey döndürmüyorsa ("void"), yöntemden hemen dönmek istiyorsanız, yine de "return" ifadesini bir değer olmadan kullanabilirsiniz. Böyle bir yöntemin sonunda, bir 'return' ifadesi gereksiz olacaktır.

Geçerli "dönüş" ifadelerine örnekler:

    return; 
    return 0; 
    return x * 2;
    return Console.ReadLine();

Bir istisna atmak, bir değer döndürmeden yöntem yürütmeyi sonlandırabilir. Ayrıca, getiri anahtar kelimesi kullanılarak dönüş değerlerinin üretildiği yineleyici bloklar vardır, ancak bunlar bu noktada açıklanmayacak özel durumlardır.

## Varsayılan Parametreler
Parametreleri dışarıda bırakma seçeneği sağlamak istiyorsanız varsayılan parametreleri kullanabilirsiniz:

    static void SaySomething(string what = "ehh") {
        Console.WriteLine(what);
    }  

    static void Main() {
        // prints "hello"
        SaySomething("hello"); 
        // prints "ehh"
        SaySomething(); // The compiler compiles this as if we had typed SaySomething("ehh")
    }

Böyle bir yöntemi çağırdığınızda ve varsayılan bir değerin sağlandığı bir parametreyi atladığınızda, derleyici bu varsayılan değeri sizin için ekler.

Varsayılan değerlere sahip parametrelerin varsayılan değerleri olmayan parametrelerden **sonra** yazılması gerektiğini unutmayın.

    static void SaySomething(string say, string what = "ehh") {
            //Correct
            Console.WriteLine(say + what);
        }

    static void SaySomethingElse(string what = "ehh", string say) {
            //Incorrect
            Console.WriteLine(say + what);
        }   

**UYARI**: Bu şekilde çalıştığı için bazı durumlarda varsayılan değerler sorunlu olabilir. Bir yöntem parametresinin varsayılan değerini değiştirirseniz ve bu yöntemin tüm çağıranlarını yeniden derlemezseniz, bu çağıranlar derlendiklerinde mevcut olan varsayılan değeri kullanmaya devam edecek ve muhtemelen tutarsızlıklara neden olacaktır.

## Yöntem aşırı yüklemesi
**Tanım :** Aynı ada sahip birden çok yöntem farklı parametrelerle bildirildiğinde, yöntem aşırı yüklemesi olarak adlandırılır. Yöntem aşırı yüklemesi, tipik olarak amaçları bakımından aynı olan ancak farklı veri türlerini parametreleri olarak kabul etmek için yazılan işlevleri temsil eder.

**Etki eden faktörler**

- Argüman Sayısı
- Argüman türü
- İade Türü**

Çeşitli argümanları kabul edecek ve sonucu döndürecek, hesaplama fonksiyonlarını gerçekleştirecek 'Alan' adında bir yöntem düşünün.

**Örnek**

    public string Area(int value1)
    {
        return String.Format("Area of Square is {0}", value1 * value1);
    }
Bu yöntem bir argüman kabul eder ve bir dizge döndürür, eğer yöntemi bir tamsayı ile çağırırsak('5' diyelim) çıktı `"Karenin Alanı 25'tir"` olacaktır.

    public  double Area(double value1, double value2)
    {
        return value1 * value2;
    }
Benzer şekilde, bu yönteme iki double değeri iletirsek, çıktı iki değerin çarpımı olacaktır ve double türündedir. Bu, dikdörtgenlerin Alanını bulmanın yanı sıra çarpma için de kullanılabilir.

    public double Area(double value1)
    {
        return 3.14 * Math.Pow(value1,2);
    }
Bu, dairenin alanını bulmak için özel olarak kullanılabilir; bu, bir çift değeri ("yarıçap") kabul edecek ve kendi Alanı olarak başka bir çift değer döndürecektir.

Bu yöntemlerin her biri normal olarak çakışma olmadan çağrılabilir - derleyici, hangi 'Alan' sürümünün kullanılması gerektiğini belirlemek için her yöntem çağrısının parametrelerini inceler.

    string squareArea = Area(2);
    double rectangleArea = Area(32.0, 17.5);
    double circleArea = Area(5.0); // all of these are valid and will compile.


----------


**Yalnızca* dönüş türünün iki yöntem arasında ayrım yapamayacağını unutmayın. Örneğin, Alan için aynı parametrelere sahip iki tanımımız olsaydı, şunun gibi:

    public string Area(double width, double height) { ... }
    public double Area(double width, double height) { ... }
    // This will NOT compile. 

Sınıfımızın farklı değerler döndüren aynı yöntem adlarını kullanmasını istiyorsak, bir arabirim uygulayarak ve kullanımını açıkça tanımlayarak belirsizlik sorunlarını ortadan kaldırabiliriz.

    public interface IAreaCalculatorString {
        
        public string Area(double width, double height);

    }

    public class AreaCalculator : IAreaCalculatorString {

        public string IAreaCalculatorString.Area(double width, double height) { ... } 
        // Note that the method call now explicitly says it will be used when called through
        // the IAreaCalculatorString interface, allowing us to resolve the ambiguity.
        public double Area(double width, double height) { ... }


## Erişim hakları
    // static: is callable on a class even when no instance of the class has been created
    public static void MyMethod()

    // virtual: can be called or overridden in an inherited class
    public virtual  void MyMethod()

    // internal: access is limited within the current assembly
    internal  void MyMethod()

    //private: access is limited only within the same class
    private  void MyMethod()

    //public: access right from every class / assembly
    public void MyMethod()

    //protected: access is limited to the containing class or types derived from it
    protected void MyMethod()

    //protected internal: access is limited to the current assembly or types derived from the containing class.
    protected internal void MyMethod()

