---
title: "Uzatma Yöntemleri"
slug: "uzatma-yontemleri"
draft: false
images: []
weight: 6772
type: docs
toc: true
---

## Sözdizimi
- genel statik ReturnType MyExtensionMethod(bu TargetType hedefi)
- genel statik ReturnType MyExtensionMethod(bu TargetType hedefi, TArg1 arg1, ...)

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| bu | Bir uzantı yönteminin ilk parametresinin başında her zaman `this` anahtar sözcüğü ve ardından genişletmekte olduğunuz nesnenin "geçerli" örneğine atıfta bulunulan tanımlayıcı gelmelidir |


Uzantı yöntemleri, nesne örneklerinde statik yöntemlerin, türün kendisinin bir üyesiymiş gibi çağrılmasına izin veren sözdizimsel şekerdir.

Uzantı yöntemleri, açık bir hedef nesne gerektirir. Uzatılmış türün kendisinden yönteme erişmek için `this` anahtar sözcüğünü kullanmanız gerekecektir.

Uzantı yöntemleri statik olarak bildirilmelidir ve statik bir sınıfta yaşamalıdır.

**Hangi ad alanı?**

Uzantı yöntemi sınıfınız için ad alanı seçimi, görünürlük ve keşfedilebilirlik arasında bir dengedir.

En sık bahsedilen [seçenek][1], uzantı yöntemleriniz için özel bir ad alanına sahip olmaktır. Ancak bu, kodunuzun kullanıcılarının uzantı yöntemlerinin var olduğunu ve bunları nerede bulacaklarını bilmeleri için bir iletişim çabası gerektirecektir.

Bir alternatif, geliştiricilerin Intellisense aracılığıyla uzantı yöntemlerinizi keşfedeceği bir ad alanı seçmektir. Dolayısıyla, 'Foo' sınıfını genişletmek istiyorsanız, uzantı yöntemlerini 'Foo' ile aynı ad alanına koymak mantıklıdır.

**Hiçbir şeyin "başkasının" ad alanını kullanmanıza engel olmadığını anlamak önemlidir**: Bu nedenle, "IEnumerable"ı genişletmek istiyorsanız, "System.Linq" ad alanına uzantı yönteminizi ekleyebilirsiniz.

Bu *her zaman* iyi bir fikir değildir. Örneğin, belirli bir durumda, ortak bir türü genişletmek isteyebilirsiniz (örneğin, "bool IsApproxEqualTo(bu double value, double other)"), ancak "Sistem"in tamamını "kirleten" değil. Bu durumda yerel, özel bir ad alanı seçilmesi tercih edilir.

Son olarak, uzantı yöntemlerini *hiç ad alanına* yerleştirmek de mümkündür!

İyi bir referans sorusu: [Uzantı yöntemlerinizin ad alanlarını nasıl yönetirsiniz?][2]

**Uygulanabilirlik**

Tüm olası girdiler için uygun olduklarından ve yalnızca belirli durumlarla ilgili olmadıklarından emin olmak için genişletme yöntemleri oluşturulurken dikkatli olunmalıdır. Örneğin, yeni kodunuzu **herhangi bir** dize için kullanılabilir hale getiren 'string' gibi sistem sınıflarını genişletmek mümkündür. Kodunuzun etki alanına özgü bir dize biçiminde etki alanına özgü mantığı gerçekleştirmesi gerekiyorsa, varlığı sistemdeki diğer dizelerle çalışan arayanların kafasını karıştıracağından bir uzantı yöntemi uygun olmaz.

**Aşağıdaki liste, uzatma yöntemlerinin temel özelliklerini ve özelliklerini içerir**

1. Statik bir yöntem olmalıdır.
2. Statik bir sınıfta yer almalıdır.
3. .NET'te bir türe sahip ilk parametre olarak "this" anahtar sözcüğünü kullanır ve bu yöntem, istemci tarafında belirli bir tür örneği tarafından çağrılır.
4. Ayrıca VS intellisense tarafından da gösterilir. Bir tür örneğinden sonra `.` noktasına bastığımızda, VS intellisense'de geliyor.
5. Uzantı yöntemi, kullanıldığı adla aynı ad alanında olmalıdır veya sınıfın ad alanını bir using deyimiyle içe aktarmanız gerekir.
6. Uzantı yöntemi olan sınıfa istediğiniz ismi verebilirsiniz ancak sınıf statik olmalıdır.
7. Bir türe yeni yöntemler eklemek istiyorsanız ve bunun için kaynak kodunuz yoksa, çözüm o türün uzantı yöntemlerini kullanmak ve uygulamaktır.
8. Genişlettiğiniz türle aynı imza yöntemlerine sahip uzantı yöntemleri oluşturursanız, uzantı yöntemleri hiçbir zaman çağrılmaz.


[1]: http://stackoverflow.com/q/1226189
[2]: http://stackoverflow.com/questions/2520446/how-do-you-manage-the-namespaces-of-your-extension-methods

## Uzantı yöntemleri - genel bakış
Uzantı yöntemleri C# 3.0'da tanıtıldı. Uzantı yöntemleri, yeni bir türetilmiş tür oluşturmadan, yeniden derlemeden veya orijinal türü değiştirmeden mevcut türlere davranış ekler ve genişletir. *Geliştirmek istediğiniz bir türün kaynağını değiştiremediğinizde özellikle yararlıdırlar.* Sistem türleri, üçüncü şahıslar tarafından tanımlanan türler ve sizin tanımladığınız türler için uzantı yöntemleri oluşturulabilir. Uzantı yöntemi, orijinal türün bir üye yöntemiymiş gibi çağrılabilir. Bu, bir **Akıcı Arayüzü** uygulamak için kullanılan **Yöntem Zincirleme**'ye izin verir.

Genişletilen orijinal türden farklı olan bir **statik sınıfa** bir **statik yöntem** eklenerek bir uzantı yöntemi oluşturulur. Uzantı yöntemini tutan statik sınıf, genellikle yalnızca uzantı yöntemlerini tutmak amacıyla oluşturulur.

Uzatma yöntemleri, genişletilmekte olan orijinal türü belirten özel bir ilk parametre alır. Bu ilk parametre 'this' anahtar kelimesiyle süslenmiştir (bu, C#'da 'this'in özel ve farklı bir kullanımını oluşturur; bu, geçerli nesne örneğinin üyelerine atıfta bulunmaya izin veren 'this' kullanımından farklı olarak anlaşılmalıdır) .

Aşağıdaki örnekte, genişletilen orijinal tür "string" sınıfıdır. "Dize", kısaltmanın ek işlevselliğini sağlayan "Shorten()" yöntemiyle genişletildi. Uzantı yöntemini tutmak için 'StringExtensions' statik sınıfı oluşturuldu. "Shorten()" uzantı yöntemi, bunun özel olarak işaretlenmiş ilk parametre aracılığıyla "dize"nin bir uzantısı olduğunu gösterir. "Shorten()" yönteminin "dize"yi genişlettiğini göstermek için, ilk parametre "this" ile işaretlenir. Bu nedenle, ilk parametrenin tam imzası "bu dize metni"dir, burada "dize" genişletilen orijinal türdür ve "metin" seçilen parametre adıdır.

    static class StringExtensions
    {
        public static string Shorten(this string text, int length) 
        {
            return text.Substring(0, length);
        }
    }

    class Program
    {
        static void Main()
        {
            // This calls method String.ToUpper()
            var myString = "Hello World!".ToUpper();

            // This calls the extension method StringExtensions.Shorten()
            var newString = myString.Shorten(5); 

            // It is worth noting that the above call is purely syntactic sugar
            // and the assignment below is functionally equivalent
            var newString2 = StringExtensions.Shorten(myString, 5);
        }
    }

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/uiPhpP)

-------------------------------------------------- ---------------------------------

Bir uzantı yönteminin* ilk argümanı olarak iletilen nesne ("bu" anahtar sözcüğüyle birlikte gelir), uzantı yönteminin çağrıldığı örnektir.

Örneğin, bu kod yürütüldüğünde:

    "some string".Shorten(5);

Argümanların değerleri aşağıdaki gibidir:

    text: "some string"
    length: 5

*Uzantı yöntemlerinin yalnızca tanımlarıyla aynı ad alanında olmaları, ad alanının uzantı yöntemi kullanılarak kod tarafından açıkça içe aktarılması veya uzantı sınıfının ad alanı içermemesi durumunda kullanılabilir olduğunu unutmayın.* .NET çerçeve yönergeleri şunları önerir: uzantı sınıflarını kendi ad alanlarına koymak. Ancak, bu keşif sorunlarına yol açabilir.

Bu, çakışabilecek ad alanları açıkça alınmadıkça, uzantı yöntemleri ve kullanılan kitaplıklar arasında hiçbir çakışmayla sonuçlanmaz. Örneğin [LINQ Uzantıları][1]:
    
    using System.Linq; // Allows use of extension methods from the System.Linq namespace

    class Program
    {
        static void Main()
        {
            var ints = new int[] {1, 2, 3, 4};

            // Call Where() extension method from the System.Linq namespace
            var even = ints.Where(x => x % 2 == 0); 
        }
    }

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/IF223c)

-------------------------------------------------- ---------------------------------

C# 6.0'dan beri, uzantı yöntemlerini içeren _class_'a bir 'using static' yönergesi koymak da mümkündür. Örneğin, `statik System.Linq.Enumerable kullanarak;`. Bu, aynı ad alanındaki diğer türleri kapsama sokmadan, o belirli sınıftan genişletme yöntemlerini kullanılabilir hale getirir.

-------------------------------------------------- ---------------------------------

Aynı imzaya sahip bir sınıf yöntemi mevcut olduğunda, derleyici, uzantı yöntemi çağrısına göre buna öncelik verir. Örneğin:

    class Test
    {
       public void Hello()
       {
           Console.WriteLine("From Test");
       }
    }

    static class TestExtensions
    {
        public static void Hello(this Test test)
        {
            Console.WriteLine("From extension method");
        }
    }

    class Program
    {
        static void Main()
        {
            Test t = new Test();
            t.Hello(); // Prints "From Test"
        }
    }

<!-- eğer --> son sürüm

[.NET Fiddle'da canlı demo](https://dotnetfiddle.net/fI3sCJ)

-------------------------------------------------- ---------------------------------

Aynı imzaya sahip iki uzantı işlevi varsa ve bunlardan biri aynı ad alanındaysa, bu işleve öncelik verileceğini unutmayın. Öte yandan, her ikisine de 'kullanarak' erişilirse, şu mesajla birlikte bir derleme zamanı hatası ortaya çıkar:
>>Çağrı, aşağıdaki yöntemler veya özellikler arasında belirsizdir**


-------------------------------------------------- ---------------------------------


'originalTypeInstance.ExtensionMethod()' aracılığıyla bir uzantı yöntemini çağırmanın sözdizimsel kolaylığının isteğe bağlı bir kolaylık olduğunu unutmayın. Yöntem ayrıca geleneksel şekilde çağrılabilir, böylece özel ilk parametre, yönteme parametre olarak kullanılır.

Yani, aşağıdaki çalışmaların her ikisi de:

    //Calling as though method belongs to string--it seamlessly extends string
    String s = "Hello World";
    s.Shorten(5);  
    
    //Calling as a traditional static method with two parameters
    StringExtensions.Shorten(s, 5);

[1]: https://www.wikiod.com/tr/docs/c%23/68/linq-queries

## Boş kontrol
Uzantı yöntemleri, örnek yöntemler gibi davranan statik yöntemlerdir. Ancak, bir "boş" başvuruda bir örnek yöntemi çağrılırken olanın aksine, bir "boş" başvuruyla bir uzantı yöntemi çağrıldığında, bir ['NullReferenceException'][1] atmaz. Bu, bazı senaryolarda oldukça yararlı olabilir.

Örneğin, aşağıdaki statik sınıfı göz önünde bulundurun:

    public static class StringExtensions
    {
        public static string EmptyIfNull(this string text)
        {
            return text ?? String.Empty;
        }

        public static string NullIfEmpty(this string text)
        {
            return String.Empty == text ? null : text;
        }
    }

<!-- ayrı -->

    string nullString = null;
    string emptyString = nullString.EmptyIfNull();// will return ""
    string anotherNullString = emptyString.NullIfEmpty(); // will return null

[.NET Fiddle'da Canlı Demo][2]

[1]: https://msdn.microsoft.com/en-us/library/system.nullreferenceexception(v=vs.110).aspx
[2]: https://dotnetfiddle.net/jNQWqg

## Açıkça bir uzatma yöntemi kullanma
Uzatma yöntemleri de sıradan statik sınıf yöntemleri gibi kullanılabilir. Bir uzantı yöntemini çağırmanın bu yolu daha ayrıntılıdır, ancak bazı durumlarda gereklidir.

    static class StringExtensions
    {
        public static string Shorten(this string text, int length) 
        {
            return text.Substring(0, length);
        }
    }

Kullanım:

    var newString = StringExtensions.Shorten("Hello World", 5);

# Uzantı yöntemleri ne zaman statik yöntemler olarak çağrılır

Statik bir yöntem olarak bir uzatma yöntemini kullanmanız gereken senaryolar hala vardır:

* Bir üye yöntemi ile çatışmayı çözme. Bu, bir kitaplığın yeni bir sürümü aynı imzaya sahip yeni bir üye yöntemi sunarsa gerçekleşebilir. Bu durumda üye yöntemi derleyici tarafından tercih edilecektir.
* Aynı imzaya sahip başka bir uzantı yöntemiyle çakışmaları çözme. Bu, iki kitaplık benzer uzantı yöntemleri içeriyorsa ve her iki sınıfın uzantı yöntemlerine sahip ad alanları aynı dosyada kullanılıyorsa gerçekleşebilir.
* Uzantı yöntemini bir yöntem grubu olarak delege parametresine geçirme.
* 'Yansıma' ile kendi ciltlemenizi yapmak.
* Visual Studio'daki Immediate penceresindeki uzatma yöntemini kullanma.

# Statik kullanma

Statik bir sınıfın statik üyelerini global kapsama getirmek için bir "using static" yönergesi kullanılırsa, uzatma yöntemleri atlanır. Örnek:

    using static OurNamespace.StringExtensions; // refers to class in previous example

    // OK: extension method syntax still works.
    "Hello World".Shorten(5);
    // OK: static method syntax still works.
    OurNamespace.StringExtensions.Shorten("Hello World", 5);
    // Compile time error: extension methods can't be called as static without specifying class.
    Shorten("Hello World", 5);

"Shorten" yönteminin ilk argümanından "this" değiştiricisini kaldırırsanız, son satır derlenir.


## Uzantı yöntemleri, genişletilmiş sınıfın yalnızca genel (veya dahili) üyelerini görebilir
    public class SomeClass
    {
        public void DoStuff()
        {
            
        }

        protected void DoMagic()
        {
            
        }
    }

    public static class SomeClassExtensions
    {
        public static void DoStuffWrapper(this SomeClass someInstance)
        {
            someInstance.DoStuff(); // ok
        }

        public static void DoMagicWrapper(this SomeClass someInstance)
        {
            someInstance.DoMagic(); // compilation error
        }
    }

Uzatma yöntemleri yalnızca sözdizimsel bir şekerdir ve aslında genişlettikleri sınıfın üyeleri değildir. Bu, kapsüllemeyi kıramayacakları anlamına gelir; yalnızca "genel" (veya aynı derlemede, "dahili" uygulandığında) alanlara, özelliklere ve yöntemlere erişimleri vardır.

## Genel Uzantı Yöntemleri
Diğer yöntemler gibi, uzatma yöntemleri de jenerik kullanabilir. Örneğin:

    static class Extensions
    {
        public static bool HasMoreThanThreeElements<T>(this IEnumerable<T> enumerable)
        {
            return enumerable.Take(4).Count() > 3;
        }
    }
ve çağırmak şöyle olurdu:

    IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
    var hasMoreThanThreeElements = numbers.HasMoreThanThreeElements();

[Demoyu Görüntüle][1]

Aynı şekilde, birden çok Tür Bağımsız Değişkeni için:

    public static TU GenericExt<T, TU>(this T obj)
    {
         TU ret = default(TU);
         // do some stuff with obj
         return ret;
    }

Çağırmak şöyle olurdu:

    IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
    var result = numbers.GenericExt<IEnumerable<int>,String>();

[Demoyu Görüntüle][2]

Çoklu genel türlerde kısmen bağlı türler için de uzantı yöntemleri oluşturabilirsiniz:

    class MyType<T1, T2>
    {
    }
    
    static class Extensions
    {
        public static void Example<T>(this MyType<int, T> test)
        {        
        }
    }

Çağırmak şöyle olurdu:

    MyType<int, string> t = new MyType<int, string>();
    t.Example();

[Demoyu Görüntüle][4]

[`where`][3] ile tür kısıtlamaları da belirtebilirsiniz:

    public static bool IsDefault<T>(this T obj) where T : struct, IEquatable<T>
    {
         return EqualityComparer<T>.Default.Equals(obj, default(T));
    }

Arama kodu:

    int number = 5;
    var IsDefault = number.IsDefault();

[Demoyu Görüntüle][5]


[1]: https://dotnetfiddle.net/UlCa3i
[2]: https://dotnetfiddle.net/aMNO0X
[3]: https://www.wikiod.com/tr/docs/c%23/26/keywords/8137/where-type-constraints#t=201607221442171394675
[4]: https://dotnetfiddle.net/1FjUOH
[5]: https://dotnetfiddle.net/Jom3cS

## Zincirleme için uzatma yöntemleri
Bir uzantı yöntemi, "this" argümanıyla aynı türde bir değer döndürdüğünde, bir veya daha fazla yöntem çağrısını uyumlu bir imzayla "zincirlemek" için kullanılabilir. Bu, mühürlü ve/veya ilkel türler için yararlı olabilir ve yöntem adları doğal insan dili gibi okunursa "akıcı" API'lerin oluşturulmasına izin verir.

    void Main()
    {
        int result = 5.Increment().Decrement().Increment(); 
        // result is now 6
    }
    
    public static class IntExtensions 
    {
        public static int Increment(this int number) {
            return ++number;
        }

        public static int Decrement(this int number) {
            return --number;
        }
    }

ya da bunun gibi

    void Main()
    {
        int[] ints = new[] { 1, 2, 3, 4, 5, 6};
        int[] a = ints.WhereEven();
        //a is { 2, 4, 6 };
        int[] b = ints.WhereEven().WhereGreaterThan(2);
        //b is { 4, 6 };
    }
    
    public static class IntArrayExtensions
    {
        public static int[] WhereEven(this int[] array)
        {
            //Enumerable.* extension methods use a fluent approach
            return array.Where(i => (i%2) == 0).ToArray();
        }
    
        public static int[] WhereGreaterThan(this int[] array, int value)
        {
            return array.Where(i => i > value).ToArray();
        }
    }

## Numaralandırma ile uzatma yöntemleri
Uzantı yöntemleri, numaralandırmalara işlevsellik eklemek için kullanışlıdır.

Yaygın bir kullanım, bir dönüştürme yöntemi uygulamaktır.

    public enum YesNo
    {
        Yes,
        No,
    }
    
    public static class EnumExtentions
    {
        public static bool ToBool(this YesNo yn)
        {
            return yn == YesNo.Yes;
        }
        public static YesNo ToYesNo(this bool yn)
        {
            return yn ? YesNo.Yes : YesNo.No;
        }
    }

Artık enum değerinizi hızlı bir şekilde farklı bir türe dönüştürebilirsiniz. Bu durumda bir bool.

    bool yesNoBool = YesNo.Yes.ToBool(); // yesNoBool == true
    YesNo yesNoEnum = false.ToYesNo();   // yesNoEnum == YesNo.No


Alternatif olarak, yöntemler gibi özellik eklemek için uzatma yöntemleri kullanılabilir.

    public enum Element
    {
        Hydrogen,
        Helium,
        Lithium,
        Beryllium,
        Boron,
        Carbon,
        Nitrogen,
        Oxygen
        //Etc
    }

    public static class ElementExtensions
    {
        public static double AtomicMass(this Element element)
        {
            switch(element)
            {
                case Element.Hydrogen:  return 1.00794;
                case Element.Helium:    return 4.002602;
                case Element.Lithium:   return 6.941;
                case Element.Beryllium: return 9.012182;
                case Element.Boron:     return 10.811;
                case Element.Carbon:    return 12.0107;
                case Element.Nitrogen:  return 14.0067;
                case Element.Oxygen:    return 15.9994;
                //Etc
            }
            return double.Nan;
        }
    }

    var massWater = 2*Element.Hydrogen.AtomicMass() + Element.Oxygen.AtomicMass();

## Statik türe göre uzantı yöntemleri gönderimi
Parametreleri eşleştirmek için dinamik (çalışma zamanı türü) yerine statik (derleme zamanı) türü kullanılır.

    public class Base 
    { 
        public virtual string GetName()
        {
            return "Base";
        }
    }

    public class Derived : Base
    { 
        public override string GetName()
        {
            return "Derived";
        }
    }

    public static class Extensions
    {
        public static string GetNameByExtension(this Base item)
        {
            return "Base";
        }

        public static string GetNameByExtension(this Derived item)
        {
            return "Derived";
        }
    }

    public static class Program   
    {
        public static void Main()
        {
            Derived derived = new Derived();
            Base @base = derived;

            // Use the instance method "GetName"
            Console.WriteLine(derived.GetName()); // Prints "Derived"
            Console.WriteLine(@base.GetName()); // Prints "Derived"

            // Use the static extension method "GetNameByExtension"
            Console.WriteLine(derived.GetNameByExtension()); // Prints "Derived"
            Console.WriteLine(@base.GetNameByExtension()); // Prints "Base"
        }
    }
[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/7BGp8o)

Ayrıca statik türe dayalı gönderme, bir "dinamik" nesne üzerinde bir uzantı yönteminin çağrılmasına izin vermez:

    public class Person
    {
        public string Name { get; set; }
    }
    
    public static class ExtenionPerson
    {
        public static string GetPersonName(this Person person)
        {
            return person.Name;
        }
    }
    
    dynamic person = new Person { Name = "Jon" };
    var name = person.GetPersonName(); // RuntimeBinderException is thrown

## Arayüzlerde Uzantı yöntemleri
Uzantı yöntemlerinin kullanışlı bir özelliği, bir arabirim için ortak yöntemler oluşturabilmenizdir. Normalde bir arabirimin paylaşılan uygulamaları olamaz, ancak uzantı yöntemleriyle yapabilirler.

    public interface IVehicle
    {
        int MilesDriven { get; set; }
    }
    
    public static class Extensions
    {
        public static int FeetDriven(this IVehicle vehicle)
        {
            return vehicle.MilesDriven * 5028;
        }
    }

Bu örnekte, "FeetDriven" yöntemi herhangi bir "IVhicle" üzerinde kullanılabilir. Bu yöntemdeki bu mantık tüm 'IVhicle'lar için geçerli olacaktır, böylece tüm çocuklar için aynı şekilde uygulanacak 'IVhicle' tanımında 'FeetDriven' olması gerekmemesi için bu şekilde yapılabilir. .

## Uzantı yöntemleri dinamik kod tarafından desteklenmez.

    static class Program
    {
        static void Main()
        {
            dynamic dynamicObject = new ExpandoObject();
    
            string awesomeString = "Awesome";
    
            // Prints True
            Console.WriteLine(awesomeString.IsThisAwesome());
    
            dynamicObject.StringValue = awesomeString;
    
            // Prints True
            Console.WriteLine(StringExtensions.IsThisAwesome(dynamicObject.StringValue)); 
            
            // No compile time error or warning, but on runtime throws RuntimeBinderException
            Console.WriteLine(dynamicObject.StringValue.IsThisAwesome());
        }
    }
    
    static class StringExtensions
    {
        public static bool IsThisAwesome(this string value)
        {
            return value.Equals("Awesome");
        }
    }

> [Dinamik koddan uzantı yöntemlerini çağırmanın] çalışmamasının nedeni, normal, dinamik olmayan kod uzantısı yöntemlerinde, derleyici tarafından bilinen tüm sınıflarda, bir uzantı yöntemine sahip statik bir sınıf için tam arama yaparak çalışmasıdır. maçlar. Arama, ad alanı iç içe yerleşimine ve her bir ad alanındaki mevcut 'kullanma' yönergelerine göre sıralanır.
> 
> Bu, bir dinamik uzantı yöntemi çağrısının doğru bir şekilde çözümlenmesini sağlamak için, DLR'nin bir şekilde *çalışma zamanında* kaynak kodunuzda* tüm ad alanı yuvalamalarının ve "kullanma" yönergelerinin ne olduğunu * bilmesi gerektiği anlamına gelir. Tüm bu bilgileri çağrı sitesine kodlamak için kullanışlı bir mekanizmamız yok. Böyle bir mekanizma icat etmeyi düşündük, ancak bunun çok yüksek maliyetli olduğuna ve buna değmeyecek kadar çok program riski ürettiğine karar verdik.

[Kaynak](http://stackoverflow.com/a/5313149/1610754)

## Arabirimlerle birlikte genişletme yöntemleri
Uygulama sınıfın dışında depolanabildiğinden ve sınıfa bazı işlevler eklemek için gereken tek şey, sınıfı arabirimle süslemek olduğundan, arabirimlerle genişletme yöntemlerini kullanmak çok uygundur.

    public interface IInterface
    {
       string Do()
    }

    public static class ExtensionMethods{
        public static string DoWith(this IInterface obj){
          //does something with IInterface instance
        }
    }

    public class Classy : IInterface
    {
       // this is a wrapper method; you could also call DoWith() on a Classy instance directly,
       // provided you import the namespace containing the extension method
       public Do(){
           return this.DoWith();
       }
    }


gibi kullanın:

     var classy = new Classy();
     classy.Do(); // will call the extension
     classy.DoWith(); // Classy implements IInterface so it can also be called this way

## Uzantılar ve arayüzler birlikte DRY kodu ve mixin benzeri işlevsellik sağlar
Uzantı yöntemleri, arabirimin kendisine yalnızca gerekli temel işlevleri dahil ederek ve kolaylık yöntemlerini ve aşırı yüklemeleri uzantı yöntemleri olarak tanımlamanıza izin vererek arabirim tanımlarınızı basitleştirmenizi sağlar. Daha az metoda sahip arayüzlerin yeni sınıflarda uygulanması daha kolaydır. Aşırı yüklemeleri arayüze eklemek yerine uzantılar olarak tutmak, sizi standart kodu her uygulamaya kopyalamaktan kurtararak kodunuzu KURU tutmanıza yardımcı olur. Bu aslında C#'ın desteklemediği mixin modeline benzer.

`System.Linq.Enumerable`ın `IEnumerable<T>` uzantıları buna harika bir örnektir. "IEnumerable<T>" yalnızca uygulayıcı sınıfın iki yöntemi uygulamasını gerektirir: genel ve genel olmayan "GetEnumerator()". Ancak "System.Linq.Enumerable", "IEnumerable<T>" öğesinin kısa ve net tüketimini sağlayan uzantılar olarak sayısız yararlı yardımcı program sağlar.

Aşağıdaki, uzantılar olarak sağlanan kolaylık aşırı yüklemeleri ile çok basit bir arayüzdür.

    public interface ITimeFormatter
    {
       string Format(TimeSpan span);
    }

    public static class TimeFormatter
    {
        // Provide an overload to *all* implementers of ITimeFormatter.
        public static string Format(
            this ITimeFormatter formatter,
            int millisecondsSpan)
            => formatter.Format(TimeSpan.FromMilliseconds(millisecondsSpan));
    }

    // Implementations only need to provide one method. Very easy to
    // write additional implementations.
    public class SecondsTimeFormatter : ITimeFormatter
    {
       public string Format(TimeSpan span)
       {
           return $"{(int)span.TotalSeconds}s";
       }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var formatter = new SecondsTimeFormatter();
            // Callers get two method overloads!
            Console.WriteLine($"4500ms is rougly {formatter.Format(4500)}");
            var span = TimeSpan.FromSeconds(5);
            Console.WriteLine($"{span} is formatted as {formatter.Format(span)}");
        }
    }


## IList<T> Uzantı Yöntemi Örneği: 2 Listeyi Karşılaştırma
Aynı türden iki IList< T > örneğinin içeriğini karşılaştırmak için aşağıdaki uzantı yöntemini kullanabilirsiniz.

Varsayılan olarak, öğeler listedeki sıralarına ve öğelerin kendilerine göre karşılaştırılır, 'isOrdered' parametresine false iletmek, sıralarına bakılmaksızın yalnızca öğelerin kendilerini karşılaştırır.

Bu yöntemin çalışması için, genel türün ("T") hem "Equals" hem de "GetHashCode" yöntemlerini geçersiz kılması gerekir.

**Kullanım:**

    List<string> list1 = new List<string> {"a1", "a2", null, "a3"};
    List<string> list2 = new List<string> {"a1", "a2", "a3", null};

    list1.Compare(list2);//this gives false
    list1.Compare(list2, false);//this gives true. they are equal when the order is disregarded

**Yöntem:**

    public static bool Compare<T>(this IList<T> list1, IList<T> list2, bool isOrdered = true) 
    {
        if (list1 == null && list2 == null)
            return true;
        if (list1 == null || list2 == null || list1.Count != list2.Count)
            return false;

        if (isOrdered)
        {
            for (int i = 0; i < list2.Count; i++)
            {
                var l1 = list1[i]; 
                var l2 = list2[i];
                if (
                     (l1 == null && l2 != null) || 
                     (l1 != null && l2 == null) || 
                     (!l1.Equals(l2)))
                {
                        return false;
                }
            }
            return true;
        }
        else
        {
            List<T> list2Copy = new List<T>(list2);
            //Can be done with Dictionary without O(n^2)
            for (int i = 0; i < list1.Count; i++)
            {
                if (!list2Copy.Remove(list1[i]))
                    return false;
            }
            return true;
        }
    }

## Kesin olarak yazılan sarmalayıcılar olarak uzatma yöntemleri
Genişletme yöntemleri, sözlük benzeri nesneler için kesin olarak yazılan sarmalayıcılar yazmak için kullanılabilir. Örneğin, cetera'da bir önbellek, `HttpContext.Items'...

    public static class CacheExtensions
    {
        public static void SetUserInfo(this Cache cache, UserInfo data) => 
            cache["UserInfo"] = data;

        public static UserInfo GetUserInfo(this Cache cache) => 
            cache["UserInfo"] as UserInfo;
    }

Bu yaklaşım, kod tabanının tamamında anahtar olarak dize değişmezlerini kullanma ihtiyacını ve okuma işlemi sırasında gerekli türe döküm yapma ihtiyacını ortadan kaldırır. Genel olarak, Sözlükler gibi gevşek yazılmış nesnelerle etkileşim kurmanın daha güvenli, güçlü bir şekilde yazılmış bir yolunu yaratır.

## Güzel eşleştirici sınıfları oluşturmak için Uzantı yöntemlerini kullanma
Uzantı yöntemleriyle daha iyi bir eşleyici sınıfları oluşturabiliriz,
Diyelim ki bazı DTO sınıflarım varsa

     public class UserDTO
     {
            public AddressDTO Address { get; set; }
     }
    
     public class AddressDTO
     {
            public string Name { get; set; }
     }

ve karşılık gelen görünüm modeli sınıflarına eşlemem gerekiyor

    public class UserViewModel
    {
        public AddressViewModel Address { get; set; }
    }
    
    public class AddressViewModel
    {
        public string Name { get; set; }
    }

sonra aşağıdaki gibi mapper sınıfımı oluşturabilirim

    public static class ViewModelMapper
    {
          public static UserViewModel ToViewModel(this UserDTO user)
          {
                return user == null ?
                    null :
                    new UserViewModel()
                    {
                        Address = user.Address.ToViewModel()
                        // Job = user.Job.ToViewModel(),
                        // Contact = user.Contact.ToViewModel() .. and so on
                    };
          }
    
          public static AddressViewModel ToViewModel(this AddressDTO userAddr)
          {
                return userAddr == null ?
                    null :
                    new AddressViewModel()
                    {
                        Name = userAddr.Name
                    };
          }
    }

Sonra nihayet haritamı aşağıdaki gibi çağırabilirim

        UserDTO userDTOObj = new UserDTO() {
                Address = new AddressDTO() {
                    Name = "Address of the user"
                }
            };

        UserViewModel user = userDTOObj.ToViewModel(); // My DTO mapped to Viewmodel


Buradaki güzellik, tüm haritalama yöntemlerinin ortak bir ada sahip olmasıdır (ToViewModel) ve bunu birkaç şekilde yeniden kullanabiliriz.

## Yeni koleksiyon türleri oluşturmak için Uzantı yöntemlerini kullanma (ör. DictList)
"Liste<T>" değerine sahip "Sözlük" gibi iç içe geçmiş koleksiyonlar için kullanılabilirliği geliştirmek için uzantı yöntemleri oluşturabilirsiniz.

Aşağıdaki uzatma yöntemlerini göz önünde bulundurun:

    public static class DictListExtensions
    {
        public static void Add<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
                where TCollection : ICollection<TValue>, new()
        {
            TCollection list;
            if (!dict.TryGetValue(key, out list))
            {
                list = new TCollection();
                dict.Add(key, list);
            }

            list.Add(value);
        }

        public static bool Remove<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
            where TCollection : ICollection<TValue>
        {
            TCollection list;
            if (!dict.TryGetValue(key, out list))
            {
                return false;
            }

            var ret = list.Remove(value);
            if (list.Count == 0)
            {
                dict.Remove(key);
            }
            return ret;
        }
    }

uzatma yöntemlerini aşağıdaki gibi kullanabilirsiniz:

    var dictList = new Dictionary<string, List<int>>();

    dictList.Add("example", 5);
    dictList.Add("example", 10);
    dictList.Add("example", 15);
    
    Console.WriteLine(String.Join(", ", dictList["example"])); // 5, 10, 15

    dictList.Remove("example", 5);
    dictList.Remove("example", 10);
    
    Console.WriteLine(String.Join(", ", dictList["example"])); // 15
    
    dictList.Remove("example", 15);
    
    Console.WriteLine(dictList.ContainsKey("example")); // False

[Demoyu Görüntüle](https://dotnetfiddle.net/UbdQuC)

## Özel durumları işlemek için uzatma yöntemleri

Uzantı yöntemleri, aksi takdirde bir çağrı işlevini if/then ifadeleriyle karıştırmayı gerektirecek uygun olmayan iş kurallarının işlenmesini "gizlemek" için kullanılabilir. Bu, boş değerlerin uzantı yöntemleriyle işlenmesine benzer ve benzerdir. Örneğin,

    public static class CakeExtensions
    {
        public static Cake EnsureTrueCake(this Cake cake)
        {
            //If the cake is a lie, substitute a cake from grandma, whose cakes aren't as tasty but are known never to be lies. If the cake isn't a lie, don't do anything and return it.
            return CakeVerificationService.IsCakeLie(cake) ? GrandmasKitchen.Get1950sCake() : cake;
        }
    }

<!-- ayrı -->

    Cake myCake = Bakery.GetNextCake().EnsureTrueCake();
    myMouth.Eat(myCake);//Eat the cake, confident that it is not a lie.


## Statik yöntemlerle ve Geri Aramalarla Uzantı yöntemlerini kullanma
Uzantı Yöntemlerini, diğer kodu saran İşlevler olarak kullanmayı düşünün; Try Catch yapısını sarmak için hem statik bir yöntem hem de uzatma yöntemini kullanan harika bir örnek. Kodunuzu Kurşun Geçirmez Yapın...

    using System;
    using System.Diagnostics;
    
    namespace Samples
    {
        /// <summary>
        /// Wraps a try catch statement as a static helper which uses 
        /// Extension methods for the exception
        /// </summary>
        public static class Bullet
        {
            /// <summary>
            /// Wrapper for Try Catch Statement
            /// </summary>
            /// <param name="code">Call back for code</param>
            /// <param name="error">Already handled and logged exception</param>
            public static void Proof(Action code, Action<Exception> error)
            {
                try
                {
                    code();
                }
                catch (Exception iox)
                {
                    //extension method used here
                    iox.Log("BP2200-ERR-Unexpected Error");
                    //callback, exception already handled and logged
                    error(iox);
                }
            }
            /// <summary>
            /// Example of a logging method helper, this is the extension method
            /// </summary>
            /// <param name="error">The Exception to log</param>
            /// <param name="messageID">A unique error ID header</param>
            public static void Log(this Exception error, string messageID)
            {
                Trace.WriteLine(messageID);
                Trace.WriteLine(error.Message);
                Trace.WriteLine(error.StackTrace);
                Trace.WriteLine("");
            }
        }
        /// <summary>
        /// Shows how to use both the wrapper and extension methods.
        /// </summary>
        public class UseBulletProofing
        {
            public UseBulletProofing()
            {
                var ok = false;
                var result = DoSomething();
                if (!result.Contains("ERR"))
                {
                    ok = true;
                    DoSomethingElse();
                }
            }
    
            /// <summary>
            /// How to use Bullet Proofing in your code.
            /// </summary>
            /// <returns>A string</returns>
            public string DoSomething()
            {
                string result = string.Empty;
                //Note that the Bullet.Proof method forces this construct.
                Bullet.Proof(() =>
                {
                    //this is the code callback
                    result = "DST5900-INF-No Exceptions in this code";
                }, error =>
                {
                    //error is the already logged and handled exception
                    //determine the base result
                    result = "DTS6200-ERR-An exception happened look at console log";
                    if (error.Message.Contains("SomeMarker"))
                    {
                        //filter the result for Something within the exception message
                        result = "DST6500-ERR-Some marker was found in the exception";
                    }
                });
                return result;
            }
    
            /// <summary>
            /// Next step in workflow
            /// </summary>
            public void DoSomethingElse()
            {
                //Only called if no exception was thrown before
            }
        }
    }

