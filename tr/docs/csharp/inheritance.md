---
title: "Miras"
slug: "miras"
draft: false
images: []
weight: 9785
type: docs
toc: true
---

## Sözdizimi
- sınıf DerivedClass : TemelSınıf
- sınıf DerivedClass : BaseClass, IExampleInterface
- sınıf DerivedClass : BaseClass, IExampleInterface, IAnotherInterface

Sınıflar doğrudan yalnızca bir sınıftan miras alabilir, ancak (bunun yerine veya aynı anda) bir veya daha fazla arabirim uygulayabilir.

Yapılar arabirimleri uygulayabilir ancak herhangi bir türden açıkça miras alamaz. Bunlar örtük olarak 'System.ValueType'dan miras alırlar ve bu da doğrudan 'System.Object'den miras alır.

Statik sınıflar [cannot][1] arayüzleri uygulayamaz.


[1]: http://stackoverflow.com/a/259079

## Miras. Yapıcıların çağrı dizisi
"Köpek" alt sınıfına sahip bir "Hayvan" sınıfımız olduğunu düşünün.

    class Animal
    {
        public Animal()
        {
            Console.WriteLine("In Animal's constructor");
        }
    }
    
    class Dog : Animal
    {
        public Dog()
        {
            Console.WriteLine("In Dog's constructor");
        }
    }

Varsayılan olarak, her sınıf örtük olarak 'Object' sınıfını miras alır.

Bu, yukarıdaki kodla aynıdır.

    class Animal : Object
    {
        public Animal()
        {
            Console.WriteLine("In Animal's constructor");
        }
    }

"Dog" sınıfının bir örneğini oluştururken, üst sınıfta başka bir kurucuya açık bir çağrı yoksa **temel sınıfların varsayılan kurucusu (parametresiz) çağrılır**. Bizim durumumuzda, önce 'Object' yapıcısı, sonra 'Animal's' ve sonunda da 'Dog's' yapıcısı olarak adlandırılacaktır.

    public class Program
    {
        public static void Main()
        {
            Dog dog = new Dog();
        }
    }

Çıktı olacak
> Animal'ın yapıcısında
> Dog'un yapıcısında

[Demoyu Görüntüle][1]

**Üst öğenin yapıcısını açıkça arayın.**

Yukarıdaki örneklerde, "Köpek" sınıfı kurucumuz "Animal" sınıfının **varsayılan** kurucusunu çağırır. İsterseniz hangi kurucunun çağrılacağını belirleyebilirsiniz: üst sınıfta tanımlanmış herhangi bir kurucuyu çağırmak mümkündür.

Bu iki sınıfımız olduğunu düşünün.

    class Animal
    {
        protected string name;
    
        public Animal()
        {
            Console.WriteLine("Animal's default constructor");
        }    
    
        public Animal(string name)
        {
            this.name = name;
            Console.WriteLine("Animal's constructor with 1 parameter");
            Console.WriteLine(this.name);
        }
    }

    class Dog : Animal
    {
        public Dog() : base()
        {
            Console.WriteLine("Dog's default constructor");
        }  
    
        public Dog(string name) : base(name)
        {
            Console.WriteLine("Dog's constructor with 1 parameter");
            Console.WriteLine(this.name);
        }
    }

**Burada neler oluyor?**

Her sınıfta 2 kurucumuz var.

**"Temel" ne anlama geliyor?**

"base", ana sınıfa bir referanstır. Bizim durumumuzda, bunun gibi bir 'Köpek' sınıfı örneği oluşturduğumuzda

    Dog dog = new Dog();

Çalışma zamanı önce parametresiz kurucu olan `Köpek()`i çağırır. Ama vücudu hemen çalışmıyor. Yapıcının parantezlerinden sonra şöyle bir çağrımız var: 'base()', bu, varsayılan 'Köpek' yapıcısını çağırdığımızda, bunun da ebeveynin **varsayılan** yapıcısını çağıracağı anlamına gelir. Ebeveynin yapıcısı çalıştıktan sonra geri dönecek ve son olarak `Dog()` kurucu gövdesini çalıştıracaktır.

Yani çıktı şöyle olacaktır:
> Animal'ın varsayılan kurucusu
>Köpeğin varsayılan yapıcısı

[Demoyu Görüntüle][2]

**Şimdi, "Dog's" yapıcısını bir parametre ile çağırırsak ne olur?**

    Dog dog = new Dog("Rex");

Üst sınıftaki private olmayan üyelerin alt sınıf tarafından miras alındığını biliyorsunuz, yani 'Köpek' aynı zamanda 'ad' alanına da sahip olacaktır.
Bu durumda, yapıcımıza bir argüman ilettik. O da argümanı, 'name' alanını başlatan bir parametreyle** üst sınıf **kurucuya iletir.

Çıktı olacak

<!-- dil: lang-none -->
    Animal's constructor with 1 parameter
    Rex
    Dog's constructor with 1 parameter
    Rex

**Özet:**

Her nesne oluşturma, temel sınıftan başlar. Kalıtımda hiyerarşide olan sınıflar zincirlenir. Tüm sınıflar 'Object'ten türetildiği için, herhangi bir nesne yaratıldığında çağrılacak ilk kurucu 'Object' sınıf kurucusudur; Ardından zincirdeki bir sonraki kurucu çağrılır ve ancak hepsi çağrıldıktan sonra nesne oluşturulur.

**temel anahtar kelime**

1) Base anahtar sözcüğü, türetilmiş bir sınıf içinden temel sınıfın üyelerine erişmek için kullanılır:
2) Başka bir yöntemle geçersiz kılınan temel sınıfta bir yöntemi çağırın.
Türetilmiş sınıfın örneklerini oluştururken hangi temel sınıf oluşturucusunun çağrılması gerektiğini belirtin.


[1]: https://dotnetfiddle.net/uOL8cE
[2]: https://dotnetfiddle.net/eRKEjT

## Bir temel sınıftan miras alma
Kodun kopyalanmasını önlemek için, genel bir sınıfta ortak yöntemleri ve öznitelikleri temel olarak tanımlayın:

    public class Animal 
    {
        public string Name { get; set; }
        // Methods and attributes common to all animals
        public void Eat(Object dinner)
        {
            // ...
        }
        public void Stare()
        {
            // ...
        }
        public void Roll()
        {
            // ...
        }
    }
  
Artık genel olarak 'Hayvan'ı temsil eden bir sınıfınız olduğuna göre, belirli hayvanların özelliklerini tanımlayan bir sınıf tanımlayabilirsiniz:
  
    public class Cat : Animal
    {
        public Cat() 
        {
            Name = "Cat";
        }
        // Methods for scratching furniture and ignoring owner
        public void Scratch(Object furniture)
        {
            // ...
        }
    }

Cat sınıfı, yalnızca tanımında açıkça açıklanan yöntemlere değil, aynı zamanda genel "Animal" temel sınıfında tanımlanan tüm yöntemlere de erişim sağlar. Herhangi bir Hayvan (Kedi olsun ya da olmasın) Yiyebilir, Bakabilir veya Yuvarlanabilir. Bununla birlikte, bir Hayvan, aynı zamanda bir Kedi olmadıkça, Kaşıyamazdı. Daha sonra diğer hayvanları tanımlayan başka sınıflar tanımlayabilirsiniz. (Çiçek bahçelerini yok etme yöntemiyle Gopher ve hiçbir ekstra yöntem olmadan Tembellik gibi.)

## Bir sınıftan miras alma ve bir arabirim uygulama
    public class Animal 
    {
        public string Name { get; set; }
    }

    public interface INoiseMaker
    {
        string MakeNoise();
    }

    //Note that in C#, the base class name must come before the interface names
    public class Cat : Animal, INoiseMaker
    {
        public Cat() 
        {
            Name = "Cat";
        }

        public string MakeNoise()
        {
            return "Nyan";
        }
    }



## Bir sınıftan miras alma ve birden çok arabirim uygulama
    public class LivingBeing
    {
        string Name { get; set; }
    }
    
    public interface IAnimal 
    {
        bool HasHair { get; set; }
    }
    
    public interface INoiseMaker
    {
        string MakeNoise();
    }
    
    //Note that in C#, the base class name must come before the interface names
    public class Cat : LivingBeing, IAnimal, INoiseMaker
    {
        public Cat() 
        {
            Name = "Cat";
            HasHair = true;
        }
    
        public bool HasHair { get; set; }
    
        public string Name { get; set; }

        public string MakeNoise()
        {
            return "Nyan";
        }
    }

## Kalıtımı test etme ve gezinme
    interface BaseInterface {}
    class BaseClass : BaseInterface {}

    interface DerivedInterface {}
    class DerivedClass : BaseClass, DerivedInterface {}
    
    var baseInterfaceType = typeof(BaseInterface);
    var derivedInterfaceType = typeof(DerivedInterface);
    var baseType = typeof(BaseClass);
    var derivedType = typeof(DerivedClass);
    
    var baseInstance = new BaseClass();
    var derivedInstance = new DerivedClass();  
    
    Console.WriteLine(derivedInstance is DerivedClass); //True
    Console.WriteLine(derivedInstance is DerivedInterface); //True
    Console.WriteLine(derivedInstance is BaseClass); //True
    Console.WriteLine(derivedInstance is BaseInterface); //True
    Console.WriteLine(derivedInstance is object); //True
    
    Console.WriteLine(derivedType.BaseType.Name);  //BaseClass
    Console.WriteLine(baseType.BaseType.Name);  //Object
    Console.WriteLine(typeof(object).BaseType);  //null
    
    Console.WriteLine(baseType.IsInstanceOfType(derivedInstance));  //True
    Console.WriteLine(derivedType.IsInstanceOfType(baseInstance));  //False

    Console.WriteLine(
        string.Join(",", 
        derivedType.GetInterfaces().Select(t => t.Name).ToArray()));
    //BaseInterface,DerivedInterface
        
    Console.WriteLine(baseInterfaceType.IsAssignableFrom(derivedType)); //True
    Console.WriteLine(derivedInterfaceType.IsAssignableFrom(derivedType)); //True
    Console.WriteLine(derivedInterfaceType.IsAssignableFrom(baseType)); //False

## Soyut bir temel sınıfı genişletme
Uygulama sözleşmeleri olarak tanımlanabilecek arabirimlerin aksine, soyut sınıflar genişletme sözleşmeleri gibi davranır.

Soyut bir sınıf somutlaştırılamaz, genişletilmelidir ve sonuçta ortaya çıkan sınıf (veya türetilmiş sınıf) daha sonra somutlaştırılabilir.

Soyut sınıflar, genel uygulamalar sağlamak için kullanılır

    public abstract class Car
    {
        public void HonkHorn() {
            // Implementation of horn being honked
        }
    }

    public class Mustang : Car
    {
        // Simply by extending the abstract class Car, the Mustang can HonkHorn()
        // If Car were an interface, the HonkHorn method would need to be included
        // in every class that implemented it.
    }

Yukarıdaki örnek, Car'ı genişleten herhangi bir sınıfın, uygulama ile birlikte HonkHorn yöntemini otomatik olarak nasıl alacağını gösterir. Bu, yeni bir Araba yaratan herhangi bir geliştiricinin kornasını nasıl çalacağı konusunda endişelenmesine gerek olmayacağı anlamına gelir.

## Bir Alt Sınıftaki Yapıcılar
Bir temel sınıfın alt sınıfını oluşturduğunuzda, alt sınıf oluşturucunun parametrelerinden sonra ": taban" kullanarak temel sınıfı oluşturabilirsiniz.

    class Instrument
    {
        string type;
        bool clean;
    
        public Instrument (string type, bool clean)
        {
            this.type = type;
            this.clean = clean;
        }
    }
    
    class Trumpet : Instrument
    {
        bool oiled;
    
        public Trumpet(string type, bool clean, bool oiled) : base(type, clean)
        {
            this.oiled = oiled;
        }
    }

## Kalıtım Anti-kalıpları
# Uygunsuz Miras

Diyelim ki `Foo` ve `Bar` olmak üzere 2 sınıf var. 'Foo', 'Do1' ve 'Do2' olmak üzere iki özelliğe sahiptir. 'Bar'ın 'Foo'dan 'Do1' kullanması gerekiyor, ancak 'Do2'ye ihtiyacı yok veya 'Do2'ye eşdeğer ama tamamen farklı bir şey yapan bir özelliğe ihtiyacı var.

**Kötü yol**: 'Foo' sanal üzerinde 'Do2()' öğesini yapın ve ardından 'Bar'da geçersiz kılın veya 'Do2()' için 'Bar'da 'İstisnayı atın'

    public class Bar : Foo
    {
        public override void Do2()
        {
            //Does something completely different that you would expect Foo to do
            //or simply throws new Exception 
        }
    }

**İyi bir yol**

'Foo'dan 'Do1()' öğesini çıkarın ve yeni 'Baz' sınıfına koyun, ardından 'Baz'dan hem 'Foo' hem de 'Bar'ı devralın ve 'Do2()' öğesini ayrı olarak uygulayın

    public class Baz
    {
        public void Do1()
        {
            // magic
        }
    }

    public class Foo : Baz
    {
        public void Do2()
        {
            // foo way
        }
    }

    public class Bar : Baz
    {
        public void Do2()
        {
            // bar way or not have Do2 at all
        }
    }

Şimdi neden ilk örnek kötü ve ikinci örnek iyi: 2 numaralı geliştirici 'Foo'da bir değişiklik yapmak zorunda kaldığında, 'Bar'ın uygulamasını kıracaktır çünkü 'Bar' artık 'Foo'dan ayrılamaz. İkinci örnekte bunu yaparken 'Foo' ve 'Bar' ortaklığı 'Baz'a taşınmış ve birbirlerini etkilemiyorlar (olmaması gerektiği gibi).


## Miras alma yöntemleri
Yöntemlerin miras alınmasının birkaç yolu vardır.

    public abstract class Car
    {
        public void HonkHorn() {
            // Implementation of horn being honked
        }

        // virtual methods CAN be overridden in derived classes
        public virtual void ChangeGear() {
            // Implementation of gears being changed
        }

        // abstract methods MUST be overridden in derived classes
        public abstract void Accelerate();
    }

    public class Mustang : Car
    {
        // Before any code is added to the Mustang class, it already contains 
        // implementations of HonkHorn and ChangeGear.

        // In order to compile, it must be given an implementation of Accelerate,
        // this is done using the override keyword
        public override void Accelerate() {
            // Implementation of Mustang accelerating
        }

        // If the Mustang changes gears differently to the implementation in Car
        // this can be overridden using the same override keyword as above
        public override void ChangeGear() {
            // Implementation of Mustang changing gears
        }
    }

## Özyinelemeli tip belirtimi olan temel sınıf
Özyinelemeli tür belirteci ile genel bir temel sınıfın tek seferlik tanımı. Her düğümün bir ebeveyni ve birden fazla çocuğu vardır.

    /// <summary>
    /// Generic base class for a tree structure
    /// </summary>
    /// <typeparam name="T">The node type of the tree</typeparam>
    public abstract class Tree<T> where T : Tree<T>
    {
        /// <summary>
        /// Constructor sets the parent node and adds this node to the parent's child nodes
        /// </summary>
        /// <param name="parent">The parent node or null if a root</param>
        protected Tree(T parent)
        {
            this.Parent=parent;
            this.Children=new List<T>();
            if(parent!=null)
            {
                parent.Children.Add(this as T);
            }
        }
        public T Parent { get; private set; }
        public List<T> Children { get; private set; }
        public bool IsRoot { get { return Parent==null; } }
        public bool IsLeaf { get { return Children.Count==0; } }
        /// <summary>
        /// Returns the number of hops to the root object
        /// </summary>
        public int Level { get { return IsRoot ? 0 : Parent.Level+1; } }
    }

Yukarıdakiler, nesnelerin bir ağaç hiyerarşisinin tanımlanması gerektiğinde her zaman yeniden kullanılabilir. Ağaçtaki düğüm nesnesi, temel sınıftan şu şekilde miras almalıdır:

    public class MyNode : Tree<MyNode>
    {
        // stuff
    }

her düğüm sınıfı, hiyerarşide nerede olduğunu, ana nesnenin ne olduğunu ve alt nesnelerin ne olduğunu bilir. Birkaç yerleşik tür, "Kontrol" veya "XmlElement" gibi bir ağaç yapısı kullanır ve yukarıdaki "Ağaç<T>", kodunuzda _any_ türünün temel sınıfı olarak kullanılabilir.


----------


Örneğin, toplam ağırlığın tüm çocukların ağırlığından hesaplandığı bir parça hiyerarşisi oluşturmak için aşağıdakileri yapın:

    public class Part : Tree<Part>
    {
        public static readonly Part Empty = new Part(null) { Weight=0 };
        public Part(Part parent) : base(parent) { }
        public Part Add(float weight)
        {
            return new Part(this) { Weight=weight };
        }
        public float Weight { get; set; }

        public float TotalWeight { get { return Weight+Children.Sum((part) => part.TotalWeight); } }
    }

olarak kullanılmak

    // [Q:2.5] -- [P:4.2] -- [R:0.4]
    //    \
    //      - [Z:0.8]
    var Q = Part.Empty.Add(2.5f);
    var P = Q.Add(4.2f);
    var R = P.Add(0.4f);
    var Z = Q.Add(0.9f);
    
    // 2.5+(4.2+0.4)+0.9 = 8.0
    float weight = Q.TotalWeight;


----------


Başka bir örnek, göreli koordinat çerçevelerinin tanımında olacaktır. Bu durumda koordinat çerçevesinin gerçek konumu, ana koordinat çerçevelerinin _all_ konumlarına bağlıdır.

    public class RelativeCoordinate : Tree<RelativeCoordinate>
    {
        public static readonly RelativeCoordinate Start = new RelativeCoordinate(null, PointF.Empty) { };
        public RelativeCoordinate(RelativeCoordinate parent, PointF local_position)
            : base(parent)
        {
            this.LocalPosition=local_position;
        }
        public PointF LocalPosition { get; set; }
        public PointF GlobalPosition
        {
            get
            {
                if(IsRoot) return LocalPosition;
                var parent_pos = Parent.GlobalPosition;
                return new PointF(parent_pos.X+LocalPosition.X, parent_pos.Y+LocalPosition.Y);
            }
        }
        public float TotalDistance
        {
            get
            {
                float dist = (float)Math.Sqrt(LocalPosition.X*LocalPosition.X+LocalPosition.Y*LocalPosition.Y);
                return IsRoot ? dist : Parent.TotalDistance+dist;
            }
        }
        public RelativeCoordinate Add(PointF local_position)
        {
            return new RelativeCoordinate(this, local_position);
        }
        public RelativeCoordinate Add(float x, float y)
        {
            return Add(new PointF(x, y));
        }
    }

olarak kullanılmak

    // Define the following coordinate system hierarchy
    //
    // o--> [A1] --+--> [B1] -----> [C1]
    //             |     
    //             +--> [B2] --+--> [C2]
    //                         |
    //                         +--> [C3]
    
    var A1 = RelativeCoordinate.Start;
    var B1 = A1.Add(100, 20);
    var B2 = A1.Add(160, 10);
    
    var C1 = B1.Add(120, -40);
    var C2 = B2.Add(80, -20);
    var C3 = B2.Add(60, -30);
    
    double dist1 = C1.TotalDistance;



