---
title: "polimorfizm"
slug: "polimorfizm"
draft: false
images: []
weight: 9906
type: docs
toc: true
---

## Polimorfizm Türleri
Polimorfizm, bir işlemin diğer bazı türlerin değerlerine de uygulanabileceği anlamına gelir.

Birden çok Polimorfizm türü vardır:

- **Ad hoc polimorfizm:**
'işlev aşırı yüklemesini' içeriyor. Hedef, bir Yöntemin birlikte kullanılabilmesidir.
jenerik olmaya gerek kalmadan farklı türler.
- **Parametrik polimorfizm:**
jenerik tiplerin kullanılmasıdır. [Jenerikler][1]'e bakın
- **Alt tipleme:**
benzer bir işlevi genelleştirmek için bir sınıfın hedef mirasını alır

---------------

# Ad hoc polimorfizm #

Ad hoc polimorfizmin hedefi, işlev çağrısında veya jeneriklerde tür dönüştürmeye gerek kalmadan farklı veri türleri tarafından çağrılabilen bir yöntem oluşturmaktır. Aşağıdaki yöntem(ler) `sumInt(par1, par2)` farklı veri tipleri ile çağrılabilir ve her tip kombinasyonu için kendi uygulamasına sahiptir:


    public static int sumInt( int a, int b)
    {
        return a + b;    
    }
    
    public static int sumInt( string a, string b)
    {
        int _a, _b;
        
        if(!Int32.TryParse(a, out _a))
            _a = 0;
        
        if(!Int32.TryParse(b, out _b))
            _b = 0;
        
        return _a + _b;
    }
    
    public static int sumInt(string a, int b)
    {
        int _a;
        
        if(!Int32.TryParse(a, out _a))
            _a = 0;    
        
        return _a + b;
    }
    
    public static int sumInt(int a, string b)
    {        
        return sumInt(b,a);
    }

İşte örnek bir çağrı:


    public static void Main()
    {
        Console.WriteLine(sumInt( 1 , 2 ));  //  3
        Console.WriteLine(sumInt("3","4"));  //  7
        Console.WriteLine(sumInt("5", 6 ));  // 11
        Console.WriteLine(sumInt( 7 ,"8"));  // 15
    }

------

# Alt tipleme #

Alt tipleme, benzer bir davranışı genelleştirmek için temel sınıftan devralmanın kullanılmasıdır:

    public interface Car{
        void refuel();
    }
    
    public class NormalCar : Car
    {
        public void refuel()
        {
            Console.WriteLine("Refueling with petrol");    
        }
    }
    
    public class ElectricCar : Car
    {
        public void refuel()
        {
            Console.WriteLine("Charging battery");    
        }
    }

Her iki 'NormalCar' ve 'ElectricCar' sınıfının artık yakıt ikmali için bir yöntemi var, ancak kendi uygulamaları. İşte bir Örnek:


    public static void Main()
    {
        List<Car> cars = new List<Car>(){
            new NormalCar(),
            new ElectricCar()
        };
        
        cars.ForEach(x => x.refuel());
    }

Çıktı aşağıdaki olacaktır:

> Benzin ile yakıt ikmali
Bataryayı şarj etmek

[1]: https://www.wikiod.com/tr/docs/c%23/27/generics

## Başka Bir Polimorfizm Örneği
Polimorfizm, OOP'nin temellerinden biridir. Poly, 'çoklu biçimler' anlamına gelen Yunanca bir terimden türemiştir.

Aşağıda Polimorfizm sergileyen bir örnek verilmiştir. "Araç" sınıfı, temel sınıf olarak birden çok biçim alır.

Türetilmiş sınıflar "Ducati" ve "Lamborghini", "Vehicle" öğesinden miras alır ve kendi "NumberOfWheels" öğesini görüntülemek için temel sınıfın "Display()" yöntemini geçersiz kılar.


    public class Vehicle
    {
        protected int NumberOfWheels { get; set; } = 0;
        public Vehicle()
        {
        }

        public virtual void Display()
        {
            Console.WriteLine($"The number of wheels for the {nameof(Vehicle)} is {NumberOfWheels}");
        }
    }

    public class Ducati : Vehicle
    {
        public Ducati()
        {
            NoOfWheels = 2;
        }

        public override void Display()
        {
            Console.WriteLine($"The number of wheels for the {nameof(Ducati)} is {NumberOfWheels}");
        }
    }

    public class Lamborghini : Vehicle
    {
        public Lamborghini()
        {
            NoOfWheels = 4;
        }

        public override void Display()
        {
            Console.WriteLine($"The number of wheels for the {nameof(Lamborghini)} is {NumberOfWheels}");
        }
    }

Aşağıda Polimorfizmin sergilendiği kod parçacığı yer almaktadır. Nesne, Satır 1'de bir 'araç' değişkeni kullanılarak 'Araç' temel türü için oluşturulur. Satır 2'de 'Display()' temel sınıf yöntemini çağırır ve çıktıyı gösterildiği gibi görüntüler.

     static void Main(string[] args)
     {
        Vehicle vehicle = new Vehicle();    //Line 1
        vehicle.Display();                  //Line 2  
        vehicle = new Ducati();             //Line 3
        vehicle.Display();                  //Line 4
        vehicle = new Lamborghini();        //Line 5
        vehicle.Display();                  //Line 6
     }

3. Satırda, "araç" nesnesi türetilmiş "Ducati" sınıfına yönlendirilir ve çıktıyı gösterildiği gibi görüntüleyen "Display()" yöntemini çağırır. Burada polimorfik davranış gelir, "araç" nesnesi "Araç" türünde olsa bile, "Ducati" türü temel sınıf "Display()" yöntemini geçersiz kıldığından türetilmiş sınıf yöntemini "Display()" olarak adlandırır. "araç" nesnesi "Ducati"ye doğru yönlendirildi.

Aynı açıklama, "Lamborghini" türünün "Display()" yöntemini çağırdığında da geçerlidir.

Çıktı aşağıda gösterilmiştir
    
    The number of wheels for the Vehicle is 0        // Line 2 
    The number of wheels for the Ducati is 2         // Line 4
    The number of wheels for the Lamborghini is 4    // Line 6
    

 

