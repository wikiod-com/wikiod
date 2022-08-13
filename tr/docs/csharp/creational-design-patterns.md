---
title: "Yaratıcı Tasarım Desenleri"
slug: "yaratc-tasarm-desenleri"
draft: false
images: []
weight: 9850
type: docs
toc: true
---

Yaratılış kalıpları, bir sistemi nesnelerinin oluşturulma, oluşturulma ve temsil edilme biçiminden ayırmayı amaçlar. Ne, kim, nasıl ve ne zaman nesne oluşturma açısından sistemin esnekliğini arttırırlar. Oluşturma kalıpları, bir sistemin hangi sınıfları kullandığı hakkındaki bilgileri kapsar, ancak bu sınıfların örneklerinin nasıl oluşturulduğuna ve bir araya getirildiğine ilişkin ayrıntıları gizler. Programcılar, kalıtımla sistemler oluşturmanın bu sistemleri çok katı hale getirdiğini fark ettiler. Yaratılış kalıpları bu yakın bağlantıyı kırmak için tasarlanmıştır.

## Tekli Desen


## Fabrika Yöntemi deseni


## Soyut Fabrika Deseni
Somut sınıflarını belirtmeden ilgili veya bağımlı nesnelerin ailelerini oluşturmak için bir arabirim sağlayın.

Bu örnekte, farklı fabrikalar kullanılarak bir bilgisayar oyunu için farklı hayvan dünyalarının yaratılması gösterilmektedir. Kıta fabrikalarının yarattığı hayvanlar farklı olsa da, hayvanlar arasındaki etkileşimler aynı kalır.

    using System;
     
    namespace GangOfFour.AbstractFactory
    {
      /// <summary>
      /// MainApp startup class for Real-World
      /// Abstract Factory Design Pattern.
      /// </summary>
      class MainApp
      {
        /// <summary>
        /// Entry point into console application.
        /// </summary>
        public static void Main()
        {
          // Create and run the African animal world
          ContinentFactory africa = new AfricaFactory();
          AnimalWorld world = new AnimalWorld(africa);
          world.RunFoodChain();
     
          // Create and run the American animal world
          ContinentFactory america = new AmericaFactory();
          world = new AnimalWorld(america);
          world.RunFoodChain();
     
          // Wait for user input
          Console.ReadKey();
        }
      }
     
     
      /// <summary>
      /// The 'AbstractFactory' abstract class
      /// </summary>
      abstract class ContinentFactory
      {
        public abstract Herbivore CreateHerbivore();
        public abstract Carnivore CreateCarnivore();
      }
     
      /// <summary>
      /// The 'ConcreteFactory1' class
      /// </summary>
      class AfricaFactory : ContinentFactory
      {
        public override Herbivore CreateHerbivore()
        {
          return new Wildebeest();
        }
        public override Carnivore CreateCarnivore()
        {
          return new Lion();
        }
      }
     
      /// <summary>
      /// The 'ConcreteFactory2' class
      /// </summary>
      class AmericaFactory : ContinentFactory
      {
        public override Herbivore CreateHerbivore()
        {
          return new Bison();
        }
        public override Carnivore CreateCarnivore()
        {
          return new Wolf();
        }
      }
     
      /// <summary>
      /// The 'AbstractProductA' abstract class
      /// </summary>
      abstract class Herbivore
      {
      }
     
      /// <summary>
      /// The 'AbstractProductB' abstract class
      /// </summary>
      abstract class Carnivore
      {
        public abstract void Eat(Herbivore h);
      }
     
      /// <summary>
      /// The 'ProductA1' class
      /// </summary>
      class Wildebeest : Herbivore
      {
      }
     
      /// <summary>
      /// The 'ProductB1' class
      /// </summary>
      class Lion : Carnivore
      {
        public override void Eat(Herbivore h)
        {
          // Eat Wildebeest
          Console.WriteLine(this.GetType().Name +
            " eats " + h.GetType().Name);
        }
      }
     
      /// <summary>
      /// The 'ProductA2' class
      /// </summary>
      class Bison : Herbivore
      {
      }
     
      /// <summary>
      /// The 'ProductB2' class
      /// </summary>
      class Wolf : Carnivore
      {
        public override void Eat(Herbivore h)
        {
          // Eat Bison
          Console.WriteLine(this.GetType().Name +
            " eats " + h.GetType().Name);
        }
      }
     
      /// <summary>
      /// The 'Client' class 
      /// </summary>
      class AnimalWorld
      {
        private Herbivore _herbivore;
        private Carnivore _carnivore;
     
        // Constructor
        public AnimalWorld(ContinentFactory factory)
        {
          _carnivore = factory.CreateCarnivore();
          _herbivore = factory.CreateHerbivore();
        }
     
        public void RunFoodChain()
        {
          _carnivore.Eat(_herbivore);
        }
      }
    }

Çıktı:

> Aslan Wildebeest'i yiyor
>
> Kurt Bizon'u yer

## Oluşturucu Modeli
Aynı yapım sürecinin farklı temsiller oluşturabilmesi ve nesnelerin montajı üzerinde yüksek düzeyde kontrol sağlaması için karmaşık bir nesnenin yapımını temsilinden ayırın.

Bu örnekte, farklı araçların adım adım monte edildiği Oluşturucu modeli gösterilmektedir. Mağaza, bir dizi ardışık adımda çeşitli Araçlar oluşturmak için Araç Üreticilerini kullanır.

    using System;
    using System.Collections.Generic;
     
    namespace GangOfFour.Builder
    {
      /// <summary>
      /// MainApp startup class for Real-World 
      /// Builder Design Pattern.
      /// </summary>
      public class MainApp
      {
        /// <summary>
        /// Entry point into console application.
        /// </summary>
        public static void Main()
        {
          VehicleBuilder builder;
     
          // Create shop with vehicle builders
          Shop shop = new Shop();
     
          // Construct and display vehicles
          builder = new ScooterBuilder();
          shop.Construct(builder);
          builder.Vehicle.Show();
     
          builder = new CarBuilder();
          shop.Construct(builder);
          builder.Vehicle.Show();
     
          builder = new MotorCycleBuilder();
          shop.Construct(builder);
          builder.Vehicle.Show();
     
          // Wait for user
          Console.ReadKey();
        }
      }
     
      /// <summary>
      /// The 'Director' class
      /// </summary>
      class Shop
      {
        // Builder uses a complex series of steps
        public void Construct(VehicleBuilder vehicleBuilder)
        {
          vehicleBuilder.BuildFrame();
          vehicleBuilder.BuildEngine();
          vehicleBuilder.BuildWheels();
          vehicleBuilder.BuildDoors();
        }
      }
     
      /// <summary>
      /// The 'Builder' abstract class
      /// </summary>
      abstract class VehicleBuilder
      {
        protected Vehicle vehicle;
     
        // Gets vehicle instance
        public Vehicle Vehicle
        {
          get { return vehicle; }
        }
     
        // Abstract build methods
        public abstract void BuildFrame();
        public abstract void BuildEngine();
        public abstract void BuildWheels();
        public abstract void BuildDoors();
      }
     
      /// <summary>
      /// The 'ConcreteBuilder1' class
      /// </summary>
      class MotorCycleBuilder : VehicleBuilder
      {
        public MotorCycleBuilder()
        {
          vehicle = new Vehicle("MotorCycle");
        }
     
        public override void BuildFrame()
        {
          vehicle["frame"] = "MotorCycle Frame";
        }
     
        public override void BuildEngine()
        {
          vehicle["engine"] = "500 cc";
        }
     
        public override void BuildWheels()
        {
          vehicle["wheels"] = "2";
        }
     
        public override void BuildDoors()
        {
          vehicle["doors"] = "0";
        }
      }
     
     
      /// <summary>
      /// The 'ConcreteBuilder2' class
      /// </summary>
      class CarBuilder : VehicleBuilder
      {
        public CarBuilder()
        {
          vehicle = new Vehicle("Car");
        }
     
        public override void BuildFrame()
        {
          vehicle["frame"] = "Car Frame";
        }
     
        public override void BuildEngine()
        {
          vehicle["engine"] = "2500 cc";
        }
     
        public override void BuildWheels()
        {
          vehicle["wheels"] = "4";
        }
     
        public override void BuildDoors()
        {
          vehicle["doors"] = "4";
        }
      }
     
      /// <summary>
      /// The 'ConcreteBuilder3' class
      /// </summary>
      class ScooterBuilder : VehicleBuilder
      {
        public ScooterBuilder()
        {
          vehicle = new Vehicle("Scooter");
        }
     
        public override void BuildFrame()
        {
          vehicle["frame"] = "Scooter Frame";
        }
     
        public override void BuildEngine()
        {
          vehicle["engine"] = "50 cc";
        }
     
        public override void BuildWheels()
        {
          vehicle["wheels"] = "2";
        }
     
        public override void BuildDoors()
        {
          vehicle["doors"] = "0";
        }
      }
     
      /// <summary>
      /// The 'Product' class
      /// </summary>
      class Vehicle
      {
        private string _vehicleType;
        private Dictionary<string,string> _parts = 
          new Dictionary<string,string>();
     
        // Constructor
        public Vehicle(string vehicleType)
        {
          this._vehicleType = vehicleType;
        }
     
        // Indexer
        public string this[string key]
        {
          get { return _parts[key]; }
          set { _parts[key] = value; }
        }
     
        public void Show()
        {
          Console.WriteLine("\n---------------------------");
          Console.WriteLine("Vehicle Type: {0}", _vehicleType);
          Console.WriteLine(" Frame : {0}", _parts["frame"]);
          Console.WriteLine(" Engine : {0}", _parts["engine"]);
          Console.WriteLine(" #Wheels: {0}", _parts["wheels"]);
          Console.WriteLine(" #Doors : {0}", _parts["doors"]);
        }
      }
    }

Çıktı

> ---------------------------
> Araç Tipi: Scooter Çerçevesi: Scooter Çerçevesi
> Motor : yok
> #Tekerlekler: 2
> #Kapılar : 0
>
> --------------------------- 
> Araç Tipi: Araba
> Çerçeve : Araba Çerçevesi
> Motor : 2500 cc
> #Tekerlekler: 4
> #Kapılar : 4
> 
> --------------------------- 
> Araç Tipi: Motosiklet
> Çerçeve : Motosiklet Çerçevesi
> Motor : 500 cc
> #Tekerlekler: 2
> #Kapılar : 0

## Prototip Kalıbı
Prototipik bir örnek kullanarak oluşturulacak nesnelerin türünü belirtin ve bu prototipi kopyalayarak yeni nesneler oluşturun.

Bu örnekte, aynı türden önceden var olan, kullanıcı tanımlı Renkler kopyalanarak yeni Color nesnelerinin oluşturulduğu Prototip deseni gösterilmektedir.

    using System;
    using System.Collections.Generic;
     
    namespace GangOfFour.Prototype
    {
      /// <summary>
      /// MainApp startup class for Real-World 
      /// Prototype Design Pattern.
      /// </summary>
      class MainApp
      {
        /// <summary>
        /// Entry point into console application.
        /// </summary>
        static void Main()
        {
          ColorManager colormanager = new ColorManager();
     
          // Initialize with standard colors
          colormanager["red"] = new Color(255, 0, 0);
          colormanager["green"] = new Color(0, 255, 0);
          colormanager["blue"] = new Color(0, 0, 255);
     
          // User adds personalized colors
          colormanager["angry"] = new Color(255, 54, 0);
          colormanager["peace"] = new Color(128, 211, 128);
          colormanager["flame"] = new Color(211, 34, 20);
     
          // User clones selected colors
          Color color1 = colormanager["red"].Clone() as Color;
          Color color2 = colormanager["peace"].Clone() as Color;
          Color color3 = colormanager["flame"].Clone() as Color;
     
          // Wait for user
          Console.ReadKey();
        }
      }
     
      /// <summary>
      /// The 'Prototype' abstract class
      /// </summary>
      abstract class ColorPrototype
      {
        public abstract ColorPrototype Clone();
      }
     
      /// <summary>
      /// The 'ConcretePrototype' class
      /// </summary>
      class Color : ColorPrototype
      {
        private int _red;
        private int _green;
        private int _blue;
     
        // Constructor
        public Color(int red, int green, int blue)
        {
          this._red = red;
          this._green = green;
          this._blue = blue;
        }
     
        // Create a shallow copy
        public override ColorPrototype Clone()
        {
          Console.WriteLine(
            "Cloning color RGB: {0,3},{1,3},{2,3}",
            _red, _green, _blue);
     
          return this.MemberwiseClone() as ColorPrototype;
        }
      }
     
      /// <summary>
      /// Prototype manager
      /// </summary>
      class ColorManager
      {
        private Dictionary<string, ColorPrototype> _colors =
          new Dictionary<string, ColorPrototype>();
     
        // Indexer
        public ColorPrototype this[string key]
        {
          get { return _colors[key]; }
          set { _colors.Add(key, value); }
        }
      }
    }

Çıktı:

> Klonlama rengi RGB: 255, 0, 0
> 
> Klonlama rengi RGB: 128.211.128
>
> Klonlama rengi RGB: 211, 34, 20

