---
title: "Modèles de conception de création"
slug: "modeles-de-conception-de-creation"
draft: false
images: []
weight: 9850
type: docs
toc: true
---

Les modèles de création visent à séparer un système de la façon dont ses objets sont créés, composés et représentés. Ils augmentent la flexibilité du système en termes de quoi, qui, comment et quand de la création d'objets. Les modèles de création encapsulent les connaissances sur les classes utilisées par un système, mais ils cachent les détails de la façon dont les instances de ces classes sont créées et assemblées. Les programmeurs se sont rendu compte que la composition de systèmes avec héritage rend ces systèmes trop rigides. Les modèles de création sont conçus pour rompre ce couplage étroit.

## Modèle de singleton


## Modèle de méthode d'usine


## Modèle d'usine abstrait
Fournir une interface pour créer des familles d'objets liés ou dépendants sans spécifier leurs classes concrètes.

Dans cet exemple montre la création de différents mondes d'animaux pour un jeu informatique en utilisant différentes usines. Bien que les animaux créés par les usines Continent soient différents, les interactions entre les animaux restent les mêmes.

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

Production:

> Lion mange des gnous
>
> Le loup mange le bison

## Modèle de constructeur
Séparez la construction d'un objet complexe de sa représentation afin que le même processus de construction puisse créer différentes représentations et offre un haut niveau de contrôle sur l'assemblage des objets.

Cet exemple illustre le modèle Builder dans lequel différents véhicules sont assemblés étape par étape. La boutique utilise des constructeurs de véhicules pour construire une variété de véhicules en une série d'étapes séquentielles.

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

Production

> ---------------------------
> Type de véhicule : Cadre de scooter : Cadre de scooter
> Moteur : aucun
> #Roues : 2
> #Portes : 0
>
> --------------------------- 
> Type de véhicule : Voiture
> Châssis : Châssis de voiture
> Moteur : 2500 cm3
> #Roues : 4
> #Portes : 4
> 
> --------------------------- 
> Type de véhicule : moto
> Châssis : Châssis Moto
> Moteur : 500 cm3
> #Roues : 2
> #Portes : 0

## Modèle de prototype
Spécifiez le type d'objets à créer à l'aide d'une instance prototype et créez de nouveaux objets en copiant ce prototype.

Cet exemple illustre le modèle Prototype dans lequel de nouveaux objets Color sont créés en copiant des couleurs préexistantes définies par l'utilisateur du même type.

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

Production:

> Couleur de clonage RVB : 255, 0, 0
> 
> Couleur de clonage RVB : 128 211 128
>
> Couleur de clonage RVB : 211, 34, 20

