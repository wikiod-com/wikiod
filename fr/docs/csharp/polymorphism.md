---
title: "Polymorphisme"
slug: "polymorphisme"
draft: false
images: []
weight: 9906
type: docs
toc: true
---

## Types de polymorphisme
Le polymorphisme signifie qu'une opération peut également être appliquée à des valeurs d'autres types.

Il existe plusieurs types de polymorphisme :

- **Polymorphisme ad hoc :**
contient `surcharge de fonction`. L'objectif est qu'une méthode puisse être utilisée avec
différents types sans qu'il soit nécessaire d'être générique.
- **Polymorphisme paramétrique :**
est l'utilisation de types génériques. Voir [Génériques][1]
- **Sous-typage :**
a pour cible l'héritage d'une classe pour généraliser une fonctionnalité similaire

---------------

# Polymorphisme ad hoc #

L'objectif du "polymorphisme ad hoc" est de créer une méthode pouvant être appelée par différents types de données sans nécessiter de conversion de type dans l'appel de fonction ou les génériques. La ou les méthodes suivantes `sumInt(par1, par2)` peuvent être appelées avec différents types de données et ont pour chaque combinaison de types une propre implémentation :


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

Voici un exemple d'appel :


    public static void Main()
    {
        Console.WriteLine(sumInt( 1 , 2 ));  //  3
        Console.WriteLine(sumInt("3","4"));  //  7
        Console.WriteLine(sumInt("5", 6 ));  // 11
        Console.WriteLine(sumInt( 7 ,"8"));  // 15
    }

------

# Sous-typage #

Le sous-typage est l'utilisation de l'héritage d'une classe de base pour généraliser un comportement similaire :

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

Les deux classes `NormalCar` et `ElectricCar` ont maintenant une méthode pour faire le plein, mais leur propre implémentation. Voici un exemple :


    public static void Main()
    {
        List<Car> cars = new List<Car>(){
            new NormalCar(),
            new ElectricCar()
        };
        
        cars.ForEach(x => x.refuel());
    }

La sortie sera la suivante :

> Faire le plein d'essence
Charge de la batterie

[1] : https://www.wikiod.com/fr/docs/c%23/27/generics

## Un autre exemple de polymorphisme
Le polymorphisme est l'un des piliers de la POO. Poly dérive d'un terme grec qui signifie « formes multiples ».

Vous trouverez ci-dessous un exemple présentant un polymorphisme. La classe `Vehicle` prend plusieurs formes comme classe de base.

Les classes dérivées `Ducati` et `Lamborghini` héritent de `Vehicle` et remplacent la méthode `Display()` de la classe de base, pour afficher son propre `NumberOfWheels`.


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

Ci-dessous se trouve l'extrait de code où le polymorphisme est exposé. L'objet est créé pour le type de base `Vehicle` à l'aide d'une variable `vehicle` à la ligne 1. Il appelle la méthode de classe de base `Display()` à la ligne 2 et affiche la sortie comme indiqué.

     static void Main(string[] args)
     {
        Vehicle vehicle = new Vehicle();    //Line 1
        vehicle.Display();                  //Line 2  
        vehicle = new Ducati();             //Line 3
        vehicle.Display();                  //Line 4
        vehicle = new Lamborghini();        //Line 5
        vehicle.Display();                  //Line 6
     }

À la ligne 3, l'objet `vehicle` pointe vers la classe dérivée `Ducati` et appelle sa méthode `Display()`, qui affiche la sortie comme indiqué. Voici le comportement polymorphe, même si l'objet `vehicle` est de type `Vehicle`, il appelle la méthode de classe dérivée `Display()` car le type `Ducati` remplace la méthode de base `Display()`, puisque le L'objet `vehicle` est pointé vers `Ducati`.

La même explication est applicable lorsqu'il invoque la méthode `Display()` du type `Lamborghini`.

La sortie est illustrée ci-dessous
    
    The number of wheels for the Vehicle is 0        // Line 2 
    The number of wheels for the Ducati is 2         // Line 4
    The number of wheels for the Lamborghini is 4    // Line 6
    

 

