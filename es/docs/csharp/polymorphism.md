---
title: "Polimorfismo"
slug: "polimorfismo"
draft: false
images: []
weight: 9906
type: docs
toc: true
---

## Tipos de polimorfismo
El polimorfismo significa que una operación también se puede aplicar a valores de algunos otros tipos.

Hay varios tipos de polimorfismo:

- **Polimorfismo ad hoc:**
contiene `sobrecarga de funciones`. El objetivo es que un método se pueda utilizar con
diferentes tipos sin necesidad de ser genéricos.
- **Polimorfismo paramétrico:**
es el uso de tipos genéricos. Ver [Genéricos][1]
- **Subtipificación:**
tiene el objetivo de heredar de una clase para generalizar una funcionalidad similar

---------------

# Polimorfismo ad hoc #

El objetivo del "polimorfismo ad hoc" es crear un método que pueda ser llamado por diferentes tipos de datos sin necesidad de conversión de tipo en la llamada de función o genéricos. Los siguientes métodos `sumInt(par1, par2)` se pueden llamar con diferentes tipos de datos y tienen una implementación propia para cada combinación de tipos:


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

Aquí hay una llamada de ejemplo:


    public static void Main()
    {
        Console.WriteLine(sumInt( 1 , 2 ));  //  3
        Console.WriteLine(sumInt("3","4"));  //  7
        Console.WriteLine(sumInt("5", 6 ));  // 11
        Console.WriteLine(sumInt( 7 ,"8"));  // 15
    }

------

# Subtipado #

La subtipificación es el uso de heredar de una clase base para generalizar un comportamiento similar:

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

Ambas clases `NormalCar` y `ElectricCar` ahora tienen un método para repostar, pero su propia implementación. Aquí hay un ejemplo:


    public static void Main()
    {
        List<Car> cars = new List<Car>(){
            new NormalCar(),
            new ElectricCar()
        };
        
        cars.ForEach(x => x.refuel());
    }

La salida será la siguiente:

> Repostaje con gasolina
Bateria cargando

[1]: https://www.wikiod.com/es/docs/c%23/27/generics

## Otro ejemplo de polimorfismo
El polimorfismo es uno de los pilares de la programación orientada a objetos. Poly deriva de un término griego que significa "múltiples formas".

A continuación se muestra un ejemplo que exhibe polimorfismo. La clase `Vehicle` toma múltiples formas como clase base.

Las clases derivadas `Ducati` y `Lamborghini` se heredan de `Vehicle` y anulan el método `Display()` de la clase base, para mostrar su propio `NumberOfWheels`.


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

A continuación se muestra el fragmento de código donde se exhibe el polimorfismo. El objeto se crea para el tipo base `Vehicle` usando una variable `vehicle` en la Línea 1. Llama al método de clase base `Display()` en la Línea 2 y muestra la salida como se muestra.

     static void Main(string[] args)
     {
        Vehicle vehicle = new Vehicle();    //Line 1
        vehicle.Display();                  //Line 2  
        vehicle = new Ducati();             //Line 3
        vehicle.Display();                  //Line 4
        vehicle = new Lamborghini();        //Line 5
        vehicle.Display();                  //Line 6
     }

En la Línea 3, el objeto `vehicle` apunta a la clase derivada `Ducati` y llama a su método `Display()`, que muestra la salida como se muestra. Aquí viene el comportamiento polimórfico, aunque el objeto `vehicle` es del tipo `Vehicle`, llama al método de clase derivada `Display()` ya que el tipo `Ducati` anula el método `Display()` de la clase base, ya que el El objeto 'vehículo' apunta hacia 'Ducati'.

La misma explicación es aplicable cuando invoca el método `Display()` del tipo `Lamborghini`.

La salida se muestra a continuación
    
    The number of wheels for the Vehicle is 0        // Line 2 
    The number of wheels for the Ducati is 2         // Line 4
    The number of wheels for the Lamborghini is 4    // Line 6
    

 

