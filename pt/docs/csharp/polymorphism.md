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
Polimorfismo significa que uma operação também pode ser aplicada a valores de alguns outros tipos.

Existem vários tipos de polimorfismo:

- **Polimorfismo ad hoc:**
contém `sobrecarga de função`. O objetivo é que um Método possa ser usado com
diferentes tipos sem a necessidade de ser genérico.
- **Polimorfismo paramétrico:**
é o uso de tipos genéricos. Veja [Genéricos][1]
- **Subdigitação:**
tem o destino herdar de uma classe para generalizar uma funcionalidade semelhante

---------------

# Polimorfismo ad hoc #

O objetivo do `Ad hoc polymorphism` é criar um método, que possa ser chamado por diferentes tipos de dados sem a necessidade de conversão de tipo na chamada de função ou genéricos. Os métodos a seguir `sumInt(par1, par2)` podem ser chamados com diferentes tipos de dados e têm para cada combinação de tipos uma implementação própria:


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

Aqui está um exemplo de chamada:


    public static void Main()
    {
        Console.WriteLine(sumInt( 1 , 2 ));  //  3
        Console.WriteLine(sumInt("3","4"));  //  7
        Console.WriteLine(sumInt("5", 6 ));  // 11
        Console.WriteLine(sumInt( 7 ,"8"));  // 15
    }

------

# Subdigitação #

A subtipagem é o uso de herdar de uma classe base para generalizar um comportamento semelhante:

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

Ambas as classes `NormalCar` e `ElectricCar` agora têm um método para reabastecer, mas sua própria implementação. Aqui está um Exemplo:


    public static void Main()
    {
        List<Car> cars = new List<Car>(){
            new NormalCar(),
            new ElectricCar()
        };
        
        cars.ForEach(x => x.refuel());
    }

A saída será a seguinte:

> Reabastecimento com gasolina
Carregando bateria

[1]: https://www.wikiod.com/pt/docs/c%23/27/generics

## Outro exemplo de polimorfismo
O polimorfismo é um dos pilares da POO. Poly deriva de um termo grego que significa "múltiplas formas".

Abaixo está um exemplo que exibe polimorfismo. A classe `Vehicle` assume várias formas como uma classe base.

As classes derivadas `Ducati` e `Lamborghini` herdam de `Vehicle` e sobrescrevem o método `Display()` da classe base, para exibir seu próprio `NumberOfWheels`.


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

Abaixo está o trecho de código onde o Polimorfismo é exibido. O objeto é criado para o tipo base `Vehicle` usando uma variável `vehicle` na Linha 1. Ele chama o método da classe base `Display()` na Linha 2 e exibe a saída como mostrado.

     static void Main(string[] args)
     {
        Vehicle vehicle = new Vehicle();    //Line 1
        vehicle.Display();                  //Line 2  
        vehicle = new Ducati();             //Line 3
        vehicle.Display();                  //Line 4
        vehicle = new Lamborghini();        //Line 5
        vehicle.Display();                  //Line 6
     }

Na Linha 3, o objeto `vehicle` é apontado para a classe derivada `Ducati` e chama seu método `Display()`, que exibe a saída conforme mostrado. Aqui vem o comportamento polimórfico, mesmo que o objeto `vehicle` seja do tipo `Vehicle`, ele chama o método da classe derivada `Display()` já que o tipo `Ducati` substitui o método `Display()` da classe base, já que o objeto `vehicle` é apontado para `Ducati`.

A mesma explicação é aplicável quando invoca o método `Display()` do tipo `Lamborghini`.

A saída é mostrada abaixo
    
    The number of wheels for the Vehicle is 0        // Line 2 
    The number of wheels for the Ducati is 2         // Line 4
    The number of wheels for the Lamborghini is 4    // Line 6
    

 

