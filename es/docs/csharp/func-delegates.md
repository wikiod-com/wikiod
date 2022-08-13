---
title: "Delegados de funciones"
slug: "delegados-de-funciones"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Sintaxis
- `delegado público TResult Func<in T, out TResult>(T arg)`
- `delegado público TResult Func<in T1, in T2, out TResult>(T1 arg1, T2 arg2)`
- `delegado público TResult Func<en T1, en T2, en T3, fuera de TResult>(T1 arg1, T2 arg2, T3 arg3)`
- `función de TResult del delegado público <en T1, en T2, en T3, en T4, fuera de TResult> (T1 arg1, T2 arg2, T3 arg3, T4 arg4)`



## Parámetros
| Parámetro | Detalles|
| -----------------| ------ |
| `arg` o `arg1` | el (primer) parámetro del método |
| `arg2` | el segundo parámetro del método |
| `arg3` | el tercer parámetro del método |
| `arg4` | el cuarto parámetro del método |
| `T` o `T1` | el tipo del (primer) parámetro del método |
| `T2` | el tipo del segundo parámetro del método |
| `T3` | el tipo del tercer parámetro del método |
| `T4` | el tipo del cuarto parámetro del método |
| `TResultado` | el tipo de retorno del método|
   


## Sin parámetros
Este ejemplo muestra cómo crear un delegado que encapsule el método que devuelve la hora actual

    static DateTime UTCNow()
    {
        return DateTime.UtcNow;
    }

    static DateTime LocalNow()
    {
        return DateTime.Now;
    }

    static void Main(string[] args)
    {
        Func<DateTime> method = UTCNow;
        // method points to the UTCNow method
        // that retuns current UTC time  
        DateTime utcNow = method();

        method = LocalNow;
        // now method points to the LocalNow method
        // that returns local time

        DateTime localNow = method();
    }

## Con múltiples variables
    static int Sum(int a, int b)
    {
        return a + b;
    }

    static int Multiplication(int a, int b)
    {
        return a * b;
    }

    static void Main(string[] args)
    {
        Func<int, int, int> method = Sum;
        // method points to the Sum method
        // that retuns 1 int variable and takes 2 int variables  
        int sum = method(1, 1);

        method = Multiplication;
        // now method points to the Multiplication method

        int multiplication = method(1, 1);
    }

## Lambda y métodos anónimos
Se puede asignar un método anónimo donde se espera un delegado:

    Func<int, int> square = delegate (int x) { return x * x; }

Las expresiones lambda se pueden usar para expresar lo mismo:

    Func<int, int> square = x => x * x;

En cualquier caso, ahora podemos invocar el método almacenado dentro de `square` de esta manera:

    var sq = square.Invoke(2);

O como abreviatura:

    var sq = square(2);

Tenga en cuenta que para que la asignación sea de tipo seguro, los tipos de parámetro y el tipo de retorno del método anónimo deben coincidir con los del tipo de delegado:

    Func<int, int> sum = delegate (int x, int y) { return x + y; } // error
    Func<int, int> sum = (x, y) => x + y; // error

[1]: https://msdn.microsoft.com/en-us/library/bb397687.aspx
[2]: https://msdn.microsoft.com/ru-ru/library/0yw3tz5k(v=vs.110).aspx

## Parámetros de tipo covariante y contravariante
`Func` también es compatible con [Covariante y Contravariante][1]

    // Simple hierarchy of classes.
    public class Person { }
    public class Employee : Person { }
    
    class Program
    {
        static Employee FindByTitle(String title)
        {
            // This is a stub for a method that returns
            // an employee that has the specified title.
            return new Employee();
        }
    
        static void Test()
        {
            // Create an instance of the delegate without using variance.
            Func<String, Employee> findEmployee = FindByTitle;
    
            // The delegate expects a method to return Person,
            // but you can assign it a method that returns Employee.
            Func<String, Person> findPerson = FindByTitle;
    
            // You can also assign a delegate 
            // that returns a more derived type 
            // to a delegate that returns a less derived type.
            findPerson = findEmployee;
    
        }
    }


[1]: https://msdn.microsoft.com/en-us/library/dd799517(v=vs.110).aspx

