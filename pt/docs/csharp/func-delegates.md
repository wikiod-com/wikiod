---
title: "Delegados de funções"
slug: "delegados-de-funcoes"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Sintaxe
- `delegado público TResult Func<in T, out TResult>(T arg)`
- `Função TResult do delegado público<em T1, em T2, fora TResult>(T1 arg1, T2 arg2)`
- `Função TResult do delegado público<em T1, em T2, em T3, fora TResult>(T1 arg1, T2 arg2, T3 arg3)`
- `Função TResult do delegado público<em T1, em T2, em T3, em T4, fora TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4)`



## Parâmetros
| Parâmetro | Detalhes|
| -----------------| ------ |
| `arg` ou `arg1` | o (primeiro) parâmetro do método |
| `arg2` | o segundo parâmetro do método |
| `arg3` | o terceiro parâmetro do método |
| `arg4` | o quarto parâmetro do método |
| `T` ou `T1` | o tipo do (primeiro) parâmetro do método |
| `T2` | o tipo do segundo parâmetro do método |
| `T3` | o tipo do terceiro parâmetro do método |
| `T4` | o tipo do quarto parâmetro do método |
| `TResult` | o tipo de retorno do método|
   


## Sem parâmetros
Este exemplo mostra como criar um delegado que encapsula o método que retorna a hora atual

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

## Com várias variáveis
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

## Lambda e métodos anônimos
Um método anônimo pode ser atribuído sempre que um delegado for esperado:

    Func<int, int> square = delegate (int x) { return x * x; }

Expressões lambda podem ser usadas para expressar a mesma coisa:

    Func<int, int> square = x => x * x;

Em ambos os casos, agora podemos invocar o método armazenado dentro de `square` assim:

    var sq = square.Invoke(2);

Ou como abreviação:

    var sq = square(2);

Observe que, para que a atribuição seja de tipo seguro, os tipos de parâmetro e o tipo de retorno do método anônimo devem corresponder aos do tipo delegado:

    Func<int, int> sum = delegate (int x, int y) { return x + y; } // error
    Func<int, int> sum = (x, y) => x + y; // error

[1]: https://msdn.microsoft.com/en-us/library/bb397687.aspx
[2]: https://msdn.microsoft.com/ru-ru/library/0yw3tz5k(v=vs.110).aspx

## Parâmetros de tipo covariante e contravariante
`Func` também suporta [Covariant & Contravariant][1]

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

