---
title: "İşlev delegeleri"
slug: "islev-delegeleri"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Sözdizimi
- `genel temsilci TResult Func<in T, out TResult>(T arg)`
- `genel temsilci TResult Func<in T1 in T2 in, out TResult>(T1 arg1, T2 arg2)`
- `genel temsilci TResult Func<T1'de, T2'de, T3'te, çıkış TResult>(T1 arg1, T2 arg2, T3 arg3)'
- `genel delege TResult Func<T1'de, T2'de, T3'te, T4'te çıkış TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4)`



## Parametreler
| parametre | Ayrıntılar|
| -----------------| ------ |
| "arg" veya "arg1" | yöntemin (ilk) parametresi |
| 'arg2' | yöntemin ikinci parametresi |
| 'arg3' | yöntemin üçüncü parametresi |
| 'arg4' | yöntemin dördüncü parametresi |
| `T` veya `T1` | yöntemin (ilk) parametresinin türü |
| "T2" | yöntemin ikinci parametresinin türü |
| "T3" | yöntemin üçüncü parametresinin türü |
| "T4" | yöntemin dördüncü parametresinin türü |
| `TSonucu` | yöntemin dönüş türü|
   


## Parametresiz
Bu örnek, geçerli saati döndüren yöntemi kapsayan bir temsilcinin nasıl oluşturulacağını gösterir.

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

## Birden çok değişkenle
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

## Lambda ve anonim yöntemler
Bir temsilcinin beklendiği her yerde anonim bir yöntem atanabilir:

    Func<int, int> square = delegate (int x) { return x * x; }

Lambda ifadeleri aynı şeyi ifade etmek için kullanılabilir:

    Func<int, int> square = x => x * x;

Her iki durumda da, şimdi "kare" içinde depolanan yöntemi şu şekilde çağırabiliriz:

    var sq = square.Invoke(2);

Veya kısa yol olarak:

    var sq = square(2);

Atama türünün güvenli olması için, anonim yöntemin parametre türlerinin ve dönüş türünün temsilci türününkilerle eşleşmesi gerektiğine dikkat edin:

    Func<int, int> sum = delegate (int x, int y) { return x + y; } // error
    Func<int, int> sum = (x, y) => x + y; // error

[1]: https://msdn.microsoft.com/en-us/library/bb397687.aspx
[2]: https://msdn.microsoft.com/ru-ru/library/0yw3tz5k(v=vs.110).aspx

## Kovaryant ve Contravariant Tip Parametreleri
"Func" ayrıca [Covariant & Contravariant][1]'i de destekler

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

