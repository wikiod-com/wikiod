---
title: "Délégués fonctionnels"
slug: "delegues-fonctionnels"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Syntaxe
- `délégué public TResult Func<in T, out TResult>(T arg)`
- `délégué public TResult Func<in T1, in T2, out TResult>(T1 arg1, T2 arg2)`
- `délégué public TResult Func<in T1, in T2, in T3, out TResult>(T1 arg1, T2 arg2, T3 arg3)`
- `délégué public TResult Func<in T1, in T2, in T3, in T4, out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4)`



## Paramètres
| Paramètre | Détails|
| -----------------| ------ |
| `arg` ou `arg1` | le (premier) paramètre de la méthode |
| `arg2` | le deuxième paramètre de la méthode |
| `arg3` | le troisième paramètre de la méthode |
| `arg4` | le quatrième paramètre de la méthode |
| 'T' ou 'T1' | le type du (premier) paramètre de la méthode |
| `T2` | le type du deuxième paramètre de la méthode |
| `T3` | le type du troisième paramètre de la méthode |
| `T4` | le type du quatrième paramètre de la méthode |
| `TRésultat` | le type de retour de la méthode |
   


## Sans paramètres
Cet exemple montre comment créer un délégué qui encapsule la méthode qui renvoie l'heure actuelle

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

## Avec plusieurs variables
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

## Méthodes Lambda et anonymes
Une méthode anonyme peut être attribuée partout où un délégué est attendu :

    Func<int, int> square = delegate (int x) { return x * x; }

Les expressions lambda peuvent être utilisées pour exprimer la même chose :

    Func<int, int> square = x => x * x;

Dans les deux cas, nous pouvons maintenant invoquer la méthode stockée dans `square` comme ceci :

    var sq = square.Invoke(2);

Ou en raccourci :

    var sq = square(2);

Notez que pour que l'affectation soit de type sécurisé, les types de paramètres et le type de retour de la méthode anonyme doivent correspondre à ceux du type délégué :

    Func<int, int> sum = delegate (int x, int y) { return x + y; } // error
    Func<int, int> sum = (x, y) => x + y; // error

[1] : https://msdn.microsoft.com/en-us/library/bb397687.aspx
[2] : https://msdn.microsoft.com/ru-ru/library/0yw3tz5k(v=vs.110).aspx

## Paramètres de type covariant et contravariant
`Func` prend également en charge [Covariant & Contravariant][1]

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


[1] : https://msdn.microsoft.com/en-us/library/dd799517(v=vs.110).aspx

