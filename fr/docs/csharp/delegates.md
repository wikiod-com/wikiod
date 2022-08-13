---
title: "Délégués"
slug: "delegues"
draft: false
images: []
weight: 9752
type: docs
toc: true
---

# Sommaire

Un **type délégué** est un type représentant une signature de méthode particulière. Une instance de ce type fait référence à une méthode particulière avec une signature correspondante. Les paramètres de méthode peuvent avoir des types délégués, et donc cette seule méthode doit recevoir une référence à une autre méthode, qui peut ensuite être invoquée

# Types délégués intégrés : `Action<...>`, `Predicate<T>` et `Func<...,TResult>`

L'espace de noms `System` contient les délégués `Action<...>`,`Predicate<T>` et `Func<...,TResult>`, où le "..." représente entre 0 et 16 paramètres de type générique ( pour 0 paramètre, `Action` n'est pas générique).

`Func` représente les méthodes avec un type de retour correspondant à `TResult` et `Action` représente les méthodes sans valeur de retour (void). Dans les deux cas, les paramètres de type générique supplémentaires correspondent, dans l'ordre, aux paramètres de méthode.

`Predicate` représente une méthode avec un type de retour booléen, T est un paramètre d'entrée.

# Types de délégués personnalisés

Les types délégués nommés peuvent être déclarés à l'aide du mot clé `delegate`.

# Invoquer des délégués

Les délégués peuvent être appelés en utilisant la même syntaxe que les méthodes : le nom de l'instance de délégué, suivi de parenthèses contenant tous les paramètres.

# Attribuer aux délégués

Les délégués peuvent être affectés de la manière suivante :

- Affectation d'une méthode nommée
- Affectation d'une méthode anonyme à l'aide d'un lambda
- Affectation d'une méthode nommée à l'aide du mot clé `delegate`.

# Combiner les délégués

Plusieurs objets délégués peuvent être affectés à une instance déléguée à l'aide de l'opérateur `+`. L'opérateur `-` peut être utilisé pour supprimer un délégué de composant d'un autre délégué.

## Déclarer un type délégué
La syntaxe suivante crée un type `delegate` avec le nom `NumberInOutDelegate`, représentant une méthode qui prend un `int` et renvoie un `int`.

    public delegate int NumberInOutDelegate(int input);

Cela peut être utilisé comme suit :

    public static class Program
    {
        static void Main()
        {
            NumberInOutDelegate square = MathDelegates.Square;
            int answer1 = square(4); 
            Console.WriteLine(answer1); // Will output 16

            NumberInOutDelegate cube = MathDelegates.Cube;
            int answer2 = cube(4);
            Console.WriteLine(answer2); // Will output 64            
        }
    }
    
    public static class MathDelegates
    {
        static int Square (int x)
        {
            return x*x;
        }

        static int Cube (int x)
        {
            return x*x*x;
        }
    }

L'instance de délégué `example` est exécutée de la même manière que la méthode `Square`. Une instance déléguée agit littéralement comme un délégué pour l'appelant : l'appelant invoque le
délégué, puis le délégué appelle la méthode cible. Cette indirection découple
l'appelant de la méthode cible.

----------

Vous pouvez déclarer un type délégué __generic__ et, dans ce cas, vous pouvez spécifier que le type est covariant (`out`) ou contravariant (`in`) dans certains des arguments de type. Par exemple:

    public delegate TTo Converter<in TFrom, out TTo>(TFrom input);

Comme les autres types génériques, les types délégués génériques peuvent avoir des contraintes, telles que `where TFrom : struct, IConvertible where TTo : new()`.

Évitez la covariance et la contravariance pour les types de délégués destinés à être utilisés pour les délégués de multidiffusion, tels que les types de gestionnaires d'événements. En effet, la concaténation (`+`) peut échouer si le type à l'exécution est différent du type à la compilation en raison de la variance. Par exemple, évitez :

    public delegate void EventHandler<in TEventArgs>(object sender, TEventArgs e);

Utilisez plutôt un type générique invariant :

    public delegate void EventHandler<TEventArgs>(object sender, TEventArgs e);

----------

Sont également pris en charge les délégués où certains paramètres sont modifiés par `ref` ou `out`, comme dans :

    public delegate bool TryParser<T>(string input, out T result);

(exemple d'utilisation `TryParser<decimal> example = decimal.TryParse;`), ou délégués où le dernier paramètre a le modificateur `params`. Les types délégués peuvent avoir des paramètres facultatifs (fournir des valeurs par défaut). Les types délégués peuvent utiliser des types de pointeurs comme `int*` ou `char*` dans leurs signatures ou leurs types de retour (utilisez le mot-clé `unsafe`). Un type délégué et ses paramètres peuvent contenir des attributs personnalisés.

## Les types délégués Func<T, TResult>, Action<T> et Predicate<T>
L'espace de noms System contient les types délégués `Func<..., TResult>` avec entre 0 et 15 paramètres génériques, renvoyant le type `TResult`.

    private void UseFunc(Func<string> func)
    {
        string output = func(); // Func with a single generic type parameter returns that type
        Console.WriteLine(output);
    }

    private void UseFunc(Func<int, int, string> func)
    {
        string output = func(4, 2); // Func with multiple generic type parameters takes all but the first as parameters of that type
        Console.WriteLine(output);
    }

L'espace de noms System contient également des types délégués `Action<...>` avec un nombre différent de paramètres génériques (de 0 à 16). Il est similaire à `Func<T1, .., Tn>`, mais il renvoie toujours `void`.

    private void UseAction(Action action)
    {
        action(); // The non-generic Action has no parameters
    }

    private void UseAction(Action<int, string> action)
    {
        action(4, "two"); // The generic action is invoked with parameters matching its type arguments
    }

`Predicate<T>` est aussi une forme de `Func` mais il renverra toujours `bool`. Un prédicat est un moyen de spécifier un critère personnalisé. En fonction de la valeur de l'entrée et de la logique définie dans le prédicat, il renverra soit "vrai" soit "faux". `Predicate<T>` se comporte donc de la même manière que `Func<T, bool>` et les deux peuvent être initialisés et utilisés de la même manière.
    
    Predicate<string> predicate = s => s.StartsWith("a");
    Func<string, bool> func = s => s.StartsWith("a");

    // Both of these return true
    var predicateReturnsTrue = predicate("abc");
    var funcReturnsTrue = func("abc");

    // Both of these return false
    var predicateReturnsFalse = predicate("xyz");
    var funcReturnsFalse = func("xyz");

Le choix d'utiliser `Predicate<T>` ou `Func<T, bool>` est vraiment une question d'opinion. `Predicate<T>` est sans doute plus expressif de l'intention de l'auteur, tandis que `Func<T, bool>` est susceptible d'être familier à une plus grande proportion de développeurs C#.

En plus de cela, il existe des cas où une seule des options est disponible, en particulier lors de l'interaction avec une autre API. Par exemple, `List<T>` et `Array<T>` prennent généralement `Predicate<T>` pour leurs méthodes, tandis que la plupart des extensions LINQ n'acceptent que `Func<T, bool>`.

## Combiner les délégués (délégués multidiffusion)
Les opérations d'addition `+` et de soustraction `-` peuvent être utilisées pour combiner des instances déléguées. Le délégué contient une liste des délégués affectés.

    using System;
    using System.Reflection;
    using System.Reflection.Emit;

    namespace DelegatesExample {
        class MainClass {
            private delegate void MyDelegate(int a);

            private static void PrintInt(int a) {
                Console.WriteLine(a);
            }

            private static void PrintType<T>(T a) {
                Console.WriteLine(a.GetType());
            }

            public static void Main (string[] args)
            {
                MyDelegate d1 = PrintInt;
                MyDelegate d2 = PrintType;

                // Output:
                // 1
                d1(1);

                // Output:
                // System.Int32
                d2(1);

                MyDelegate d3 = d1 + d2;
                // Output:
                // 1
                // System.Int32
                d3(1);

                MyDelegate d4 = d3 - d2;
                // Output:
                // 1
                d4(1);

                // Output:
                // True
                Console.WriteLine(d1 == d4);
            }
        }
    }

Dans cet exemple, `d3` est une combinaison de délégués `d1` et `d2`, donc lorsqu'il est appelé, le programme affiche les chaînes `1` et `System.Int32`.


----------

Combinaison de délégués avec des types de retour **non void** :

Si un délégué multicast a un type de retour `nonvoid`, l'appelant reçoit la valeur de retour
de la dernière méthode invoquée. Les méthodes précédentes sont encore appelées, mais leur
les valeurs de retour sont ignorées.

        class Program
        {
            public delegate int Transformer(int x);

            static void Main(string[] args)
            {
                Transformer t = Square;
                t += Cube;
                Console.WriteLine(t(2));  // O/P 8 
            }

            static int Square(int x) { return x * x; }

            static int Cube(int x) { return x*x*x; }
        }

`t(2)` appellera d'abord `Square` puis `Cube`. La valeur de retour de Square est ignorée et la valeur de retour de la dernière méthode, c'est-à-dire "Cube", est conservée.

## Invoquer en toute sécurité le délégué multicast
Vous avez toujours voulu appeler un délégué multidiffusion, mais vous souhaitez que la liste d'invocation entière soit appelée même si une exception se produit dans la chaîne. Alors vous avez de la chance, j'ai créé une méthode d'extension qui fait exactement cela, en lançant une `AggregateException` uniquement après l'exécution de la liste complète :

    public static class DelegateExtensions
    {
        public static void SafeInvoke(this Delegate del,params object[] args)
        {
            var exceptions = new List<Exception>();

            foreach (var handler in del.GetInvocationList())
            {
                try
                {
                    handler.Method.Invoke(handler.Target, args);
                }
                catch (Exception ex)
                {
                    exceptions.Add(ex);
                }
            }

            if(exceptions.Any())
            {
                throw new AggregateException(exceptions);
            }
        }
    }

    public class Test
    {
        public delegate void SampleDelegate();

        public void Run()
        {
            SampleDelegate delegateInstance = this.Target2;
            delegateInstance += this.Target1;

            try
            {
                delegateInstance.SafeInvoke();
            } 
            catch(AggregateException ex)
            {
                // Do any exception handling here
            }
        }

        private void Target1()
        {
            Console.WriteLine("Target 1 executed");
        }

        private void Target2()
        {
            Console.WriteLine("Target 2 executed");
            throw new Exception();
        }
    }

Cela génère :

    Target 2 executed
    Target 1 executed

Appeler directement, sans `SaveInvoke`, n'exécuterait que la cible 2.

## Déléguer l'égalité
L'appel de `.Equals()` sur un délégué compare par référence l'égalité :

    Action action1 = () => Console.WriteLine("Hello delegates");
    Action action2 = () => Console.WriteLine("Hello delegates");
    Action action1Again = action1;

    Console.WriteLine(action1.Equals(action1)) // True
    Console.WriteLine(action1.Equals(action2)) // False
    Console.WriteLine(action1Again.Equals(action1)) // True

Ces règles s'appliquent également lors de l'exécution de `+=` ou `-=` sur un délégué multicast, par exemple lors de l'abonnement et du désabonnement à des événements.

## Références sous-jacentes des délégués de méthode nommés
Lors de l'affectation de méthodes nommées aux délégués, elles feront référence au même objet sous-jacent si :

- Il s'agit de la même méthode d'instance, sur la même instance d'une classe
- Ce sont la même méthode statique sur une classe

       public class Greeter
       {
           public void WriteInstance()
           {
               Console.WriteLine("Instance");
           }

           public static void WriteStatic()
           {
               Console.WriteLine("Static");
           }
       }

       // ...

       Greeter greeter1 = new Greeter();
       Greeter greeter2 = new Greeter();

       Action instance1 = greeter1.WriteInstance;
       Action instance2 = greeter2.WriteInstance;
       Action instance1Again = greeter1.WriteInstance;
    
       Console.WriteLine(instance1.Equals(instance2)); // False
       Console.WriteLine(instance1.Equals(instance1Again)); // True

       Action @static = Greeter.WriteStatic;
       Action staticAgain = Greeter.WriteStatic;

       Console.WriteLine(@static.Equals(staticAgain)); // True

## Affectation d'une méthode nommée à un délégué
Des méthodes nommées peuvent être attribuées à des délégués avec des signatures correspondantes :

    public static class Example
    {
        public static int AddOne(int input)
        {
            return input + 1;
        }
    }


    Func<int,int> addOne = Example.AddOne

`Example.AddOne` prend un `int` et renvoie un `int`, sa signature correspond au délégué `Func<int,int>`. `Example.AddOne` peut être directement affecté à `addOne` car ils ont des signatures correspondantes.

## Affectation à un délégué par lambda
Lambdas peut être utilisé pour créer des méthodes anonymes à attribuer à un délégué :

    Func<int,int> addOne = x => x+1;

Notez que la déclaration explicite de type est requise lors de la création d'une variable de cette manière :

    var addOne = x => x+1; // Does not work

## Passer des délégués en tant que paramètres
Les délégués peuvent être utilisés comme pointeurs de fonction typés :

    class FuncAsParameters
    {
      public void Run()
      {
        DoSomething(ErrorHandler1);
        DoSomething(ErrorHandler2);
      }
    
      public bool ErrorHandler1(string message)
      {
        Console.WriteLine(message);
        var shouldWeContinue = ...  
        return shouldWeContinue;
      }
    
      public bool ErrorHandler2(string message)
      {
        // ...Write message to file...
        var shouldWeContinue = ...  
        return shouldWeContinue;
      }
    
      public void DoSomething(Func<string, bool> errorHandler)
      {
        // In here, we don't care what handler we got passed!
        ...
        if (...error...)
        {
          if (!errorHandler("Some error occurred!"))
          {
            // The handler decided we can't continue
            return;
          }
        }
      }
    }

## Fermeture à l'intérieur d'un délégué


## Encapsuler les transformations dans les fonctions


