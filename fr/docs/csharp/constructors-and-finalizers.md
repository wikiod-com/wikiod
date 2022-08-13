---
title: "Constructeurs et finaliseurs"
slug: "constructeurs-et-finaliseurs"
draft: false
images: []
weight: 9573
type: docs
toc: true
---

Les constructeurs sont des méthodes d'une classe qui sont appelées lorsqu'une instance de cette classe est créée. Leur principale responsabilité est de laisser le nouvel objet dans un état utile et cohérent.

Les destructeurs/finaliseurs sont des méthodes d'une classe qui sont invoquées lorsqu'une instance de celle-ci est détruite. En C#, ils sont rarement écrits/utilisés explicitement.

C # n'a pas réellement de destructeurs, mais plutôt des finaliseurs qui utilisent la syntaxe de destructeur de style C ++. La spécification d'un destructeur remplace la méthode `Object.Finalize()` qui ne peut pas être appelée directement.

Contrairement à d'autres langages avec une syntaxe similaire, ces méthodes ne sont *pas* appelées lorsque les objets sortent de la portée, mais sont appelées lorsque le Garbage Collector s'exécute, ce qui se produit [sous certaines conditions][3]. En tant que tels, ils ne sont *pas* garantis pour fonctionner dans un ordre particulier.

Les finaliseurs doivent être responsables du nettoyage des ressources non gérées **uniquement** (pointeurs acquis via la classe Marshal, reçus via p/Invoke (appels système) ou pointeurs bruts utilisés dans des blocs non sécurisés). Pour nettoyer les ressources gérées, veuillez consulter IDisposable, le modèle Dispose et l'instruction [`using`][1].

(Pour en savoir plus : [Quand dois-je créer un destructeur ?][2])


[1] : https://www.wikiod.com/fr/docs/c%23/38/using-statement
[2] : http://stackoverflow.com/a/4899622
[3] : https://msdn.microsoft.com/en-us/library/ee787088(v=vs.110).aspx#conditions_for_a_garbage_collection

## Constructeur statique
Un constructeur statique est appelé la première fois qu'un membre d'un type est initialisé, un membre de classe statique est appelé ou une méthode statique.
Le constructeur statique est thread-safe.
Un constructeur statique est couramment utilisé pour :
- Initialiser l'état statique, c'est-à-dire l'état partagé entre différentes instances de la même classe.
- Créer un singleton

**Exemple:**

    class Animal
    {
        // * A static constructor is executed only once,
        //   when a class is first accessed.
        // * A static constructor cannot have any access modifiers
        // * A static constructor cannot have any parameters
        static Animal()
        {
            Console.WriteLine("Animal initialized");
        }

        // Instance constructor, this is executed every time the class is created
        public Animal()
        {
            Console.WriteLine("Animal created");
        }

        public static void Yawn()
        {
            Console.WriteLine("Yawn!");
        }
    }

    var turtle = new Animal();
    var giraffe = new Animal();
**Production:**
 
> Animal initialisé
> Animal créé
> Animal créé

[Voir la démo][1]

Si le premier appel est une méthode statique, le constructeur statique est appelé sans le constructeur d'instance. C'est OK, car la méthode statique ne peut de toute façon pas accéder à l'état de l'instance.

    Animal.Yawn();

Cela affichera :

> Animal initialisé
> Bâillement !

Voir aussi [Exceptions dans les constructeurs statiques][2] et [Constructeurs statiques génériques][3] .


[1] : https://dotnetfiddle.net/XmExII
[2] : https://www.wikiod.com/fr/docs/c%23/25/constructors-finalizers/15007/exceptions-in-static-constructors
[3] : https://www.wikiod.com/fr/docs/c%23/25/constructors-finalizers/15003/generic-static-constructors

Exemple de singleton :

    public class SessionManager
    {
        public static SessionManager Instance;

        static SessionManager()
        {
            Instance = new SessionManager();
        }
    }

## Modèle de constructeur singleton
    public class SingletonClass
    {
        public static SingletonClass Instance { get; } = new SingletonClass();

        private SingletonClass()
        {
            // Put custom constructor code here
        }    
    }

Étant donné que le constructeur est privé, aucune nouvelle instance de `SingletonClass` ne peut être créée en consommant du code. La seule façon d'accéder à l'instance unique de `SingletonClass` consiste à utiliser la propriété statique `SingletonClass.Instance`.

La propriété `Instance` est assignée par un constructeur statique que le compilateur C# génère. Le runtime .NET garantit que le constructeur statique est exécuté au plus une fois et avant la première lecture de `Instance`. Par conséquent, tous les problèmes de synchronisation et d'initialisation sont pris en charge par le runtime.

Notez que si le constructeur statique échoue, la classe `Singleton` devient définitivement inutilisable pour la durée de vie de l'AppDomain.

De plus, il n'est pas garanti que le constructeur statique s'exécute au moment du premier accès à `Instance`. Au contraire, il s'exécutera *à un moment donné avant cela*. Cela rend le moment auquel l'initialisation se produit non déterministe. Dans des cas pratiques, le JIT appelle souvent le constructeur statique lors de la *compilation* (et non de l'exécution) d'une méthode référençant `Instance`. Il s'agit d'une optimisation des performances.

Voir la page [Singleton Implementations][1] pour d'autres façons d'implémenter le modèle singleton.


[1] : https://www.wikiod.com/fr/docs/c%23/1192/singleton-implementation#t=201607231143190778053

## Constructeur par défaut
Lorsqu'un type est défini sans constructeur :

    public class Animal
    {
    }

puis le compilateur génère un constructeur par défaut équivalent au suivant :

    public class Animal
    {
        public Animal() {}
    }

La définition de tout constructeur pour le type supprimera la génération de constructeur par défaut. Si le type était défini comme suit :

    public class Animal
    {
        public Animal(string name) {}
    }

alors un `Animal` ne peut être créé qu'en appelant le constructeur déclaré.

    // This is valid
    var myAnimal = new Animal("Fluffy");
    // This fails to compile
    var unnamedAnimal = new Animal();

Pour le deuxième exemple, le compilateur affichera un message d'erreur :
>'Animal' ne contient pas de constructeur prenant 0 argument

Si vous voulez qu'une classe ait à la fois un constructeur sans paramètre et un constructeur qui prend un paramètre, vous pouvez le faire en implémentant explicitement les deux constructeurs.

    public class Animal
    {
        
        public Animal() {} //Equivalent to a default constructor.
        public Animal(string name) {}
    }

Le compilateur ne pourra pas générer de constructeur par défaut si la classe étend une autre classe qui n'a pas de constructeur sans paramètre. Par exemple, si nous avions une classe `Creature` :

    public class Creature
    {
        public Creature(Genus genus) {}
    }

alors `Animal` défini comme `class Animal : Creature {}` ne compilerait pas.

## Forcer l'appel d'un constructeur statique
Alors que les constructeurs statiques sont toujours appelés avant la première utilisation d'un type, il est parfois utile de pouvoir les forcer à être appelés et la classe `RuntimeHelpers` fournit une aide pour cela :

    using System.Runtime.CompilerServices;    
    // ...
    RuntimeHelpers.RunClassConstructor(typeof(Foo).TypeHandle);

***Remarque* :** Toutes les initialisations statiques (les initialiseurs de champs par exemple) s'exécuteront, pas seulement le constructeur lui-même.

***Utilisations potentielles* :** Forcer l'initialisation pendant l'écran de démarrage dans une application d'interface utilisateur ou s'assurer qu'un constructeur statique n'échoue pas dans un test unitaire.

## Appel d'un constructeur depuis un autre constructeur
    public class Animal
    {
        public string Name { get; set; }

        public Animal() : this("Dog")
        {
        }

        public Animal(string name)
        {
            Name = name;
        }
    }

    var dog = new Animal();      // dog.Name will be set to "Dog" by default.
    var cat = new Animal("Cat"); // cat.Name is "Cat", the empty constructor is not called.


## Appel du constructeur de la classe de base
Un constructeur d'une classe de base est appelé avant l'exécution d'un constructeur d'une classe dérivée. Par exemple, si `Mammal` étend `Animal`, alors le code contenu dans le constructeur de `Animal` est appelé en premier lors de la création d'une instance de `Mammal`.

Si une classe dérivée ne spécifie pas explicitement quel constructeur de la classe de base doit être appelé, le compilateur assume le constructeur sans paramètre.

    public class Animal
    {
        public Animal() { Console.WriteLine("An unknown animal gets born."); }
        public Animal(string name) { Console.WriteLine(name + " gets born"); }
    }

    public class Mammal : Animal
    {
        public Mammal(string name)
        {
            Console.WriteLine(name + " is a mammal.");
        }
    }

Dans ce cas, l'instanciation d'un `Mammal` en appelant `new Mammal("George the Cat")` imprimera

> Un animal inconnu naît.
>George le chat est un mammifère.

[Voir la démo][1]

L'appel d'un constructeur différent de la classe de base se fait en plaçant `: base(args)` entre la signature du constructeur et son corps :

    public class Mammal : Animal
    {
        public Mammal(string name) : base(name)
        {
            Console.WriteLine(name + " is a mammal.");
        }
    }

L'appel de `new Mammal("George the Cat")` affichera désormais :

>George le chat est né.
>George le chat est un mammifère.

[Voir la démo][2]


[1] : https://dotnetfiddle.net/xb8Vqr
[2] : https://dotnetfiddle.net/gbdERq

## Finaliseurs sur les classes dérivées
Lorsqu'un graphe d'objets est finalisé, l'ordre est l'inverse de la construction. Par exemple. le super-type est finalisé avant le type de base comme le montre le code suivant :

    class TheBaseClass
    {
        ~TheBaseClass() 
        {
            Console.WriteLine("Base class finalized!");
        }
    }
    
    class TheDerivedClass : TheBaseClass
    {
        ~TheDerivedClass() 
        {
            Console.WriteLine("Derived class finalized!");
        }
    }

    //Don't assign to a variable
    //to make the object unreachable
    new TheDerivedClass();
    
    //Just to make the example work;
    //this is otherwise NOT recommended!
    GC.Collect();

    //Derived class finalized!
    //Base class finalized!

## Exceptions dans les constructeurs statiques
Si un constructeur statique lève une exception, il n'est jamais réessayé. Le type est inutilisable pendant la durée de vie de l'AppDomain. Toute autre utilisation du type déclenchera une `TypeInitializationException` enroulée autour de l'exception d'origine.


    public class Animal
    {
        static Animal()
        {
            Console.WriteLine("Static ctor");
            throw new Exception();
        }
    
        public static void Yawn() {}
    }

    try
    {
        Animal.Yawn();
    }
    catch (Exception e)
    {
        Console.WriteLine(e.ToString());
    }

    try
    {
        Animal.Yawn();
    }
    catch (Exception e)
    {
        Console.WriteLine(e.ToString());
    }

Cela affichera :

> Cteur statique
>
> System.TypeInitializationException : L'initialiseur de type
> pour 'Animal' a lancé une exception. ---> System.Exception : Exception de
> le type 'System.Exception' a été lancé.

[...]

> System.TypeInitializationException : L'initialiseur de type pour 'Animal'
> a lancé une exception. ---> System.Exception : Exception de type
> 'System.Exception' a été levée.

où vous pouvez voir que le constructeur réel n'est exécuté qu'une seule fois et que l'exception est réutilisée.

## Appel de méthodes virtuelles dans le constructeur
Contrairement à C++ en C#, vous pouvez appeler une méthode virtuelle à partir du constructeur de classe (OK, vous pouvez également en C++ mais le comportement au début est surprenant). Par exemple:

    abstract class Base
    {
        protected Base()
        {
            _obj = CreateAnother();
        }
    
        protected virtual AnotherBase CreateAnother()
        {
            return new AnotherBase();
        }
    
        private readonly AnotherBase _obj;
    }
    
    sealed class Derived : Base
    {
        public Derived() { }
    
        protected override AnotherBase CreateAnother()
        {
            return new AnotherDerived();
        }
    }
    
    var test = new Derived();
    // test._obj is AnotherDerived

Si vous venez d'une formation C++, cela est surprenant, le constructeur de la classe de base voit déjà la table des méthodes virtuelles de la classe dérivée !

**Attention** : la classe dérivée n'a peut-être pas encore été entièrement initialisée (son constructeur sera exécuté après le constructeur de la classe de base) et cette technique est dangereuse (il existe également un avertissement StyleCop pour cela). Ceci est généralement considéré comme une mauvaise pratique.


## Constructeurs statiques génériques
Si le type sur lequel le constructeur statique est déclaré est générique, le constructeur statique sera appelé une fois pour chaque combinaison unique d'arguments génériques.

    class Animal<T>
    {
        static Animal()
        {
            Console.WriteLine(typeof(T).FullName);
        }

        public static void Yawn() { }
    }

    Animal<Object>.Yawn();
    Animal<String>.Yawn();

Cela affichera :

> System.Object
> System.String

Voir aussi [Comment fonctionnent les constructeurs statiques pour les types génériques ?][1]

[1] : http://stackoverflow.com/q/5629388

## Initialisation du constructeur et de la propriété
L'affectation de la valeur de la propriété doit-elle être exécutée *avant* ou *après* le constructeur de la classe ?

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;
        
        public TestClass() 
        {
            if (TestProperty == 1) 
            {
                Console.WriteLine("Shall this be executed?");
            }

            if (TestProperty == 2) 
            {
                Console.WriteLine("Or shall this be executed");
            }
        }
    }

    var testInstance = new TestClass() { TestProperty = 1 };

Dans l'exemple ci-dessus, la valeur `TestProperty` doit-elle être `1` dans le constructeur de la classe ou après le constructeur de la classe ?

----

Attribuer des valeurs de propriété lors de la création de l'instance comme ceci :

    var testInstance = new TestClass() {TestProperty = 1};

Sera exécuté ***après*** l'exécution du constructeur. Cependant, initialiser la valeur de la propriété dans la propriété de la classe en C# 6.0 comme ceci :

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;

        public TestClass() 
        {
        }
    }

sera fait ***avant*** l'exécution du constructeur.

---

Combinant les deux concepts ci-dessus dans un seul exemple :

    public class TestClass 
    {
        public int TestProperty { get; set; } = 2;
        
        public TestClass() 
        {
            if (TestProperty == 1) 
            {
                Console.WriteLine("Shall this be executed?");
            }

            if (TestProperty == 2) 
            {
                Console.WriteLine("Or shall this be executed");
            }
        }
    }

    static void Main(string[] args) 
    {
        var testInstance = new TestClass() { TestProperty = 1 };
        Console.WriteLine(testInstance.TestProperty); //resulting in 1
    }

Résultat final:

    "Or shall this be executed"
    "1"

---

**Explication:**

La valeur `TestProperty` sera d'abord affectée à `2`, puis le constructeur `TestClass` sera exécuté, ce qui entraînera l'impression de

    "Or shall this be executed"
    
Et ensuite, le `TestProperty` sera attribué à `1` en raison de `new TestClass() { TestProperty = 1 }`, ce qui rendra la valeur finale de `TestProperty` imprimée par `Console.WriteLine (testInstance.TestProperty)`

    "1"



