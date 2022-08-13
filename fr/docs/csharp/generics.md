---
title: "Génériques"
slug: "generiques"
draft: false
images: []
weight: 9216
type: docs
toc: true
---

## Syntaxe
- `public void SomeMethod <T> () { }`
- `public void SomeMethod<T, V>() { }`
- `public T SomeMethod<T>(IEnumerable<T> sequence) { ... }`
- `public void SomeMethod<T>() où T : new() { }`
- `public void SomeMethod<T, V>() where T : new() where V : struct { }`
- `public void SomeMethod<T>() where T: IDisposable { }`
- `public void SomeMethod<T>() where T: Foo { }`
- `public class MyClass<T> { public T Data {get ; Positionner; } }`

## Paramètres
| Paramètre(s) | Descriptif |
|---|---|
| T, V | Saisissez les espaces réservés pour les déclarations génériques |



Les génériques en C# sont pris en charge jusqu'à l'exécution : les types génériques construits avec C# verront leur sémantique générique préservée même après avoir été compilés en [CIL][1].


Cela signifie effectivement qu'en C#, il est possible de réfléchir sur les types génériques et de les voir tels qu'ils ont été déclarés ou de vérifier si un objet est une instance d'un type générique, par exemple. Cela contraste avec [effacement de type][2], où les informations de type générique sont supprimées lors de la compilation. Cela contraste également avec l'approche modèle des génériques, où plusieurs types génériques concrets deviennent plusieurs types non génériques au moment de l'exécution, et toutes les métadonnées nécessaires pour instancier davantage les définitions de types génériques d'origine sont perdues.

Soyez toutefois prudent lorsque vous réfléchissez aux types génériques : les noms des types génériques seront modifiés à la compilation, en remplaçant les crochets pointus et les noms des paramètres de type par un backtick suivi du nombre de paramètres de type générique. Ainsi, un `Dictionary<TKey, Tvalue>` sera traduit en ``Dictionary`2``.


[1] : https://en.wikipedia.org/wiki/Common_Intermediate_Language
[2] : https://en.wikipedia.org/wiki/Type_erasure

## Inférence de type implicite (méthodes)
Lors du passage d'arguments formels à une méthode générique, les arguments de type générique pertinents peuvent généralement être déduits implicitement. Si tous les types génériques peuvent être déduits, leur spécification dans la syntaxe est facultative.

Considérez la méthode générique suivante. Il a un paramètre formel et un paramètre de type générique. Il existe une relation très évidente entre eux -- le type passé en argument au paramètre de type générique doit être le même que le type au moment de la compilation de l'argument passé au paramètre formel.

    void M<T>(T obj)
    {
    }

Ces deux appels sont équivalents :

    M<object>(new object());
    M(new object());

Ces deux appels sont également équivalents :

    M<string>("");
    M("");

Et il en va de même pour ces trois appels :

    M<object>("");
    M((object) "");
    M("" as object);

---

Notez que si au moins un argument de type ne peut pas être déduit, alors tous doivent être spécifiés.

Considérez la méthode générique suivante. Le premier argument de type générique est le même que le type de l'argument formel. Mais il n'y a pas une telle relation pour le deuxième argument de type générique. Par conséquent, le compilateur n'a aucun moyen de déduire le deuxième argument de type générique dans tout appel à cette méthode.

    void X<T1, T2>(T1 obj)
    {
    }

Cela ne fonctionne plus :

    X("");

Cela ne fonctionne pas non plus, car le compilateur ne sait pas si nous spécifions le premier ou le deuxième paramètre générique (les deux seraient valides en tant qu'objet) :

    X<object>("");

Nous sommes obligés de taper les deux, comme ceci:

    X<string, object>("");

## Inférence de type (classes)
Les développeurs peuvent être surpris par le fait que l'inférence de type *ne fonctionne pas* pour les constructeurs :

    class Tuple<T1,T2>
    {
       public Tuple(T1 value1, T2 value2)
       {
       }
    }

    var x = new Tuple(2, "two");              // This WON'T work...
    var y = new Tuple<int, string>(2, "two"); // even though the explicit form will.

La première façon de créer une instance sans spécifier explicitement les paramètres de type provoquera une erreur de compilation qui dira :
>L'utilisation du type générique 'Tuple<T1, T2>' nécessite 2 arguments de type

Une solution de contournement courante consiste à ajouter une méthode d'assistance dans une classe statique :

    static class Tuple
    {
        public static Tuple<T1, T2> Create<T1, T2>(T1 value1, T2 value2)
        {
             return new Tuple<T1, T2>(value1, value2);
        }
    }

    var x = Tuple.Create(2, "two");  // This WILL work...

## Utilisation d'une méthode générique avec une interface comme type de contrainte.
Ceci est un exemple d'utilisation du type générique TFood dans la méthode Eat<TFood> sur la classe Animal

    public interface IFood
    {
        void EatenBy(Animal animal);
    }
    
    public class Grass: IFood
    {
        public void EatenBy(Animal animal)
        {
            Console.WriteLine("Grass was eaten by: {0}", animal.Name);
        }
    }
    
    public class Animal
    {
        public string Name { get; set; }
    
        public void Eat<TFood>(TFood food)
            where TFood : IFood
        {
            food.EatenBy(this);
        }
    }
    
    public class Carnivore : Animal
    {
        public Carnivore()
        {
            Name = "Carnivore";
        }
    }
    
    public class Herbivore : Animal, IFood
    {
        public Herbivore()
        {
            Name = "Herbivore";
        }
        
        public void EatenBy(Animal animal)
        {
            Console.WriteLine("Herbivore was eaten by: {0}", animal.Name);
        }
    }

Vous pouvez appeler la méthode Eat<TFood> comme ceci :

    var grass = new Grass();        
    var sheep = new Herbivore();
    var lion = new Carnivore();
        
    sheep.Eat(grass);
    //Output: Grass was eaten by: Herbivore

    lion.Eat(sheep);
    //Output: Herbivore was eaten by: Carnivore

Dans ce cas si vous essayez d'appeler :
   
    sheep.Eat(lion);

Ce ne sera pas possible car l'objet lion n'implémente pas l'interface IFood. Tenter d'effectuer l'appel ci-dessus générera une erreur de compilation : "Le type 'Carnivore' ne peut pas être utilisé comme paramètre de type 'TFood' dans le type ou la méthode générique 'Animal.Eat<TFood>(TFood)'. Il n'y a pas de référence implicite conversion de 'Carnivore' en 'IFood'."

## Contraintes de type (nouveau mot-clé)
En utilisant la contrainte `new()`, il est possible d'appliquer des paramètres de type pour définir un constructeur vide (par défaut).

    class Foo
    {
        public Foo () { }
    }

    class Bar
    {
        public Bar (string s) { ... }
    }

    class Factory<T>
        where T : new()
    {
        public T Create()
        {
            return new T();
        }
    }

    Foo f = new Factory<Foo>().Create(); // Valid.
    Bar b = new Factory<Bar>().Create(); // Invalid, Bar does not define a default/empty constructor.

Le deuxième appel à `Create()` donnera une erreur de compilation avec le message suivant :
>'Bar' doit être un type non abstrait avec un constructeur public sans paramètre afin de l'utiliser comme paramètre 'T' dans le type ou la méthode générique 'Factory<T>'

Il n'y a pas de contrainte pour un constructeur avec paramètres, seuls les constructeurs sans paramètres sont pris en charge.

## Contraintes de type (classes et interfaces)
Les contraintes de type sont capables de forcer un paramètre de type à implémenter une certaine interface ou classe.

    interface IType;
    interface IAnotherType;

    // T must be a subtype of IType
    interface IGeneric<T>
        where T : IType
    {
    }

    // T must be a subtype of IType
    class Generic<T>
        where T : IType
    {
    }

    class NonGeneric
    {
        // T must be a subtype of IType
        public void DoSomething<T>(T arg)
            where T : IType
        {
        }
    }

    // Valid definitions and expressions:
    class Type : IType { }
    class Sub : IGeneric<Type> { }
    class Sub : Generic<Type> { }
    new NonGeneric().DoSomething(new Type());

    // Invalid definitions and expressions:
    class AnotherType : IAnotherType { }
    class Sub : IGeneric<AnotherType> { }
    class Sub : Generic<AnotherType> { }
    new NonGeneric().DoSomething(new AnotherType());

Syntaxe pour plusieurs contraintes :

    class Generic<T, T1>
        where T : IType 
        where T1 : Base, new()
    {
    }

Les contraintes de type fonctionnent de la même manière que l'héritage, en ce sens qu'il est possible de spécifier plusieurs interfaces comme contraintes sur le type générique, mais une seule classe :

    class A { /* ... */ }
    class B { /* ... */ }

    interface I1 { }
    interface I2 { }

    class Generic<T>
        where T : A, I1, I2
    {
    }

    class Generic2<T>
        where T : A, B //Compilation error
    {
    }

Une autre règle est que la classe doit être ajoutée en tant que première contrainte, puis les interfaces :

    class Generic<T>
        where T : A, I1
    {
    }

    class Generic2<T>
        where T : I1, A //Compilation error
    {
    }

Toutes les contraintes déclarées doivent être satisfaites simultanément pour qu'une instanciation générique particulière fonctionne. Il n'existe aucun moyen de spécifier deux ensembles de contraintes alternatifs ou plus.

## Réflexion sur les paramètres de type
L'opérateur `typeof` fonctionne sur les paramètres de type.

    class NameGetter<T>
    {
        public string GetTypeName()
        {
            return typeof(T).Name;
        }
    }

## Covariance
Quand un `IEnumerable<T>` est-il un sous-type d'un autre `IEnumerable<T1>` ? Lorsque 'T' est un sous-type de 'T1'. `IEnumerable` est _covariant_ dans son paramètre `T`, ce qui signifie que la relation de sous-type de `IEnumerable` va dans _la même direction_ que `T`.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }

    IEnumerable<Dog> dogs = Enumerable.Empty<Dog>();
    IEnumerable<Animal> animals = dogs;  // IEnumerable<Dog> is a subtype of IEnumerable<Animal>
    // dogs = animals;  // Compilation error - IEnumerable<Animal> is not a subtype of IEnumerable<Dog>

Une instance d'un type générique covariant avec un paramètre de type donné est implicitement convertible en le même type générique avec un paramètre de type moins dérivé.

Cette relation tient parce que `IEnumerable` _produit_ `T`s mais ne les consomme pas. Un objet qui produit `Dog`s peut être utilisé comme s'il produisait `Animal`s.

Les paramètres de type covariant sont déclarés à l'aide du mot-clé `out`, car le paramètre doit être utilisé uniquement comme _output_.

    interface IEnumerable<out T> { /* ... */ }

Un paramètre de type déclaré comme covariant peut ne pas apparaître comme une entrée.

    interface Bad<out T>
    {
        void SetT(T t);  // type error
    }

Voici un exemple complet :

    using NUnit.Framework;
    
    namespace ToyStore
    {
       enum Taste { Bitter, Sweet };
    
       interface IWidget
       {
          int Weight { get; }
       }
    
       interface IFactory<out TWidget>
           where TWidget : IWidget
       {
          TWidget Create();
       }
    
       class Toy : IWidget
       {
          public int Weight { get; set; }
          public Taste Taste { get; set; }
       }
    
       class ToyFactory : IFactory<Toy>
       {
          public const int StandardWeight = 100;
          public const Taste StandardTaste = Taste.Sweet;

          public Toy Create() { return new Toy { Weight = StandardWeight, Taste = StandardTaste }; }
       }
    
       [TestFixture]
       public class GivenAToyFactory
       {
          [Test]
          public static void WhenUsingToyFactoryToMakeWidgets()
          {
             var toyFactory = new ToyFactory();
    
             //// Without out keyword, note the verbose explicit cast:
             // IFactory<IWidget> rustBeltFactory = (IFactory<IWidget>)toyFactory;
    
             // covariance: concrete being assigned to abstract (shiny and new)
             IFactory<IWidget> widgetFactory = toyFactory;
             IWidget anotherToy = widgetFactory.Create();
             Assert.That(anotherToy.Weight, Is.EqualTo(ToyFactory.StandardWeight)); // abstract contract
             Assert.That(((Toy)anotherToy).Taste, Is.EqualTo(ToyFactory.StandardTaste)); // concrete contract
          }
       }
    }
    

## Contravariance
Quand un `IComparer<T>` est-il un sous-type d'un `IComparer<T1>` différent ? Lorsque 'T1' est un sous-type de 'T'. `IComparer` est _contravariant_ dans son paramètre `T`, ce qui signifie que la relation de sous-type de `IComparer` va dans la _direction opposée_ en tant que `T`.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }

    IComparer<Animal> animalComparer = /* ... */;
    IComparer<Dog> dogComparer = animalComparer;  // IComparer<Animal> is a subtype of IComparer<Dog>
    // animalComparer = dogComparer;  // Compilation error - IComparer<Dog> is not a subtype of IComparer<Animal>

Une instance d'un type générique contravariant avec un paramètre de type donné est implicitement convertible en le même type générique avec un paramètre de type plus dérivé.

Cette relation tient parce que `IComparer` _consume_ `T`s mais ne les produit pas. Un objet qui peut comparer deux `Animaux` peut être utilisé pour comparer deux `Chien`.

Les paramètres de type contravariant sont déclarés à l'aide du mot-clé `in`, car le paramètre doit être utilisé uniquement comme _input_.

    interface IComparer<in T> { /* ... */ }

Un paramètre de type déclaré comme contravariant peut ne pas apparaître en sortie.

    interface Bad<in T>
    {
        T GetT();  // type error
    }

## Invariance
`IList<T>` n'est jamais un sous-type d'un autre `IList<T1>`. `IList` est _invariant_ dans son paramètre de type.

    class Animal { /* ... */ }
    class Dog : Animal { /* ... */ }
    
    IList<Dog> dogs = new List<Dog>();
    IList<Animal> animals = dogs;  // type error

Il n'y a pas de relation de sous-type pour les listes car vous pouvez mettre des valeurs dans une liste _et_ retirer des valeurs d'une liste.

Si `IList` était covariant, vous pourriez ajouter des éléments du _mauvais sous-type_ à une liste donnée.

    IList<Animal> animals = new List<Dog>();  // supposing this were allowed...
    animals.Add(new Giraffe());  // ... then this would also be allowed, which is bad!

Si `IList` était contravariant, vous seriez capable d'extraire des valeurs du mauvais sous-type d'une liste donnée.

    IList<Dog> dogs = new List<Animal> { new Dog(), new Giraffe() };  // if this were allowed...
    Dog dog = dogs[1];  // ... then this would be allowed, which is bad!

Les paramètres de type invariant sont déclarés en omettant les mots clés `in` et `out`.

    interface IList<T> { /* ... */ }

## Interfaces variantes
Les interfaces peuvent avoir des paramètres de type variant.

    interface IEnumerable<out T>
    {
        // ...
    }
    interface IComparer<in T>
    {
        // ...
    }

mais les classes et les structures peuvent ne pas

    class BadClass<in T1, out T2>  // not allowed
    {
    }
    
    struct BadStruct<in T1, out T2>  // not allowed
    {
    }

les déclarations de méthode génériques non plus

    class MyClass
    {
        public T Bad<out T, in T1>(T1 t1)  // not allowed
        {
            // ...
        }
    }

L'exemple ci-dessous montre plusieurs déclarations d'écart sur la même interface

    interface IFoo<in T1, out T2, T3>
    //  T1 : Contravariant type
    //  T2 : Covariant type 
    //  T3 : Invariant type
    {
        // ...
    }
    
    IFoo<Animal, Dog, int> foo1 = /* ... */;
    IFoo<Dog, Animal, int> foo2 = foo1;  
    // IFoo<Animal, Dog, int> is a subtype of IFoo<Dog, Animal, int>



## Vérification de l'égalité des valeurs génériques.
Si la logique d'une classe ou d'une méthode générique nécessite de vérifier l'égalité des valeurs ayant un type générique, utilisez `EqualityComparer<TType>.Default` [property][1] :


    public void Foo<TBar>(TBar arg1, TBar arg2)
    {
        var comparer = EqualityComparer<TBar>.Default;
        if (comparer.Equals(arg1,arg2)
        {
            ...
        }
    }

Cette approche est meilleure que d'appeler simplement la méthode `Object.Equals()`, car l'implémentation du comparateur par défaut vérifie si le type `TBar` implémente `IEquatale<TBar>` [interface][2] et si oui, appelle `IEquatable<TBar> .Equals(TBar autre)` méthode. Cela permet d'éviter le boxing/unboxing des types valeur.


[1] : https://msdn.microsoft.com/en-us/library/ms224763(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx

## Paramètres de type (interfaces)
Déclaration:

    interface IMyGenericInterface<T1, T2, T3, ...> { ... }

Utilisation (en héritage) :

    class ClassA<T1, T2, T3> : IMyGenericInterface<T1, T2, T3> { ... }

    class ClassB<T1, T2> : IMyGenericInterface<T1, T2, int> { ... }

    class ClassC<T1> : IMyGenericInterface<T1, char, int> { ... }

    class ClassD : IMyGenericInterface<bool, char, int> { ... }

Utilisation (comme type d'un paramètre) :

    void SomeMethod(IMyGenericInterface<int, char, bool> arg) { ... }

## Délégués de variante
Les délégués peuvent avoir des paramètres de type variant.

    delegate void Action<in T>(T t);    // T is an input
    delegate T Func<out T>();           // T is an output
    delegate T2 Func<in T1, out T2>();  // T1 is an input, T2 is an output

Cela découle du [principe de substitution de Liskov][1], qui stipule (entre autres) qu'une méthode D peut être considérée comme plus dérivée qu'une méthode B si :

- D a un type de retour dérivé égal ou supérieur à B
- D a des types de paramètres correspondants égaux ou plus généraux que B

Par conséquent, les affectations suivantes sont toutes sécurisées :

    Func<object, string> original = SomeMethod;
    Func<object, object> d1 = original;
    Func<string, string> d2 = original;
    Func<string, object> d3 = original;

[1] : https://en.wikipedia.org/wiki/Liskov_substitution_principle

## Types de variantes comme paramètres et valeurs de retour
Si un type covariant apparaît en sortie, le type conteneur est covariant. Produire un producteur de "T" revient à produire des "T".

    interface IReturnCovariant<out T>
    {
        IEnumerable<T> GetTs();
    }

Si un type contravariant apparaît en sortie, le type contenant est contravariant. Produire un consommateur de "T" revient à consommer des "T".

    interface IReturnContravariant<in T>
    {
        IComparer<T> GetTComparer();
    }

Si un type covariant apparaît comme entrée, le type contenant est contravariant. Consommer un producteur de "T" revient à consommer des "T".

    interface IAcceptCovariant<in T>
    {
        void ProcessTs(IEnumerable<T> ts);
    }

Si un type contravariant apparaît comme entrée, le type conteneur est covariant. Consommer un consommateur de "T" revient à produire des "T".

    interface IAcceptContravariant<out T>
    {
        void CompareTs(IComparer<T> tComparer);
    }

## Paramètres de type (classes)
Déclaration:

    class MyGenericClass<T1, T2, T3, ...>
    {
        // Do something with the type parameters.
    }

Initialisation:

    var x = new MyGenericClass<int, char, bool>();

Utilisation (comme type d'un paramètre) :

    void AnotherMethod(MyGenericClass<float, byte, char> arg) { ... }

## Paramètres de type (Méthodes)
Déclaration:

    void MyGenericMethod<T1, T2, T3>(T1 a, T2 b, T3 c)
    {
        // Do something with the type parameters.
    }

Invocation:

Il n'est pas nécessaire de fournir des arguments de type à une méthode générique, car le compilateur peut déduire implicitement le type.
    
    int x =10;
    int y =20;
    string z = "test";
    MyGenericMethod(x,y,z);

Cependant, s'il y a une ambiguïté, les méthodes génériques doivent être appelées avec des arguments de type comme

    MyGenericMethod<int, int, string>(x,y,z);



## Contraintes de type (classe et structure)
Il est possible de spécifier si l'argument type doit être ou non un type référence ou un type valeur en utilisant les contraintes respectives `class` ou `struct`. Si ces contraintes sont utilisées, elles *doivent* être définies _avant que toutes_ les autres contraintes (par exemple un type parent ou `new()`) puissent être listées.

    // TRef must be a reference type, the use of Int32, Single, etc. is invalid.
    // Interfaces are valid, as they are reference types
    class AcceptsRefType<TRef>
        where TRef : class
    {
        // TStruct must be a value type.
        public void AcceptStruct<TStruct>()
            where TStruct : struct
        {
        }

        // If multiple constraints are used along with class/struct
        // then the class or struct constraint MUST be specified first
        public void Foo<TComparableClass>()
            where TComparableClass : class, IComparable
        {
        }
    }

## Paramètres de type explicite
Il existe différents cas où vous devez spécifier explicitement les paramètres de type pour une méthode générique. Dans les deux cas ci-dessous, le compilateur n'est pas en mesure de déduire tous les paramètres de type à partir des paramètres de méthode spécifiés.

Un cas est lorsqu'il n'y a pas de paramètres :

    public void SomeMethod<T, V>() 
    {
       // No code for simplicity
    }

    SomeMethod(); // doesn't compile
    SomeMethod<int, bool>(); // compiles

Le deuxième cas est lorsqu'un (ou plusieurs) des paramètres de type ne fait pas partie des paramètres de méthode :

    public K SomeMethod<K, V>(V input)
    {
        return default(K);
    }

    int num1 = SomeMethod(3); // doesn't compile
    int num2 = SomeMethod<int>("3"); // doesn't compile
    int num3 = SomeMethod<int, string>("3"); // compiles.

## Casting de type générique
        /// <summary>
        /// Converts a data type to another data type.
        /// </summary>
        public static class Cast
        {
            /// <summary>
            /// Converts input to Type of default value or given as typeparam T
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="input">Input that need to be converted to specified type</param>
            /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
            /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
            public static T To<T>(object input, T defaultValue)
            {
                var result = defaultValue;
                try
                {
                    if (input == null || input == DBNull.Value) return result;
                    if (typeof (T).IsEnum)
                    {
                        result = (T) Enum.ToObject(typeof (T), To(input, Convert.ToInt32(defaultValue)));
                    }
                    else
                    {
                        result = (T) Convert.ChangeType(input, typeof (T));
                    }
                }
                catch (Exception ex)
                {
                    Tracer.Current.LogException(ex);
                }
    
                return result;
            }
            
            /// <summary>
            /// Converts input to Type of typeparam T
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="input">Input that need to be converted to specified type</param>
            /// <returns>Input is converted in Type of default value or given as typeparam T and returned</returns>
            public static T To<T>(object input)
            {
                return To(input, default(T));
            }
    
            
    
        }

Usages:

    std.Name = Cast.To<string>(drConnection["Name"]);
    std.Age = Cast.To<int>(drConnection["Age"]);
    std.IsPassed = Cast.To<bool>(drConnection["IsPassed"]);

    
    // Casting type using default value
    //Following both ways are correct
    // Way 1 (In following style input is converted into type of default value)
    std.Name = Cast.To(drConnection["Name"], "");
    std.Marks = Cast.To(drConnection["Marks"], 0);
    // Way 2    
    std.Name = Cast.To<string>(drConnection["Name"], "");
    std.Marks = Cast.To<int>(drConnection["Marks"], 0);

## Lecteur de configuration avec casting de type générique
        /// <summary>
        /// Read configuration values from app.config and convert to specified types
        /// </summary>
        public static class ConfigurationReader
        {
            /// <summary>
            /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="strKey">key to find value from AppSettings</param>
            /// <param name="defaultValue">defaultValue will be returned in case of value is null or any exception occures</param>
            /// <returns>AppSettings value against key is returned in Type of default value or given as typeparam T</returns>
            public static T GetConfigKeyValue<T>(string strKey, T defaultValue)
            {
                var result = defaultValue;
                try
                {
                    if (ConfigurationManager.AppSettings[strKey] != null)
                        result = (T)Convert.ChangeType(ConfigurationManager.AppSettings[strKey], typeof(T));
                }
                catch (Exception ex)
                {
                    Tracer.Current.LogException(ex);
                }
    
                return result;
            }
            /// <summary>
            /// Get value from AppSettings by key, convert to Type of default value or typeparam T and return
            /// </summary>
            /// <typeparam name="T">typeparam is the type in which value will be returned, it could be any type eg. int, string, bool, decimal etc.</typeparam>
            /// <param name="strKey">key to find value from AppSettings</param>
            /// <returns>AppSettings value against key is returned in Type given as typeparam T</returns>
            public static T GetConfigKeyValue<T>(string strKey)
            {
                return GetConfigKeyValue(strKey, default(T));
            }
    
        }

Usages:

    var timeOut = ConfigurationReader.GetConfigKeyValue("RequestTimeout", 2000);
    var url = ConfigurationReader.GetConfigKeyValue("URL", "www.someurl.com");
    var enabled = ConfigurationReader.GetConfigKeyValue("IsEnabled", false);

