---
title: "Réflexion"
slug: "reflexion"
draft: false
images: []
weight: 9214
type: docs
toc: true
---

La réflexion est un mécanisme du langage C# permettant d'accéder aux propriétés d'un objet dynamique lors de l'exécution. Généralement, la réflexion est utilisée pour extraire les informations sur le type d'objet dynamique et les valeurs d'attribut d'objet. Dans l'application REST, par exemple, la réflexion peut être utilisée pour parcourir un objet de réponse sérialisé.
   
Remarque:
Selon les directives de MS, le code critique pour les performances doit éviter la réflexion. Voir https://msdn.microsoft.com/en-us/library/ff647790.aspx

[Reflection][1] permet au code d'accéder aux informations sur les assemblys, les modules et les types au moment de l'exécution (exécution du programme). Cela peut ensuite être utilisé pour créer, modifier ou accéder dynamiquement à des types. Les types incluent les propriétés, les méthodes, les champs et les attributs.

Lecture complémentaire :

[Réflexion(C#)][1]

[Réflexion dans .Net Framework][2]

[1] : https://msdn.microsoft.com/en-us/library/mt656691.aspx
[2] : https://msdn.microsoft.com/en-us/library/f7ykdhsy%28v=vs.110%29.aspx


## Récupère les membres d'un type
    using System;
    using System.Reflection;
    using System.Linq;
                    
    public class Program
    {
      public static void Main()
      {
        var members = typeof(object)
                        .GetMembers(BindingFlags.Public |
                                    BindingFlags.Static |
                                    BindingFlags.Instance);
        
        foreach (var member in members)
        {
          bool inherited = member.DeclaringType.Equals( typeof(object).Name );
          Console.WriteLine($"{member.Name} is a {member.MemberType}, " +
                            $"it has {(inherited ? "":"not")} been inherited.");
        }
      }
    }

Sortie (*voir note sur l'ordre de sortie plus bas*) :

<pre>GetType est une méthode, elle n'a pas été héritée.
GetHashCode est une méthode, elle n'a pas été héritée.
ToString est une méthode, elle n'a pas été héritée.
Equals est une méthode, elle n'a pas été héritée.
Equals est une méthode, elle n'a pas été héritée.
ReferenceEquals est une méthode, elle n'a pas été héritée.
.ctor est un constructeur, il n'a pas été hérité.</pre>

Nous pouvons également utiliser `GetMembers()` sans passer de `BindingFlags`. Cela renverra *tous* les membres publics de ce type spécifique.

Une chose à noter que `GetMembers` ne renvoie pas les membres dans un ordre particulier, donc ne vous fiez jamais à l'ordre dans lequel `GetMembers` vous renvoie.

[Voir la démo][1]

[1] : https://dotnetfiddle.net/bJczwn

## Obtenir une méthode et l'invoquer
**Obtenez la méthode Instance et invoquez-la**

    using System;
                    
    public class Program
    {
        public static void Main()
        {
            var theString = "hello";
            var method = theString
                         .GetType()
                         .GetMethod("Substring",
                                    new[] {typeof(int), typeof(int)}); //The types of the method arguments
             var result = method.Invoke(theString, new object[] {0, 4});
             Console.WriteLine(result);
        }
    }

**Production:**
> enfer

[Voir la démo][1]

**Obtenez la méthode statique et invoquez-la**

En revanche, si la méthode est statique, vous n'avez pas besoin d'instance pour l'appeler.

    var method = typeof(Math).GetMethod("Exp");
    var result = method.Invoke(null, new object[] {2});//Pass null as the first argument (no need for an instance)
    Console.WriteLine(result); //You'll get e^2

**Production:**
>7.38905609893065

[Voir la démo][2]


[1] : https://dotnetfiddle.net/AF8RVe
[2] : https://dotnetfiddle.net/vNEsyk

## Création d'une instance d'un Type
Le plus simple est d'utiliser la classe `Activator`.

Cependant, même si les performances de `Activator` ont été améliorées depuis .NET 3.5, l'utilisation de `Activator.CreateInstance()` est parfois une mauvaise option, en raison de performances (relativement) faibles : [Test 1][1], [Test 2][ 2], [Essai 3][3]...


----------


Avec la classe `Activator`
------------------------

    Type type = typeof(BigInteger);
    object result = Activator.CreateInstance(type); //Requires parameterless constructor.
    Console.WriteLine(result); //Output: 0
    result = Activator.CreateInstance(type, 123); //Requires a constructor which can receive an 'int' compatible argument.
    Console.WriteLine(result); //Output: 123

Vous pouvez passer un tableau d'objets à `Activator.CreateInstance` si vous avez plusieurs paramètres.

    // With a constructor such as MyClass(int, int, string)
    Activator.CreateInstance(typeof(MyClass), new object[] { 1, 2, "Hello World" });

    Type type = typeof(someObject);
    var instance = Activator.CreateInstance(type);

**Pour un type générique**

La méthode `MakeGenericType` transforme un type générique ouvert (comme `List<>`) en un type concret (comme `List<string>`) en lui appliquant des arguments de type.

    // generic List with no parameters
    Type openType = typeof(List<>);

    // To create a List<string>
    Type[] tArgs = { typeof(string) };
    Type target = openType.MakeGenericType(tArgs);

    // Create an instance - Activator.CreateInstance will call the default constructor.
    // This is equivalent to calling new List<string>().
    List<string> result = (List<string>)Activator.CreateInstance(target);

La syntaxe `List<>` n'est pas autorisée en dehors d'une expression `typeof`.


----------


Sans la classe `Activator`
------------------------
**Utilisation du mot-clé `new` (convient pour les constructeurs sans paramètre)**

    T GetInstance<T>() where T : new()
    {
        T instance = new T();
        return instance;
    }

**Utilisation de la méthode Invoke**

    // Get the instance of the desired constructor (here it takes a string as a parameter).
    ConstructorInfo c = typeof(T).GetConstructor(new[] { typeof(string) }); 
    // Don't forget to check if such constructor exists
    if (c == null) 
        throw new InvalidOperationException(string.Format("A constructor for type '{0}' was not found.", typeof(T)));
    T instance = (T)c.Invoke(new object[] { "test" });

**Utilisation des arbres d'expression**

Les arbres d'expression représentent le code dans une structure de données arborescente, où chaque nœud est une expression.
Comme l'explique [MSDN][4] :

> L'expression est une séquence d'un ou plusieurs opérandes et zéro ou plusieurs
> opérateurs qui peuvent être évalués à une seule valeur, objet, méthode ou
> espace de noms. Les expressions peuvent consister en une valeur littérale, une méthode
> invocation, un opérateur et ses opérandes, ou un simple nom. Simple
> les noms peuvent être le nom d'une variable, un membre de type, un paramètre de méthode,
> espace de noms ou type.


    public class GenericFactory<TKey, TType>
        {
           private readonly Dictionary<TKey, Func<object[], TType>> _registeredTypes; // dictionary, that holds constructor functions.
           private object _locker = new object(); // object for locking dictionary, to guarantee thread safety
    
            public GenericFactory()
            {
                _registeredTypes = new Dictionary<TKey, Func<object[], TType>>();
            }
    
            /// <summary>
            /// Find and register suitable constructor for type
            /// </summary>
            /// <typeparam name="TType"></typeparam>
            /// <param name="key">Key for this constructor</param>
            /// <param name="parameters">Parameters</param>
            public void Register(TKey key, params Type[] parameters)
            {
                ConstructorInfo ci = typeof(TType).GetConstructor(BindingFlags.Public | BindingFlags.Instance, null, CallingConventions.HasThis, parameters, new ParameterModifier[] { }); // Get the instance of ctor.
                if (ci == null)
                    throw new InvalidOperationException(string.Format("Constructor for type '{0}' was not found.", typeof(TType)));
    
                Func<object[], TType> ctor;
    
                lock (_locker)
                {
                    if (!_registeredTypes.TryGetValue(key, out ctor)) // check if such ctor already been registered
                    {
                        var pExp = Expression.Parameter(typeof(object[]), "arguments"); // create parameter Expression
                        var ctorParams = ci.GetParameters(); // get parameter info from constructor
    
                        var argExpressions = new Expression[ctorParams.Length]; // array that will contains parameter expessions
                        for (var i = 0; i < parameters.Length; i++)
                        {
    
                            var indexedAcccess = Expression.ArrayIndex(pExp, Expression.Constant(i));
    
                            if (!parameters[i].IsClass && !parameters[i].IsInterface) // check if parameter is a value type
                            {
                                var localVariable = Expression.Variable(parameters[i], "localVariable"); // if so - we should create local variable that will store paraameter value
    
                                var block = Expression.Block(new[] { localVariable },
                                        Expression.IfThenElse(Expression.Equal(indexedAcccess, Expression.Constant(null)),
                                            Expression.Assign(localVariable, Expression.Default(parameters[i])),
                                            Expression.Assign(localVariable, Expression.Convert(indexedAcccess, parameters[i]))
                                        ),
                                        localVariable
                                    );
    
                                argExpressions[i] = block;
    
                            }
                            else
                                argExpressions[i] = Expression.Convert(indexedAcccess, parameters[i]);
                        }
                        var newExpr = Expression.New(ci, argExpressions); // create expression that represents call to specified ctor with the specified arguments.
      
                        _registeredTypes.Add(key, Expression.Lambda(newExpr, new[] { pExp }).Compile() as Func<object[], TType>); // compile expression to create delegate, and add fucntion to dictionary
                    }
                }
            }
    
            /// <summary>
            /// Returns instance of registered type by key.
            /// </summary>
            /// <typeparam name="TType"></typeparam>
            /// <param name="key"></param>
            /// <param name="args"></param>
            /// <returns></returns>
            public TType Create(TKey key, params object[] args)
            {
                Func<object[], TType> foo;
                if (_registeredTypes.TryGetValue(key, out foo))
                {
                    return (TType)foo(args);
                }
    
                throw new ArgumentException("No type registered for this key.");
            }
        }

Peut être utilisé comme ceci :

     public class TestClass
     {
            public TestClass(string parameter)
            {
                Console.Write(parameter);
            }
     } 


    public void TestMethod()
    {
           var factory = new GenericFactory<string, TestClass>();
           factory.Register("key", typeof(string));
           TestClass newInstance = factory.Create("key", "testParameter");
    }

**Utilisation de FormatterServices.GetUninitializedObject**


    T instance = (T)FormatterServices.GetUninitializedObject(typeof(T));

En cas d'utilisation de `FormatterServices.GetUninitializedObject`
les constructeurs et les initialiseurs de champs ne seront pas appelés. Il est destiné à être utilisé dans les sérialiseurs et les moteurs distants



[1] : https://blogs.msdn.microsoft.com/haibo_luo/2005/11/17/activator-createinstance-and-beyond/ "Un"
[2] : https://codingsolution.wordpress.com/2013/07/12/activator-createinstance-is-slow/
[3] : http://stackoverflow.com/questions/6069661/does-system-activator-createinstancet-have-performance-issues-big-enough-to-di
[4] : https://msdn.microsoft.com/en-us/library/ms173144.aspx "MSDN"

## Obtenez une méthode générique et invoquez-la
Disons que vous avez une classe avec des méthodes génériques. Et vous devez appeler ses fonctions avec réflexion.

    public class Sample
    {
        public void GenericMethod<T>()
        {
            // ...
        }

        public static void StaticMethod<T>()
        {
            //...
        }
    }


Disons que nous voulons appeler GenericMethod avec le type string.

    Sample sample = new Sample();//or you can get an instance via reflection

    MethodInfo method = typeof(Sample).GetMethod("GenericMethod");
    MethodInfo generic = method.MakeGenericMethod(typeof(string));
    generic.Invoke(sample, null);//Since there are no arguments, we are passing null

Pour la méthode statique, vous n'avez pas besoin d'instance. Par conséquent, le premier argument sera également nul.

    MethodInfo method = typeof(Sample).GetMethod("StaticMethod");
    MethodInfo generic = method.MakeGenericMethod(typeof(string));
    generic.Invoke(null, null);

## Obtenir un délégué fortement typé à une méthode ou une propriété via la réflexion
Lorsque les performances sont un problème, invoquer une méthode par réflexion (c'est-à-dire via la méthode `MethodInfo.Invoke`) n'est pas idéal. Cependant, il est relativement simple d'obtenir un délégué fortement typé plus performant en utilisant la fonction `Delegate.CreateDelegate`. La pénalité de performances liée à l'utilisation de la réflexion n'est encourue que pendant le processus de création de délégué. Une fois le délégué créé, il y a peu ou pas de pénalité de performance pour l'invoquer :

    // Get a MethodInfo for the Math.Max(int, int) method...
    var maxMethod = typeof(Math).GetMethod("Max", new Type[] { typeof(int), typeof(int) });
    // Now get a strongly-typed delegate for Math.Max(int, int)...
    var stronglyTypedDelegate = (Func<int, int, int>)Delegate.CreateDelegate(typeof(Func<int, int, int>), null, maxMethod);
    // Invoke the Math.Max(int, int) method using the strongly-typed delegate...
    Console.WriteLine("Max of 3 and 5 is: {0}", stronglyTypedDelegate(3, 5));

Cette technique peut également être étendue aux propriétés. Si nous avons une classe nommée `MyClass` avec une propriété `int` nommée `MyIntProperty`, le code pour obtenir un getter fortement typé serait (l'exemple suivant suppose que 'target' est une instance valide de `MyClass`):

    // Get a MethodInfo for the MyClass.MyIntProperty getter...
    var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
    var theGetter = theProperty.GetGetMethod();
    // Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
    var stronglyTypedGetter = (Func<MyClass, int>)Delegate.CreateDelegate(typeof(Func<MyClass, int>), theGetter);
    // Invoke the MyIntProperty getter against MyClass instance 'target'...
    Console.WriteLine("target.MyIntProperty is: {0}", stronglyTypedGetter(target));

... et la même chose peut être faite pour le setter :

    // Get a MethodInfo for the MyClass.MyIntProperty setter...
    var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
    var theSetter = theProperty.GetSetMethod();
    // Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
    var stronglyTypedSetter = (Action<MyClass, int>)Delegate.CreateDelegate(typeof(Action<MyClass, int>), theSetter);
    // Set MyIntProperty to 5...
    stronglyTypedSetter(target, 5);



## Obtenir un System.Type
Pour une instance d'un type :

    var theString = "hello";
    var theType = theString.GetType();

Du type lui-même:

    var theType = typeof(string);



## Obtenir et définir des propriétés
Utilisation de base :

    PropertyInfo prop = myInstance.GetType().GetProperty("myProperty");
    // get the value myInstance.myProperty
    object value = prop.GetValue(myInstance);

    int newValue = 1;
    // set the value myInstance.myProperty to newValue
    prop.setValue(myInstance, newValue);

La définition de propriétés en lecture seule implémentées automatiquement peut être effectuée via son champ de sauvegarde (dans .NET Framework, le nom du champ de sauvegarde est "<propertyName>k__BackingField") :

    // get backing field info
    FieldInfo fieldInfo = myInstance.GetType()
        .GetField("<myProperty>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic);

    int newValue = 1;
    // set the value of myInstance.myProperty backing field to newValue
    fieldInfo.SetValue(myInstance, newValue);

## Attributs personnalisés
**Rechercher des propriétés avec un attribut personnalisé** - `MyAttribute`

    var props = t.GetProperties(BindingFlags.NonPublic | BindingFlags.Public | 
                BindingFlags.Instance).Where(
                prop => Attribute.IsDefined(prop, typeof(MyAttribute)));

**Rechercher tous les attributs personnalisés d'une propriété donnée**

    var attributes = typeof(t).GetProperty("Name").GetCustomAttributes(false);

** Énumérer toutes les classes avec un attribut personnalisé ** - `MyAttribute`

    static IEnumerable<Type> GetTypesWithAttribute(Assembly assembly) {
        foreach(Type type in assembly.GetTypes()) {
            if (type.GetCustomAttributes(typeof(MyAttribute), true).Length > 0) {
                yield return type;
            }
        }
    }

**Lire la valeur d'un attribut personnalisé lors de l'exécution**

    public static class AttributeExtensions
    {
    
            /// <summary>
            /// Returns the value of a member attribute for any member in a class.
            ///     (a member is a Field, Property, Method, etc...)    
            /// <remarks>
            /// If there is more than one member of the same name in the class, it will return the first one (this applies to overloaded methods)
            /// </remarks>
            /// <example>
            /// Read System.ComponentModel Description Attribute from method 'MyMethodName' in class 'MyClass': 
            ///     var Attribute = typeof(MyClass).GetAttribute("MyMethodName", (DescriptionAttribute d) => d.Description);
            /// </example>
            /// <param name="type">The class that contains the member as a type</param>
            /// <param name="MemberName">Name of the member in the class</param>
            /// <param name="valueSelector">Attribute type and property to get (will return first instance if there are multiple attributes of the same type)</param>
            /// <param name="inherit">true to search this member's inheritance chain to find the attributes; otherwise, false. This parameter is ignored for properties and events</param>
            /// </summary>    
            public static TValue GetAttribute<TAttribute, TValue>(this Type type, string MemberName, Func<TAttribute, TValue> valueSelector, bool inherit = false) where TAttribute : Attribute
            {
                var att = type.GetMember(MemberName).FirstOrDefault().GetCustomAttributes(typeof(TAttribute), inherit).FirstOrDefault() as TAttribute;
                if (att != null)
                {
                    return valueSelector(att);
                }
                return default(TValue);
            }
        }

Usage

    //Read System.ComponentModel Description Attribute from method 'MyMethodName' in class 'MyClass'
    var Attribute = typeof(MyClass).GetAttribute("MyMethodName", (DescriptionAttribute d) => d.Description);

## Créer une instance d'un type générique et invoquer sa méthode
    var baseType = typeof(List<>);
    var genericType = baseType.MakeGenericType(typeof(String));
    var instance = Activator.CreateInstance(genericType);
    var method = genericType.GetMethod("GetHashCode");
    var result = method.Invoke(instance, new object[] { });

## Classes d'instanciation qui implémentent une interface (par exemple, l'activation d'un plugin)
Si vous souhaitez que votre application prenne en charge un système de plug-in, par exemple pour charger des plug-ins à partir d'assemblys situés dans le dossier `plugins` :
    
    interface IPlugin
    {
        string PluginDescription { get; }
        void DoWork();
    }

Cette classe serait située dans une dll séparée

    class HelloPlugin : IPlugin
    {
        public string PluginDescription => "A plugin that says Hello";
        public void DoWork()
        {
            Console.WriteLine("Hello");
        }
    }

Le chargeur de plug-in de votre application trouverait les fichiers dll, obtiendrait tous les types dans ces assemblages qui implémentent `IPlugin` et créerait des instances de ceux-ci.

        public IEnumerable<IPlugin> InstantiatePlugins(string directory)
        {
            var pluginAssemblyNames = Directory.GetFiles(directory, "*.addin.dll").Select(name => new FileInfo(name).FullName).ToArray();
            //load the assemblies into the current AppDomain, so we can instantiate the types later
            foreach (var fileName in pluginAssemblyNames)
                AppDomain.CurrentDomain.Load(File.ReadAllBytes(fileName));
            var assemblies = pluginAssemblyNames.Select(System.Reflection.Assembly.LoadFile);
            var typesInAssembly = assemblies.SelectMany(asm => asm.GetTypes());
            var pluginTypes = typesInAssembly.Where(type => typeof (IPlugin).IsAssignableFrom(type));
            return pluginTypes.Select(Activator.CreateInstance).Cast<IPlugin>(); 
        }

## Détermination des arguments génériques des instances de types génériques
Si vous avez une instance d'un type générique mais que, pour une raison quelconque, vous ne connaissez pas le type spécifique, vous souhaiterez peut-être déterminer les arguments génériques qui ont été utilisés pour créer cette instance.

Disons que quelqu'un a créé une instance de `List<T>` comme ça et la passe à une méthode :

    var myList = new List<int>();
    ShowGenericArguments(myList);

où `ShowGenericArguments` a cette signature :
   
    public void ShowGenericArguments(object o)

ainsi, au moment de la compilation, vous n'avez aucune idée des arguments génériques utilisés pour créer `o`. [Reflection](https://msdn.microsoft.com/en-us/library/system.type(v=vs.110).aspx) fournit de nombreuses méthodes pour inspecter les types génériques. Dans un premier temps, nous pouvons déterminer si le type de `o` est un type générique :

    public void ShowGenericArguments(object o)
    {
        if (o == null) return;
    
        Type t = o.GetType();
        if (!t.IsGenericType) return;
        ...

[`Type.IsGenericType`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictype(v=vs.110).aspx) renvoie `true` si le type est un type générique et "faux" sinon.

Mais ce n'est pas tout ce que nous voulons savoir. `List<>` lui-même est également un type générique. Mais nous ne voulons examiner que les instances de types *génériques construits* spécifiques. Un type générique construit est par exemple une `List<int>` qui a un type spécifique *argument* pour tous ses *paramètres* génériques.

La classe `Type` fournit deux autres propriétés, [`IsConstructedGenericType`](https://msdn.microsoft.com/en-us/library/system.type.isconstructedgenerictype(v=vs.110).aspx) et [` IsGenericTypeDefinition`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictypedefinition(v=vs.110).aspx), pour distinguer ces types génériques construits des définitions de types génériques :

    typeof(List<>).IsGenericType // true
    typeof(List<>).IsGenericTypeDefinition // true
    typeof(List<>).IsConstructedGenericType// false
  
    typeof(List<int>).IsGenericType // true
    typeof(List<int>).IsGenericTypeDefinition // false
    typeof(List<int>).IsConstructedGenericType// true

Pour énumérer les arguments génériques d'une instance, nous pouvons utiliser [`GetGenericArguments()`](https://msdn.microsoft.com/en-us/library/system.type.getgenericarguments(v=vs.110). aspx) qui renvoie un tableau `Type` contenant les arguments de type générique :

    public void ShowGenericArguments(object o)
    {
        if (o == null) return;   
        Type t = o.GetType();
        if (!t.IsConstructedGenericType) return;

        foreach(Type genericTypeArgument in t.GetGenericArguments())
            Console.WriteLine(genericTypeArgument.Name);
    }

Ainsi, l'appel ci-dessus (`ShowGenericArguments(myList)`) donne cette sortie :

    Int32

## Obtenir un type par nom avec un espace de noms
Pour ce faire, vous avez besoin d'une référence à l'assembly qui contient le type. Si vous avez un autre type disponible dont vous savez qu'il se trouve dans le même assemblage que celui que vous voulez, vous pouvez le faire :

    typeof(KnownType).Assembly.GetType(typeName);

- où `typeName` est le nom du type que vous recherchez (y compris l'espace de noms)
, et `KnownType` est le type dont vous savez qu'il se trouve dans le même assembly.

Moins efficace mais plus général est le suivant :

    Type t = null;
    foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
    {
        if (ass.FullName.StartsWith("System."))
            continue;
        t = ass.GetType(typeName);
        if (t != null)
            break;
    }
Notez la coche pour exclure l'analyse des assemblages d'espace de noms système pour accélérer la recherche. Si votre type peut en fait être un type CLR, vous devrez supprimer ces deux lignes.

S'il vous arrive d'avoir le nom de type entièrement qualifié d'assembly, y compris l'assembly, vous pouvez simplement l'obtenir avec

    Type.GetType(fullyQualifiedName);

## Boucle sur toutes les propriétés d'une classe
    Type type = obj.GetType();
    //To restrict return properties. If all properties are required don't provide flag.
    BindingFlags flags = BindingFlags.Public | BindingFlags.Instance; 
    PropertyInfo[] properties = type.GetProperties(flags);
    
    foreach (PropertyInfo property in properties)
    {
        Console.WriteLine("Name: " + property.Name + ", Value: " + property.GetValue(obj, null));
    }
    
    


