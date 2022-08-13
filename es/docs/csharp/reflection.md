---
title: "Reflexión"
slug: "reflexion"
draft: false
images: []
weight: 9214
type: docs
toc: true
---

Reflection es un mecanismo del lenguaje C# para acceder a propiedades de objetos dinámicos en tiempo de ejecución. Por lo general, la reflexión se usa para obtener información sobre el tipo de objeto dinámico y los valores de los atributos del objeto. En la aplicación REST, por ejemplo, la reflexión podría usarse para iterar a través del objeto de respuesta serializado.
   
Observación:
De acuerdo con las pautas de MS, el código crítico para el rendimiento debe evitar la reflexión. Consulte https://msdn.microsoft.com/en-us/library/ff647790.aspx

[Reflexión][1] permite que el código acceda a información sobre los ensamblados, módulos y tipos en tiempo de ejecución (ejecución del programa). Esto se puede usar más para crear, modificar o acceder dinámicamente a los tipos. Los tipos incluyen propiedades, métodos, campos y atributos.

Otras lecturas :

[Reflexión (C#)][1]

[Reflexión en .Net Framework][2]

[1]: https://msdn.microsoft.com/en-us/library/mt656691.aspx
[2]: https://msdn.microsoft.com/en-us/library/f7ykdhsy%28v=vs.110%29.aspx


## Obtener los miembros de un tipo
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

Salida (*ver nota sobre el orden de salida más abajo*):

<pre>GetType es un Método, no ha sido heredado.
GetHashCode es un Método, no ha sido heredado.
ToString es un Método, no ha sido heredado.
Equals es un Método, no ha sido heredado.
Equals es un Método, no ha sido heredado.
ReferenceEquals es un Método, no ha sido heredado.
.ctor es un Constructor, no ha sido heredado.</pre>

También podemos usar `GetMembers()` sin pasar ningún `BindingFlags`. Esto devolverá *todos* los miembros públicos de ese tipo específico.

Una cosa a tener en cuenta es que `GetMembers` no devuelve los miembros en ningún orden en particular, así que nunca confíe en el orden en que `GetMembers` le devuelve.

[Ver demostración][1]

[1]: https://dotnetfiddle.net/bJczwn

## Obtener un método e invocarlo
**Obtener el método Instancia e invocarlo**

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

**Producción:**
>infierno

[Ver demostración][1]

**Obtener el método estático e invocarlo**

Por otro lado, si el método es estático, no necesita una instancia para llamarlo.

    var method = typeof(Math).GetMethod("Exp");
    var result = method.Invoke(null, new object[] {2});//Pass null as the first argument (no need for an instance)
    Console.WriteLine(result); //You'll get e^2

**Producción:**
>7.38905609893065

[Ver demostración][2]


[1]: https://dotnetfiddle.net/AF8RVe
[2]: https://dotnetfiddle.net/vNEsyk

## Creando una instancia de un Tipo
La forma más sencilla es utilizar la clase `Activator`.

Sin embargo, aunque el rendimiento de `Activator` ha mejorado desde .NET 3.5, usar `Activator.CreateInstance()` es una mala opción a veces, debido al (relativamente) bajo rendimiento: [Prueba 1][1], [Prueba 2][ 2], [Prueba 3][3]...


----------


Con la clase `Activator`
-----------------------

    Type type = typeof(BigInteger);
    object result = Activator.CreateInstance(type); //Requires parameterless constructor.
    Console.WriteLine(result); //Output: 0
    result = Activator.CreateInstance(type, 123); //Requires a constructor which can receive an 'int' compatible argument.
    Console.WriteLine(result); //Output: 123

Puede pasar una matriz de objetos a `Activator.CreateInstance` si tiene más de un parámetro.

    // With a constructor such as MyClass(int, int, string)
    Activator.CreateInstance(typeof(MyClass), new object[] { 1, 2, "Hello World" });

    Type type = typeof(someObject);
    var instance = Activator.CreateInstance(type);

**Para un tipo genérico**

El método `MakeGenericType` convierte un tipo genérico abierto (como `List<>`) en un tipo concreto (como `List<string>`) al aplicarle argumentos de tipo.

    // generic List with no parameters
    Type openType = typeof(List<>);

    // To create a List<string>
    Type[] tArgs = { typeof(string) };
    Type target = openType.MakeGenericType(tArgs);

    // Create an instance - Activator.CreateInstance will call the default constructor.
    // This is equivalent to calling new List<string>().
    List<string> result = (List<string>)Activator.CreateInstance(target);

La sintaxis `List<>` no está permitida fuera de una expresión `typeof`.


----------


Sin clase `Activator`
-----------------------
**Usando la palabra clave `nueva` (servirá para constructores sin parámetros)**

    T GetInstance<T>() where T : new()
    {
        T instance = new T();
        return instance;
    }

**Usando el método Invocar**

    // Get the instance of the desired constructor (here it takes a string as a parameter).
    ConstructorInfo c = typeof(T).GetConstructor(new[] { typeof(string) }); 
    // Don't forget to check if such constructor exists
    if (c == null) 
        throw new InvalidOperationException(string.Format("A constructor for type '{0}' was not found.", typeof(T)));
    T instance = (T)c.Invoke(new object[] { "test" });

**Usando árboles de expresión**

Los árboles de expresión representan código en una estructura de datos similar a un árbol, donde cada nodo es una expresión.
Como explica [MSDN][4]:

> La expresión es una secuencia de uno o más operandos y cero o más
> operadores que se pueden evaluar en un solo valor, objeto, método o
> espacio de nombres. Las expresiones pueden consistir en un valor literal, un método
> invocación, un operador y sus operandos, o un nombre simple. Simple
> los nombres pueden ser el nombre de una variable, miembro de tipo, parámetro de método,
> espacio de nombres o tipo.


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

Podría usarse así:

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

**Usando FormatterServices.GetUninitializedObject**


    T instance = (T)FormatterServices.GetUninitializedObject(typeof(T));

En caso de usar `FormatterServices.GetUninitializedObject`
los constructores y los inicializadores de campo no serán llamados. Está destinado a ser utilizado en serializadores y motores remotos.



[1]: https://blogs.msdn.microsoft.com/haibo_luo/2005/11/17/activator-createinstance-and-beyond/ "Uno"
[2]: https://codingsolution.wordpress.com/2013/07/12/activator-createinstance-is-slow/
[3]: http://stackoverflow.com/questions/6069661/does-system-activator-createinstancet-have-performance-issues-big-enough-to-di
[4]: https://msdn.microsoft.com/en-us/library/ms173144.aspx "MSDN"

## Obtener un método genérico e invocarlo
Digamos que tienes una clase con métodos genéricos. Y necesitas llamar a sus funciones con reflexión.

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


Digamos que queremos llamar a GenericMethod con tipo cadena.

    Sample sample = new Sample();//or you can get an instance via reflection

    MethodInfo method = typeof(Sample).GetMethod("GenericMethod");
    MethodInfo generic = method.MakeGenericMethod(typeof(string));
    generic.Invoke(sample, null);//Since there are no arguments, we are passing null

Para el método estático no necesita una instancia. Por lo tanto, el primer argumento también será nulo.

    MethodInfo method = typeof(Sample).GetMethod("StaticMethod");
    MethodInfo generic = method.MakeGenericMethod(typeof(string));
    generic.Invoke(null, null);

## Obtenga un delegado fuertemente tipado para un método o propiedad a través de la reflexión
Cuando el rendimiento es una preocupación, invocar un método a través de la reflexión (es decir, a través del método `MethodInfo.Invoke`) no es lo ideal. Sin embargo, es relativamente sencillo obtener un delegado fuertemente tipado de mayor rendimiento usando la función `Delegate.CreateDelegate`. Solo se incurre en la penalización de rendimiento por usar la reflexión durante el proceso de creación de delegados. Una vez que se crea el delegado, hay poca o ninguna penalización de rendimiento por invocarlo:

    // Get a MethodInfo for the Math.Max(int, int) method...
    var maxMethod = typeof(Math).GetMethod("Max", new Type[] { typeof(int), typeof(int) });
    // Now get a strongly-typed delegate for Math.Max(int, int)...
    var stronglyTypedDelegate = (Func<int, int, int>)Delegate.CreateDelegate(typeof(Func<int, int, int>), null, maxMethod);
    // Invoke the Math.Max(int, int) method using the strongly-typed delegate...
    Console.WriteLine("Max of 3 and 5 is: {0}", stronglyTypedDelegate(3, 5));

Esta técnica también se puede extender a las propiedades. Si tenemos una clase llamada `MyClass` con una propiedad `int` llamada `MyIntProperty`, el código para obtener un getter fuertemente tipado sería (el siguiente ejemplo asume que 'target' es una instancia válida de `MyClass`):

    // Get a MethodInfo for the MyClass.MyIntProperty getter...
    var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
    var theGetter = theProperty.GetGetMethod();
    // Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
    var stronglyTypedGetter = (Func<MyClass, int>)Delegate.CreateDelegate(typeof(Func<MyClass, int>), theGetter);
    // Invoke the MyIntProperty getter against MyClass instance 'target'...
    Console.WriteLine("target.MyIntProperty is: {0}", stronglyTypedGetter(target));

... y lo mismo se puede hacer para el colocador:

    // Get a MethodInfo for the MyClass.MyIntProperty setter...
    var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
    var theSetter = theProperty.GetSetMethod();
    // Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
    var stronglyTypedSetter = (Action<MyClass, int>)Delegate.CreateDelegate(typeof(Action<MyClass, int>), theSetter);
    // Set MyIntProperty to 5...
    stronglyTypedSetter(target, 5);



## Obtenga un tipo de sistema
Para una instancia de un tipo:

    var theString = "hello";
    var theType = theString.GetType();

Del tipo en sí:

    var theType = typeof(string);



## Obtener y establecer propiedades
Uso básico:

    PropertyInfo prop = myInstance.GetType().GetProperty("myProperty");
    // get the value myInstance.myProperty
    object value = prop.GetValue(myInstance);

    int newValue = 1;
    // set the value myInstance.myProperty to newValue
    prop.setValue(myInstance, newValue);

La configuración de propiedades de solo lectura implementadas automáticamente se puede realizar a través de su campo de respaldo (en .NET Framework, el nombre del campo de respaldo es "<propertyName>k__BackingField"):

    // get backing field info
    FieldInfo fieldInfo = myInstance.GetType()
        .GetField("<myProperty>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic);

    int newValue = 1;
    // set the value of myInstance.myProperty backing field to newValue
    fieldInfo.SetValue(myInstance, newValue);

## Atributos personalizados
**Encuentra propiedades con un atributo personalizado** - `MyAttribute`

    var props = t.GetProperties(BindingFlags.NonPublic | BindingFlags.Public | 
                BindingFlags.Instance).Where(
                prop => Attribute.IsDefined(prop, typeof(MyAttribute)));

**Encuentra todos los atributos personalizados en una propiedad dada**

    var attributes = typeof(t).GetProperty("Name").GetCustomAttributes(false);

**Enumerar todas las clases con atributo personalizado** - `MyAttribute`

    static IEnumerable<Type> GetTypesWithAttribute(Assembly assembly) {
        foreach(Type type in assembly.GetTypes()) {
            if (type.GetCustomAttributes(typeof(MyAttribute), true).Length > 0) {
                yield return type;
            }
        }
    }

**Valor de lectura de un atributo personalizado en tiempo de ejecución**

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

Uso

    //Read System.ComponentModel Description Attribute from method 'MyMethodName' in class 'MyClass'
    var Attribute = typeof(MyClass).GetAttribute("MyMethodName", (DescriptionAttribute d) => d.Description);

## Crear una instancia de un tipo genérico e invocar su método
    var baseType = typeof(List<>);
    var genericType = baseType.MakeGenericType(typeof(String));
    var instance = Activator.CreateInstance(genericType);
    var method = genericType.GetMethod("GetHashCode");
    var result = method.Invoke(instance, new object[] { });

## Creación de instancias de clases que implementan una interfaz (por ejemplo, activación de complementos)
Si desea que su aplicación admita un sistema de complementos, por ejemplo, para cargar complementos desde ensamblajes ubicados en la carpeta `plugins`:
    
    interface IPlugin
    {
        string PluginDescription { get; }
        void DoWork();
    }

Esta clase se ubicaría en un dll separado

    class HelloPlugin : IPlugin
    {
        public string PluginDescription => "A plugin that says Hello";
        public void DoWork()
        {
            Console.WriteLine("Hello");
        }
    }

El cargador de complementos de su aplicación encontraría los archivos dll, obtendría todos los tipos en esos ensamblajes que implementan `IPlugin` y crearía instancias de esos.

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

## Determinar argumentos genéricos de instancias de tipos genéricos
Si tiene una instancia de un tipo genérico pero por alguna razón no conoce el tipo específico, es posible que desee determinar los argumentos genéricos que se usaron para crear esta instancia.

Digamos que alguien creó una instancia de `List<T>` así y la pasa a un método:

    var myList = new List<int>();
    ShowGenericArguments(myList);

donde `ShowGenericArguments` tiene esta firma:
   
    public void ShowGenericArguments(object o)

por lo tanto, en el momento de la compilación, no tiene idea de qué argumentos genéricos se han utilizado para crear `o`. [Reflection](https://msdn.microsoft.com/en-us/library/system.type(v=vs.110).aspx) proporciona muchos métodos para inspeccionar tipos genéricos. Al principio, podemos determinar si el tipo de `o` es un tipo genérico:

    public void ShowGenericArguments(object o)
    {
        if (o == null) return;
    
        Type t = o.GetType();
        if (!t.IsGenericType) return;
        ...

[`Type.IsGenericType`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictype(v=vs.110).aspx) devuelve `true` si el tipo es un tipo genérico y `falso` si no.

Pero esto no es todo lo que queremos saber. `List<>` también es un tipo genérico. Pero solo queremos examinar instancias de tipos *genéricos* construidos específicos. Un tipo genérico construido es, por ejemplo, una `List<int>` que tiene un *argumento* de tipo específico para todos sus *parámetros* genéricos.

La clase `Type` proporciona dos propiedades más, [`IsConstructedGenericType`](https://msdn.microsoft.com/en-us/library/system.type.isconstructedgenerictype(v=vs.110).aspx) y [` IsGenericTypeDefinition`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictypedefinition(v=vs.110).aspx), para distinguir estos tipos genéricos construidos de las definiciones de tipos genéricos:

    typeof(List<>).IsGenericType // true
    typeof(List<>).IsGenericTypeDefinition // true
    typeof(List<>).IsConstructedGenericType// false
  
    typeof(List<int>).IsGenericType // true
    typeof(List<int>).IsGenericTypeDefinition // false
    typeof(List<int>).IsConstructedGenericType// true

Para enumerar los argumentos genéricos de una instancia, podemos usar [`GetGenericArguments()`](https://msdn.microsoft.com/en-us/library/system.type.getgenericarguments(v=vs.110). aspx) que devuelve una matriz `Type` que contiene los argumentos de tipo genérico:

    public void ShowGenericArguments(object o)
    {
        if (o == null) return;   
        Type t = o.GetType();
        if (!t.IsConstructedGenericType) return;

        foreach(Type genericTypeArgument in t.GetGenericArguments())
            Console.WriteLine(genericTypeArgument.Name);
    }

Entonces, la llamada desde arriba (`ShowGenericArguments(myList)`) da como resultado esta salida:

    Int32

## Obtener un tipo por nombre con espacio de nombres
Para hacer esto, necesita una referencia al ensamblado que contiene el tipo. Si tiene otro tipo disponible que sabe que está en el mismo ensamblaje que el que desea, puede hacer esto:

    typeof(KnownType).Assembly.GetType(typeName);

- donde `typeName` es el nombre del tipo que está buscando (incluido el espacio de nombres)
, y `KnownType` es el tipo que sabe que está en el mismo ensamblado.

Menos eficiente pero más general es el siguiente:

    Type t = null;
    foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
    {
        if (ass.FullName.StartsWith("System."))
            continue;
        t = ass.GetType(typeName);
        if (t != null)
            break;
    }
Observe la marca para excluir el análisis de ensamblados de espacio de nombres del sistema para acelerar la búsqueda. Si su tipo puede ser realmente un tipo CLR, deberá eliminar estas dos líneas.

Si tiene el nombre de tipo completamente calificado para ensamblaje, incluido el ensamblaje, simplemente puede obtenerlo con

    Type.GetType(fullyQualifiedName);

## Recorriendo todas las propiedades de una clase
    Type type = obj.GetType();
    //To restrict return properties. If all properties are required don't provide flag.
    BindingFlags flags = BindingFlags.Public | BindingFlags.Instance; 
    PropertyInfo[] properties = type.GetProperties(flags);
    
    foreach (PropertyInfo property in properties)
    {
        Console.WriteLine("Name: " + property.Name + ", Value: " + property.GetValue(obj, null));
    }
    
    


