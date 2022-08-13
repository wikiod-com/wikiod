---
title: "Reflexão"
slug: "reflexao"
draft: false
images: []
weight: 9214
type: docs
toc: true
---

Reflection é um mecanismo de linguagem C# para acessar propriedades de objetos dinâmicos em tempo de execução. Normalmente, a reflexão é usada para buscar as informações sobre o tipo de objeto dinâmico e os valores de atributo do objeto. No aplicativo REST, por exemplo, a reflexão pode ser usada para iterar por meio do objeto de resposta serializado.
   
Observação:
De acordo com as diretrizes do MS, o código crítico de desempenho deve evitar reflexão. Consulte https://msdn.microsoft.com/en-us/library/ff647790.aspx

[Reflection][1] permite que o código acesse informações sobre os assemblies, módulos e tipos em tempo de execução (execução do programa). Isso pode ser usado para criar, modificar ou acessar tipos dinamicamente. Os tipos incluem propriedades, métodos, campos e atributos.

Leitura adicional:

[Reflexão(C#)][1]

[Reflexão no .Net Framework][2]

[1]: https://msdn.microsoft.com/en-us/library/mt656691.aspx
[2]: https://msdn.microsoft.com/en-us/library/f7ykdhsy%28v=vs.110%29.aspx


## Obter os membros de um tipo
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

Saída (*veja a nota sobre a ordem de saída mais abaixo*):

<pre>GetType é um Método, não foi herdado.
GetHashCode é um Método, não foi herdado.
ToString é um Método, não foi herdado.
Equals é um Método, não foi herdado.
Equals é um Método, não foi herdado.
ReferenceEquals é um método, não foi herdado.
.ctor é um Construtor, não foi herdado.</pre>

Também podemos usar o `GetMembers()` sem passar nenhum `BindingFlags`. Isso retornará *todos* os membros públicos desse tipo específico.

Uma coisa a notar que `GetMembers` não retorna os membros em nenhuma ordem específica, então nunca confie na ordem que `GetMembers` retorna você.

[Ver demonstração][1]

[1]: https://dotnetfiddle.net/bJczwn

## Obtenha um método e invoque-o
**Obter o método Instance e invocá-lo**

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

**Resultado:**
>inferno

[Ver demonstração][1]

**Obtenha o método estático e invoque-o**

Por outro lado, se o método for estático, você não precisará de uma instância para chamá-lo.

    var method = typeof(Math).GetMethod("Exp");
    var result = method.Invoke(null, new object[] {2});//Pass null as the first argument (no need for an instance)
    Console.WriteLine(result); //You'll get e^2

**Resultado:**
>7.38905609893065

[Ver demonstração][2]


[1]: https://dotnetfiddle.net/AF8RVe
[2]: https://dotnetfiddle.net/vNEsyk

## Criando uma instância de um tipo
A maneira mais simples é usar a classe `Activator`.

No entanto, embora o desempenho do `Activator` tenha sido melhorado desde o .NET 3.5, usar `Activator.CreateInstance()` às vezes é uma má opção, devido ao desempenho (relativamente) baixo: [Test 1][1], [Test 2][ 2], [Teste 3][3]...


----------


Com classe 'Ativador'
-----------------------

    Type type = typeof(BigInteger);
    object result = Activator.CreateInstance(type); //Requires parameterless constructor.
    Console.WriteLine(result); //Output: 0
    result = Activator.CreateInstance(type, 123); //Requires a constructor which can receive an 'int' compatible argument.
    Console.WriteLine(result); //Output: 123

Você pode passar um array de objetos para `Activator.CreateInstance` se tiver mais de um parâmetro.

    // With a constructor such as MyClass(int, int, string)
    Activator.CreateInstance(typeof(MyClass), new object[] { 1, 2, "Hello World" });

    Type type = typeof(someObject);
    var instance = Activator.CreateInstance(type);

**Para um tipo genérico**

O método `MakeGenericType` transforma um tipo genérico aberto (como `List<>`) em um tipo concreto (como `List<string>`) aplicando argumentos de tipo a ele.

    // generic List with no parameters
    Type openType = typeof(List<>);

    // To create a List<string>
    Type[] tArgs = { typeof(string) };
    Type target = openType.MakeGenericType(tArgs);

    // Create an instance - Activator.CreateInstance will call the default constructor.
    // This is equivalent to calling new List<string>().
    List<string> result = (List<string>)Activator.CreateInstance(target);

A sintaxe `List<>` não é permitida fora de uma expressão `typeof`.


----------


Sem classe 'Ativador'
-----------------------
**Usando a palavra-chave `new` (vai servir para construtores sem parâmetros)**

    T GetInstance<T>() where T : new()
    {
        T instance = new T();
        return instance;
    }

**Usando o método Invoke**

    // Get the instance of the desired constructor (here it takes a string as a parameter).
    ConstructorInfo c = typeof(T).GetConstructor(new[] { typeof(string) }); 
    // Don't forget to check if such constructor exists
    if (c == null) 
        throw new InvalidOperationException(string.Format("A constructor for type '{0}' was not found.", typeof(T)));
    T instance = (T)c.Invoke(new object[] { "test" });

**Usando árvores de expressão**

As árvores de expressão representam o código em uma estrutura de dados semelhante a uma árvore, onde cada nó é uma expressão.
Como [MSDN][4] explica:

> Expressão é uma sequência de um ou mais operandos e zero ou mais
> operadores que podem ser avaliados para um único valor, objeto, método ou
> espaço de nomes. As expressões podem consistir em um valor literal, um método
> invocação, um operador e seus operandos, ou um simples nome. Simples
> nomes podem ser o nome de uma variável, membro de tipo, parâmetro de método,
> namespace ou tipo.


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

Poderia ser usado assim:

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

No caso de usar `FormatterServices.GetUninitializedObject`
construtores e inicializadores de campo não serão chamados. Destina-se a ser usado em serializadores e mecanismos remotos



[1]: https://blogs.msdn.microsoft.com/haibo_luo/2005/11/17/activator-createinstance-and-beyond/ "One"
[2]: https://codingsolution.wordpress.com/2013/07/12/activator-createinstance-is-slow/
[3]: http://stackoverflow.com/questions/6069661/does-system-activator-createinstancet-have-performance-issues-big-enough-to-di
[4]: https://msdn.microsoft.com/en-us/library/ms173144.aspx "MSDN"

## Obtenha um método genérico e invoque-o
Digamos que você tenha uma classe com métodos genéricos. E você precisa chamar suas funções com reflexão.

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


Digamos que queremos chamar o GenericMethod com o tipo string.

    Sample sample = new Sample();//or you can get an instance via reflection

    MethodInfo method = typeof(Sample).GetMethod("GenericMethod");
    MethodInfo generic = method.MakeGenericMethod(typeof(string));
    generic.Invoke(sample, null);//Since there are no arguments, we are passing null

Para o método estático, você não precisa de uma instância. Portanto, o primeiro argumento também será nulo.

    MethodInfo method = typeof(Sample).GetMethod("StaticMethod");
    MethodInfo generic = method.MakeGenericMethod(typeof(string));
    generic.Invoke(null, null);

## Obtenha um delegado fortemente tipado para um método ou propriedade via reflexão
Quando o desempenho é uma preocupação, invocar um método via reflexão (ou seja, através do método `MethodInfo.Invoke`) não é o ideal. No entanto, é relativamente simples obter um delegado fortemente tipado com melhor desempenho usando a função `Delegate.CreateDelegate`. A penalidade de desempenho por usar reflexão ocorre apenas durante o processo de criação de delegado. Depois que o delegado é criado, há pouca ou nenhuma penalidade de desempenho por invocá-lo:

    // Get a MethodInfo for the Math.Max(int, int) method...
    var maxMethod = typeof(Math).GetMethod("Max", new Type[] { typeof(int), typeof(int) });
    // Now get a strongly-typed delegate for Math.Max(int, int)...
    var stronglyTypedDelegate = (Func<int, int, int>)Delegate.CreateDelegate(typeof(Func<int, int, int>), null, maxMethod);
    // Invoke the Math.Max(int, int) method using the strongly-typed delegate...
    Console.WriteLine("Max of 3 and 5 is: {0}", stronglyTypedDelegate(3, 5));

Essa técnica também pode ser estendida para propriedades. Se tivermos uma classe chamada `MyClass` com uma propriedade `int` chamada `MyIntProperty`, o código para obter um getter fortemente tipado seria (o exemplo a seguir assume que 'target' é uma instância válida de `MyClass`):

    // Get a MethodInfo for the MyClass.MyIntProperty getter...
    var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
    var theGetter = theProperty.GetGetMethod();
    // Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
    var stronglyTypedGetter = (Func<MyClass, int>)Delegate.CreateDelegate(typeof(Func<MyClass, int>), theGetter);
    // Invoke the MyIntProperty getter against MyClass instance 'target'...
    Console.WriteLine("target.MyIntProperty is: {0}", stronglyTypedGetter(target));

...e o mesmo pode ser feito para o setter:

    // Get a MethodInfo for the MyClass.MyIntProperty setter...
    var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
    var theSetter = theProperty.GetSetMethod();
    // Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
    var stronglyTypedSetter = (Action<MyClass, int>)Delegate.CreateDelegate(typeof(Action<MyClass, int>), theSetter);
    // Set MyIntProperty to 5...
    stronglyTypedSetter(target, 5);



## Obtenha um System.Type
Para uma instância de um tipo:

    var theString = "hello";
    var theType = theString.GetType();

Do próprio tipo:

    var theType = typeof(string);



## Obtendo e configurando propriedades
Uso básico:

    PropertyInfo prop = myInstance.GetType().GetProperty("myProperty");
    // get the value myInstance.myProperty
    object value = prop.GetValue(myInstance);

    int newValue = 1;
    // set the value myInstance.myProperty to newValue
    prop.setValue(myInstance, newValue);

A configuração de propriedades implementadas automaticamente somente leitura pode ser feita por meio de seu campo de apoio (no .NET Framework o nome do campo de apoio é "<propertyName>k__BackingField"):

    // get backing field info
    FieldInfo fieldInfo = myInstance.GetType()
        .GetField("<myProperty>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic);

    int newValue = 1;
    // set the value of myInstance.myProperty backing field to newValue
    fieldInfo.SetValue(myInstance, newValue);

## Atributos personalizados
**Encontre propriedades com um atributo personalizado** - `MyAttribute`

    var props = t.GetProperties(BindingFlags.NonPublic | BindingFlags.Public | 
                BindingFlags.Instance).Where(
                prop => Attribute.IsDefined(prop, typeof(MyAttribute)));

**Encontre todos os atributos personalizados em uma determinada propriedade**

    var attributes = typeof(t).GetProperty("Name").GetCustomAttributes(false);

**Enumere todas as classes com atributo personalizado** - `MyAttribute`

    static IEnumerable<Type> GetTypesWithAttribute(Assembly assembly) {
        foreach(Type type in assembly.GetTypes()) {
            if (type.GetCustomAttributes(typeof(MyAttribute), true).Length > 0) {
                yield return type;
            }
        }
    }

**Ler o valor de um atributo personalizado em tempo de execução**

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

## Crie uma instância de um tipo genérico e invoque seu método
    var baseType = typeof(List<>);
    var genericType = baseType.MakeGenericType(typeof(String));
    var instance = Activator.CreateInstance(genericType);
    var method = genericType.GetMethod("GetHashCode");
    var result = method.Invoke(instance, new object[] { });

## Instanciando classes que implementam uma interface (por exemplo, ativação de plugin)
Se você deseja que seu aplicativo suporte um sistema de plug-in, por exemplo, para carregar plug-ins de assemblies localizados na pasta `plugins`:
    
    interface IPlugin
    {
        string PluginDescription { get; }
        void DoWork();
    }

Esta classe estaria localizada em uma dll separada

    class HelloPlugin : IPlugin
    {
        public string PluginDescription => "A plugin that says Hello";
        public void DoWork()
        {
            Console.WriteLine("Hello");
        }
    }

O carregador de plugins do seu aplicativo encontraria os arquivos dll, obteria todos os tipos nesses assemblies que implementam o `IPlugin` e criaria instâncias deles.

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

## Determinando argumentos genéricos de instâncias de tipos genéricos
Se você tiver uma instância de um tipo genérico, mas por algum motivo não souber o tipo específico, convém determinar os argumentos genéricos que foram usados ​​para criar essa instância.

Digamos que alguém criou uma instância de `List<T>` assim e a passa para um método:

    var myList = new List<int>();
    ShowGenericArguments(myList);

onde `ShowGenericArguments` tem esta assinatura:
   
    public void ShowGenericArguments(object o)

então em tempo de compilação você não tem ideia de quais argumentos genéricos foram usados ​​para criar `o`. [Reflection](https://msdn.microsoft.com/en-us/library/system.type(v=vs.110).aspx) fornece muitos métodos para inspecionar tipos genéricos. Em primeiro lugar, podemos determinar se o tipo de `o` é um tipo genérico:

    public void ShowGenericArguments(object o)
    {
        if (o == null) return;
    
        Type t = o.GetType();
        if (!t.IsGenericType) return;
        ...

[`Type.IsGenericType`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictype(v=vs.110).aspx) retorna `true` se o tipo for um tipo genérico e `false` se não.

Mas isso não é tudo o que queremos saber. O próprio `List<>` também é um tipo genérico. Mas queremos apenas examinar instâncias de tipos específicos *construídos genéricos*. Um tipo genérico construído é, por exemplo, uma `List<int>` que possui um tipo específico *argumento* para todos os seus *parâmetros* genéricos.

A classe `Type` fornece mais duas propriedades, [`IsConstructedGenericType`](https://msdn.microsoft.com/en-us/library/system.type.isconstructedgenerictype(v=vs.110).aspx) e [` IsGenericTypeDefinition`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictypedefinition(v=vs.110).aspx), para distinguir esses tipos genéricos construídos de definições de tipo genérico:

    typeof(List<>).IsGenericType // true
    typeof(List<>).IsGenericTypeDefinition // true
    typeof(List<>).IsConstructedGenericType// false
  
    typeof(List<int>).IsGenericType // true
    typeof(List<int>).IsGenericTypeDefinition // false
    typeof(List<int>).IsConstructedGenericType// true

Para enumerar os argumentos genéricos de uma instância, podemos usar o [`GetGenericArguments()`](https://msdn.microsoft.com/en-us/library/system.type.getgenericarguments(v=vs.110). aspx) que retorna um array `Type` contendo os argumentos de tipo genérico:

    public void ShowGenericArguments(object o)
    {
        if (o == null) return;   
        Type t = o.GetType();
        if (!t.IsConstructedGenericType) return;

        foreach(Type genericTypeArgument in t.GetGenericArguments())
            Console.WriteLine(genericTypeArgument.Name);
    }

Portanto, a chamada acima (`ShowGenericArguments(myList)`) resulta nesta saída:

    Int32

## Obtenha um tipo por nome com namespace
Para fazer isso, você precisa de uma referência ao assembly que contém o tipo. Se você tiver outro tipo disponível que você sabe que está no mesmo assembly que você deseja, você pode fazer isso:

    typeof(KnownType).Assembly.GetType(typeName);

- onde `typeName` é o nome do tipo que você está procurando (incluindo o namespace)
, e `KnownType` é o tipo que você sabe que está no mesmo assembly.

Menos eficiente, mas mais geral é o seguinte:

    Type t = null;
    foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
    {
        if (ass.FullName.StartsWith("System."))
            continue;
        t = ass.GetType(typeName);
        if (t != null)
            break;
    }
Observe a verificação para excluir os assemblies de namespace do sistema de varredura para acelerar a pesquisa. Se o seu tipo pode realmente ser um tipo CLR, você terá que excluir essas duas linhas.

Se acontecer de você ter o nome do tipo totalmente qualificado para assembly, incluindo o assembly, você pode simplesmente obtê-lo com

    Type.GetType(fullyQualifiedName);

## Percorrendo todas as propriedades de uma classe
    Type type = obj.GetType();
    //To restrict return properties. If all properties are required don't provide flag.
    BindingFlags flags = BindingFlags.Public | BindingFlags.Instance; 
    PropertyInfo[] properties = type.GetProperties(flags);
    
    foreach (PropertyInfo property in properties)
    {
        Console.WriteLine("Name: " + property.Name + ", Value: " + property.GetValue(obj, null));
    }
    
    


