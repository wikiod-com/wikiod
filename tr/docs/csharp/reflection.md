---
title: "Refleks"
slug: "refleks"
draft: false
images: []
weight: 9214
type: docs
toc: true
---

Yansıma, çalışma zamanında dinamik nesne özelliklerine erişmek için bir C# dili mekanizmasıdır. Tipik olarak yansıma, dinamik nesne türü ve nesne öznitelik değerleri hakkında bilgi almak için kullanılır. Örneğin, REST uygulamasında yansıma, serileştirilmiş yanıt nesnesini yinelemek için kullanılabilir.
   
Açıklama:
MS yönergelerine göre performans açısından kritik kod yansımadan kaçınmalıdır. https://msdn.microsoft.com/en-us/library/ff647790.aspx adresine bakın.

[Yansıma][1], çalışma zamanında (program yürütme) kodun derlemeler, modüller ve türler hakkındaki bilgilere erişmesine izin verir. Bu daha sonra türleri dinamik olarak oluşturmak, değiştirmek veya bunlara erişmek için kullanılabilir. Türler, özellikleri, yöntemleri, alanları ve nitelikleri içerir.

Daha fazla okuma :

[Yansıma(C#)][1]

[.Net Framework'te Yansıma[2]

[1]: https://msdn.microsoft.com/en-us/library/mt656691.aspx
[2]: https://msdn.microsoft.com/en-us/library/f7ykdhsy%28v=vs.110%29.aspx


## Bir türün üyelerini alın
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

Çıktı (*aşağıdaki çıktı sırası hakkındaki nota bakın*):

<pre>GetType bir Yöntemdir, miras alınmamıştır.
GetHashCode bir Yöntemdir, miras alınmamıştır.
ToString bir Yöntemdir, miras alınmamıştır.
Equals bir Yöntemdir, miras alınmamıştır.
Equals bir Yöntemdir, miras alınmamıştır.
ReferenceEquals bir Yöntemdir, miras alınmamıştır.
.ctor bir Yapıcıdır, miras alınmamıştır.</pre>

GetMembers()'i herhangi bir 'BindingFlags' geçmeden de kullanabiliriz. Bu, söz konusu türün *tüm* genel üyelerini döndürür.

Unutulmaması gereken bir nokta, 'GetMembers' üyeleri belirli bir sırayla iade etmez, bu nedenle asla 'GetMembers'ın size geri verdiği sıraya güvenmeyin.

[Demoyu Görüntüle][1]

[1]: https://dotnetfiddle.net/bJczwn

## Bir yöntem alın ve onu çağırın
**Örnek yöntemini alın ve çağırın**

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

**Çıktı:**
>cehennem

[Demoyu Görüntüle][1]

**Statik yöntemi alın ve çağırın**

Öte yandan, yöntem statik ise, onu çağırmak için bir örneğe ihtiyacınız yoktur.

    var method = typeof(Math).GetMethod("Exp");
    var result = method.Invoke(null, new object[] {2});//Pass null as the first argument (no need for an instance)
    Console.WriteLine(result); //You'll get e^2

**Çıktı:**
>7.38905609893065

[Demoyu Görüntüle][2]


[1]: https://dotnetfiddle.net/AF8RVe
[2]: https://dotnetfiddle.net/vNEsyk

## Bir Türün örneğini oluşturma
En basit yol, Activator sınıfını kullanmaktır.

Ancak, 'Activator' performansı .NET 3.5'ten beri geliştirilmiş olsa da, 'Activator.CreateInstance()' kullanmak (nispeten) düşük performans nedeniyle bazen kötü bir seçenektir: [Test 1][1], [Test 2][ 2], [Test 3][3]...


----------


'Activator' sınıfı ile
--------------------------

    Type type = typeof(BigInteger);
    object result = Activator.CreateInstance(type); //Requires parameterless constructor.
    Console.WriteLine(result); //Output: 0
    result = Activator.CreateInstance(type, 123); //Requires a constructor which can receive an 'int' compatible argument.
    Console.WriteLine(result); //Output: 123

Birden fazla parametreniz varsa, bir nesne dizisini Activator.CreateInstance'a iletebilirsiniz.

    // With a constructor such as MyClass(int, int, string)
    Activator.CreateInstance(typeof(MyClass), new object[] { 1, 2, "Hello World" });

    Type type = typeof(someObject);
    var instance = Activator.CreateInstance(type);

**Genel bir tür için**

"MakeGenericType" yöntemi, açık bir genel türü ("List<>" gibi) ona tür bağımsız değişkenleri uygulayarak somut bir türe ("List<string>" gibi) dönüştürür.

    // generic List with no parameters
    Type openType = typeof(List<>);

    // To create a List<string>
    Type[] tArgs = { typeof(string) };
    Type target = openType.MakeGenericType(tArgs);

    // Create an instance - Activator.CreateInstance will call the default constructor.
    // This is equivalent to calling new List<string>().
    List<string> result = (List<string>)Activator.CreateInstance(target);

Bir "typeof" ifadesinin dışında "List<>" sözdizimine izin verilmez.


----------


'Activator' sınıfı olmadan
--------------------------
**`new` anahtar kelimesini kullanmak (parametresiz kurucular için geçerlidir)**

    T GetInstance<T>() where T : new()
    {
        T instance = new T();
        return instance;
    }

**Invoke yöntemini kullanma**

    // Get the instance of the desired constructor (here it takes a string as a parameter).
    ConstructorInfo c = typeof(T).GetConstructor(new[] { typeof(string) }); 
    // Don't forget to check if such constructor exists
    if (c == null) 
        throw new InvalidOperationException(string.Format("A constructor for type '{0}' was not found.", typeof(T)));
    T instance = (T)c.Invoke(new object[] { "test" });

**İfade ağaçlarını kullanma**

İfade ağaçları, her düğümün bir ifade olduğu ağaç benzeri bir veri yapısındaki kodu temsil eder.
[MSDN][4]'ün açıkladığı gibi:

> İfade, bir veya daha fazla işlenen ve sıfır veya daha fazla dizidir.
> tek bir değer, nesne, yöntem veya
> ad alanı. İfadeler değişmez bir değerden, bir yöntemden oluşabilir.
> çağırma, bir operatör ve işlenenleri veya basit bir ad. Basit
> isimler bir değişkenin adı, tür üyesi, yöntem parametresi,
> ad alanı veya tür.


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

Şu şekilde kullanılabilir:

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

** FormatterServices.GetUninitializedObject'i kullanma**


    T instance = (T)FormatterServices.GetUninitializedObject(typeof(T));

`FormatterServices.GetUninitializedObject` kullanılması durumunda
yapıcılar ve alan başlatıcılar çağrılmayacak. Serileştiricilerde ve uzaktan kumanda motorlarında kullanılmak içindir.



[1]: https://blogs.msdn.microsoft.com/haibo_luo/2005/11/17/activator-createinstance-and-beyond/ "Bir"
[2]: https://codingsolution.wordpress.com/2013/07/12/activator-createinstance-is-slow/
[3]: http://stackoverflow.com/questions/6069661/does-system-activator-createinstancet-have-performance-issues-big-enough-to-di
[4]: https://msdn.microsoft.com/en-us/library/ms173144.aspx "MSDN"

## Genel bir yöntem alın ve onu çağırın
Diyelim ki genel yöntemlerle sınıfınız var. Ve işlevlerini yansıma ile çağırmanız gerekir.

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


Diyelim ki GenericMethod'u type string ile çağırmak istiyoruz.

    Sample sample = new Sample();//or you can get an instance via reflection

    MethodInfo method = typeof(Sample).GetMethod("GenericMethod");
    MethodInfo generic = method.MakeGenericMethod(typeof(string));
    generic.Invoke(sample, null);//Since there are no arguments, we are passing null

Statik yöntem için bir örneğe ihtiyacınız yoktur. Bu nedenle, ilk argüman da boş olacaktır.

    MethodInfo method = typeof(Sample).GetMethod("StaticMethod");
    MethodInfo generic = method.MakeGenericMethod(typeof(string));
    generic.Invoke(null, null);

## Yansıma Yoluyla Bir Yönteme veya Özelliğe Kesin Yazılan Delege Alma
Performans söz konusu olduğunda, yansıtma yoluyla bir yöntemi (yani, "MethodInfo.Invoke" yöntemi aracılığıyla) çağırmak ideal değildir. Ancak, 'Delegate.CreateDelegate' işlevini kullanarak daha performanslı, kesin olarak yazılmış bir temsilci elde etmek nispeten kolaydır. Yansıma kullanmanın performans cezası, yalnızca temsilci oluşturma işlemi sırasında ortaya çıkar. Temsilci oluşturulduktan sonra, onu çağırmak için çok az veya hiç performans cezası yoktur:

    // Get a MethodInfo for the Math.Max(int, int) method...
    var maxMethod = typeof(Math).GetMethod("Max", new Type[] { typeof(int), typeof(int) });
    // Now get a strongly-typed delegate for Math.Max(int, int)...
    var stronglyTypedDelegate = (Func<int, int, int>)Delegate.CreateDelegate(typeof(Func<int, int, int>), null, maxMethod);
    // Invoke the Math.Max(int, int) method using the strongly-typed delegate...
    Console.WriteLine("Max of 3 and 5 is: {0}", stronglyTypedDelegate(3, 5));

Bu teknik özelliklere de genişletilebilir. "MyIntProperty" adlı bir "int" özelliğine sahip "MyClass" adlı bir sınıfımız varsa, kesin olarak yazılmış bir alıcı elde etmek için gereken kod şöyle olacaktır (aşağıdaki örnek "hedef" öğesinin "MyClass"ın geçerli bir örneği olduğunu varsayar):

    // Get a MethodInfo for the MyClass.MyIntProperty getter...
    var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
    var theGetter = theProperty.GetGetMethod();
    // Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
    var stronglyTypedGetter = (Func<MyClass, int>)Delegate.CreateDelegate(typeof(Func<MyClass, int>), theGetter);
    // Invoke the MyIntProperty getter against MyClass instance 'target'...
    Console.WriteLine("target.MyIntProperty is: {0}", stronglyTypedGetter(target));

... ve aynısı ayarlayıcı için de yapılabilir:

    // Get a MethodInfo for the MyClass.MyIntProperty setter...
    var theProperty = typeof(MyClass).GetProperty("MyIntProperty");
    var theSetter = theProperty.GetSetMethod();
    // Now get a strongly-typed delegate for MyIntProperty that can be executed against any MyClass instance...
    var stronglyTypedSetter = (Action<MyClass, int>)Delegate.CreateDelegate(typeof(Action<MyClass, int>), theSetter);
    // Set MyIntProperty to 5...
    stronglyTypedSetter(target, 5);



## Bir System.Type Alın
Bir tür örneği için:

    var theString = "hello";
    var theType = theString.GetType();

Türün kendisinden:

    var theType = typeof(string);



## Özellikleri alma ve ayarlama
Temel kullanım:

    PropertyInfo prop = myInstance.GetType().GetProperty("myProperty");
    // get the value myInstance.myProperty
    object value = prop.GetValue(myInstance);

    int newValue = 1;
    // set the value myInstance.myProperty to newValue
    prop.setValue(myInstance, newValue);

Salt okunur, otomatik olarak uygulanan özelliklerin ayarlanması, destek alanı aracılığıyla yapılabilir (.NET Framework'te destek alanının adı "<propertyName>k__BackingField"):

    // get backing field info
    FieldInfo fieldInfo = myInstance.GetType()
        .GetField("<myProperty>k__BackingField", BindingFlags.Instance | BindingFlags.NonPublic);

    int newValue = 1;
    // set the value of myInstance.myProperty backing field to newValue
    fieldInfo.SetValue(myInstance, newValue);

## Özel Nitelikler
**Özel özniteliğe sahip özellikleri bulun** - "MyAttribute"

    var props = t.GetProperties(BindingFlags.NonPublic | BindingFlags.Public | 
                BindingFlags.Instance).Where(
                prop => Attribute.IsDefined(prop, typeof(MyAttribute)));

**Belirli bir mülkteki tüm özel özellikleri bulun**

    var attributes = typeof(t).GetProperty("Name").GetCustomAttributes(false);

**Tüm sınıfları özel öznitelikle numaralandır** - "MyAttribute"

    static IEnumerable<Type> GetTypesWithAttribute(Assembly assembly) {
        foreach(Type type in assembly.GetTypes()) {
            if (type.GetCustomAttributes(typeof(MyAttribute), true).Length > 0) {
                yield return type;
            }
        }
    }

**Çalışma zamanında özel bir özelliğin değerini okuyun**

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

kullanım

    //Read System.ComponentModel Description Attribute from method 'MyMethodName' in class 'MyClass'
    var Attribute = typeof(MyClass).GetAttribute("MyMethodName", (DescriptionAttribute d) => d.Description);

## Bir Genel Tür örneği oluşturun ve yöntemini çağırın
    var baseType = typeof(List<>);
    var genericType = baseType.MakeGenericType(typeof(String));
    var instance = Activator.CreateInstance(genericType);
    var method = genericType.GetMethod("GetHashCode");
    var result = method.Invoke(instance, new object[] { });

## Bir arabirim uygulayan sınıfları örnekleme (ör. eklenti aktivasyonu)
Uygulamanızın bir eklenti sistemini desteklemesini istiyorsanız, örneğin eklentileri "eklentiler" klasöründe bulunan derlemelerden yüklemek için:
    
    interface IPlugin
    {
        string PluginDescription { get; }
        void DoWork();
    }

Bu sınıf ayrı bir dll'de bulunur

    class HelloPlugin : IPlugin
    {
        public string PluginDescription => "A plugin that says Hello";
        public void DoWork()
        {
            Console.WriteLine("Hello");
        }
    }

Uygulamanızın eklenti yükleyicisi dll dosyalarını bulur, "IPlugin" uygulayan bu derlemelerdeki tüm türleri alır ve bunların örneklerini oluşturur.

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

## Genel tür örneklerinin genel argümanlarını belirleme
Genel bir tür örneğiniz varsa ancak belirli bir türü bilmiyorsanız, bu örneği oluşturmak için kullanılan genel bağımsız değişkenleri belirlemek isteyebilirsiniz.

Diyelim ki birisi şöyle bir `List<T>` örneği yarattı ve onu bir metoda iletti:

    var myList = new List<int>();
    ShowGenericArguments(myList);

ShowGenericArguments'ın şu imzaya sahip olduğu yer:
   
    public void ShowGenericArguments(object o)

bu nedenle derleme zamanında 'o' oluşturmak için hangi genel argümanların kullanıldığı hakkında hiçbir fikriniz yok. [Yansıma](https://msdn.microsoft.com/en-us/library/system.type(v=vs.110).aspx), genel türleri denetlemek için birçok yöntem sağlar. İlk başta, "o" türünün genel bir tür olup olmadığını belirleyebiliriz:

    public void ShowGenericArguments(object o)
    {
        if (o == null) return;
    
        Type t = o.GetType();
        if (!t.IsGenericType) return;
        ...

[`Type.IsGenericType`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictype(v=vs.110).aspx), tür genel bir türse "true" değerini döndürür ve değilse "yanlış".

Ama bilmek istediğimiz tek şey bu değil. `List<>` de genel bir türdür. Ancak biz yalnızca belirli *inşa edilmiş jenerik* türlerin örneklerini incelemek istiyoruz. Oluşturulan bir genel tür, örneğin, tüm genel *parametreleri* için belirli bir *argüman* türüne sahip bir "Liste<int>"dir.

'Type' sınıfı iki özellik daha sağlar, ['IsConstructedGenericType'](https://msdn.microsoft.com/en-us/library/system.type.isconstructedgenerictype(v=vs.110).aspx) ve [` IsGenericTypeDefinition`](https://msdn.microsoft.com/en-us/library/system.type.isgenerictypedefinition(v=vs.110).aspx), bu oluşturulmuş genel türleri genel tür tanımlarından ayırt etmek için:

    typeof(List<>).IsGenericType // true
    typeof(List<>).IsGenericTypeDefinition // true
    typeof(List<>).IsConstructedGenericType// false
  
    typeof(List<int>).IsGenericType // true
    typeof(List<int>).IsGenericTypeDefinition // false
    typeof(List<int>).IsConstructedGenericType// true

Bir örneğin genel argümanlarını numaralandırmak için [`GetGenericArguments()`](https://msdn.microsoft.com/en-us/library/system.type.getgenericarguments(v=vs.110) kullanabiliriz. aspx) genel tür bağımsız değişkenlerini içeren bir "Tür" dizisi döndüren yöntem:

    public void ShowGenericArguments(object o)
    {
        if (o == null) return;   
        Type t = o.GetType();
        if (!t.IsConstructedGenericType) return;

        foreach(Type genericTypeArgument in t.GetGenericArguments())
            Console.WriteLine(genericTypeArgument.Name);
    }

Yani yukarıdan gelen çağrı (`ShowGenericArguments(myList)`) şu çıktıyla sonuçlanır:

    Int32

## Ad alanı ile ada göre bir Tür alın
Bunu yapmak için, türü içeren derlemeye bir başvuruya ihtiyacınız var. İstediğinizle aynı Mecliste olduğunu bildiğiniz başka bir türünüz varsa, bunu yapabilirsiniz:

    typeof(KnownType).Assembly.GetType(typeName);

- "typeName" aradığınız türün adıdır (ad alanı dahil)
, ve "KnownType", aynı derlemede olduğunu bildiğiniz türdür.

Daha az verimli ancak daha genel olanı aşağıdaki gibidir:

    Type t = null;
    foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
    {
        if (ass.FullName.StartsWith("System."))
            continue;
        t = ass.GetType(typeName);
        if (t != null)
            break;
    }
Aramayı hızlandırmak için Sistem ad alanı derlemelerini taramayı dışlamak için yapılan kontrole dikkat edin. Türünüz aslında bir CLR türüyse, bu iki satırı silmeniz gerekecektir.

Derleme dahil olmak üzere tam derleme nitelikli tür adına sahipseniz, bunu kolayca alabilirsiniz.

    Type.GetType(fullyQualifiedName);

## Bir sınıfın tüm özellikleri arasında dolaşmak
    Type type = obj.GetType();
    //To restrict return properties. If all properties are required don't provide flag.
    BindingFlags flags = BindingFlags.Public | BindingFlags.Instance; 
    PropertyInfo[] properties = type.GetProperties(flags);
    
    foreach (PropertyInfo property in properties)
    {
        Console.WriteLine("Name: " + property.Name + ", Value: " + property.GetValue(obj, null));
    }
    
    


