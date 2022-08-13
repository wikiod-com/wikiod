---
title: "Métodos de extensión"
slug: "metodos-de-extension"
draft: false
images: []
weight: 6772
type: docs
toc: true
---

## Sintaxis
- público estático ReturnType MyExtensionMethod (este objetivo TargetType)
- público estático ReturnType MyExtensionMethod (este objetivo TargetType, TArg1 arg1, ...)

## Parámetros
| Parámetro | Detalles |
| --------- | ------- |  
| esto | El primer parámetro de un método de extensión siempre debe estar precedido por la palabra clave `this`, seguido del identificador con el que se hace referencia a la instancia "actual" del objeto que está extendiendo |


Los métodos de extensión son azúcar sintáctico que permite invocar métodos estáticos en instancias de objetos como si fueran miembros del tipo mismo.

Los métodos de extensión requieren un objeto de destino explícito. Deberá utilizar la palabra clave `this` para acceder al método desde el propio tipo extendido.

Los métodos de extensión deben declararse estáticos y deben vivir en una clase estática.

**¿Qué espacio de nombres?**

La elección del espacio de nombres para su clase de método de extensión es una compensación entre visibilidad y descubrimiento.

La [opción][1] más comúnmente mencionada es tener un espacio de nombres personalizado para sus métodos de extensión. Sin embargo, esto implicará un esfuerzo de comunicación para que los usuarios de su código sepan que existen los métodos de extensión y dónde encontrarlos.

Una alternativa es elegir un espacio de nombres para que los desarrolladores descubran sus métodos de extensión a través de Intellisense. Entonces, si desea extender la clase `Foo`, es lógico colocar los métodos de extensión en el mismo espacio de nombres que `Foo`.

Es importante darse cuenta de que **nada le impide usar el espacio de nombres de "alguien más"**: Por lo tanto, si desea extender `IEnumerable`, puede agregar su método de extensión en el espacio de nombres `System.Linq`.

Esto no es *siempre* una buena idea. Por ejemplo, en un caso específico, es posible que desee extender un tipo común ("bool IsApproxEqualTo (este valor doble, doble otro)" por ejemplo), pero que no "contamine" todo el "Sistema". En este caso, es preferible elegir un espacio de nombres específico y local.

Finalmente, ¡también es posible poner los métodos de extensión en *sin ningún espacio de nombres*!

Una buena pregunta de referencia: [¿Cómo administra los espacios de nombres de sus métodos de extensión?][2]

**Aplicabilidad**

Se debe tener cuidado al crear métodos de extensión para garantizar que sean apropiados para todas las entradas posibles y que no solo sean relevantes para situaciones específicas. Por ejemplo, es posible extender las clases del sistema como `cadena`, lo que hace que su nuevo código esté disponible para **cualquier** cadena. Si su código necesita realizar una lógica específica de dominio en un formato de cadena específico de dominio, un método de extensión no sería apropiado ya que su presencia confundiría a las personas que llaman que trabajan con otras cadenas en el sistema.

**La siguiente lista contiene características y propiedades básicas de los métodos de extensión**

1. Debe ser un método estático.
2. Debe estar ubicado en una clase estática.
3. Utiliza la palabra clave "this" como primer parámetro con un tipo en .NET y este método será llamado por una instancia de tipo dada en el lado del cliente.
4. También se muestra por VS intellisense. Cuando presionamos el punto `.` después de una instancia de tipo, aparece en VS intellisense.
5. Un método de extensión debe estar en el mismo espacio de nombres que se usa o debe importar el espacio de nombres de la clase mediante una declaración de uso.
6. Puede dar cualquier nombre a la clase que tiene un método de extensión, pero la clase debe ser estática.
7. Si desea agregar nuevos métodos a un tipo y no tiene el código fuente para ello, entonces la solución es usar e implementar métodos de extensión de ese tipo.
8. Si crea métodos de extensión que tienen los mismos métodos de firma que el tipo que está extendiendo, nunca se llamará a los métodos de extensión.


[1]: http://stackoverflow.com/q/1226189
[2]: http://stackoverflow.com/questions/2520446/how-do-you-manage-the-namespaces-of-your-extension-methods

## Métodos de extensión - descripción general
Los métodos de extensión se introdujeron en C# 3.0. Los métodos de extensión amplían y agregan comportamiento a los tipos existentes sin crear un nuevo tipo derivado, volver a compilar o modificar el tipo original. *Son especialmente útiles cuando no puede modificar la fuente de un tipo que desea mejorar.* Los métodos de extensión se pueden crear para tipos de sistemas, tipos definidos por terceros y tipos que usted mismo ha definido. El método de extensión se puede invocar como si fuera un método miembro del tipo original. Esto permite que **Encadenamiento de métodos** se use para implementar una **Interfaz fluida**.

Un método de extensión se crea agregando un **método estático** a una **clase estática** que es distinta del tipo original que se está extendiendo. La clase estática que contiene el método de extensión a menudo se crea con el único propósito de contener métodos de extensión.

Los métodos de extensión toman un primer parámetro especial que designa el tipo original que se está extendiendo. Este primer parámetro está decorado con la palabra clave `this` (que constituye un uso especial y distinto de `this` en C#; debe entenderse como diferente del uso de `this` que permite referirse a miembros de la instancia del objeto actual) .

En el siguiente ejemplo, el tipo original que se amplía es la clase `cadena`. `String` se ha ampliado mediante un método `Shorten()`, que proporciona la funcionalidad adicional de acortamiento. La clase estática `StringExtensions` se ha creado para contener el método de extensión. El método de extensión `Shorten()` muestra que es una extensión de `string` a través del primer parámetro especialmente marcado. Para mostrar que el método `Shorten()` extiende `string`, el primer parámetro está marcado con `this`. Por lo tanto, la firma completa del primer parámetro es `este texto de cadena`, donde `cadena` es el tipo original que se está ampliando y `texto` es el nombre del parámetro elegido.

    static class StringExtensions
    {
        public static string Shorten(this string text, int length) 
        {
            return text.Substring(0, length);
        }
    }

    class Program
    {
        static void Main()
        {
            // This calls method String.ToUpper()
            var myString = "Hello World!".ToUpper();

            // This calls the extension method StringExtensions.Shorten()
            var newString = myString.Shorten(5); 

            // It is worth noting that the above call is purely syntactic sugar
            // and the assignment below is functionally equivalent
            var newString2 = StringExtensions.Shorten(myString, 5);
        }
    }

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/uiPhpP)

-------------------------------------------------- ---------------------------------

El objeto pasado como el *primer argumento de un método de extensión* (que va acompañado de la palabra clave `this`) es la instancia a la que se llama el método de extensión.

Por ejemplo, cuando se ejecuta este código:

    "some string".Shorten(5);

Los valores de los argumentos son los siguientes:

    text: "some string"
    length: 5

*Tenga en cuenta que los métodos de extensión solo se pueden usar si están en el mismo espacio de nombres que su definición, si el código importa explícitamente el espacio de nombres usando el método de extensión, o si la clase de extensión no tiene espacio de nombres.* Las pautas de .NET Framework recomiendan poner clases de extensión en su propio espacio de nombres. Sin embargo, esto puede conducir a problemas de descubrimiento.

Esto da como resultado que no haya conflictos entre los métodos de extensión y las bibliotecas que se utilizan, a menos que se extraigan explícitamente los espacios de nombres que podrían entrar en conflicto. Por ejemplo, [Extensiones LINQ][1]:
    
    using System.Linq; // Allows use of extension methods from the System.Linq namespace

    class Program
    {
        static void Main()
        {
            var ints = new int[] {1, 2, 3, 4};

            // Call Where() extension method from the System.Linq namespace
            var even = ints.Where(x => x % 2 == 0); 
        }
    }

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/IF223c)

-------------------------------------------------- ---------------------------------

Desde C# 6.0, también es posible colocar una directiva `using static` en la _clase_ que contiene los métodos de extensión. Por ejemplo, `usando static System.Linq.Enumerable;`. Esto hace que los métodos de extensión de esa clase en particular estén disponibles sin incluir otros tipos del mismo espacio de nombres en el ámbito.

-------------------------------------------------- ---------------------------------

Cuando hay disponible un método de clase con la misma firma, el compilador lo prioriza sobre la llamada al método de extensión. Por ejemplo:

    class Test
    {
       public void Hello()
       {
           Console.WriteLine("From Test");
       }
    }

    static class TestExtensions
    {
        public static void Hello(this Test test)
        {
            Console.WriteLine("From extension method");
        }
    }

    class Program
    {
        static void Main()
        {
            Test t = new Test();
            t.Hello(); // Prints "From Test"
        }
    }

<!-- versión final si -->

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/fI3sCJ)

-------------------------------------------------- ---------------------------------

Tenga en cuenta que si hay dos funciones de extensión con la misma firma, y ​​una de ellas está en el mismo espacio de nombres, entonces esa tendrá prioridad. Por otro lado, si se accede a ambos mediante `using`, se producirá un error de tiempo de compilación con el mensaje:
>**La llamada es ambigua entre los siguientes métodos o propiedades**


-------------------------------------------------- ---------------------------------


Tenga en cuenta que la conveniencia sintáctica de llamar a un método de extensión a través de `originalTypeInstance.ExtensionMethod()` es una conveniencia opcional. El método también se puede llamar de la manera tradicional, de modo que el primer parámetro especial se utilice como parámetro del método.

Es decir, ambos de los siguientes trabajos:

    //Calling as though method belongs to string--it seamlessly extends string
    String s = "Hello World";
    s.Shorten(5);  
    
    //Calling as a traditional static method with two parameters
    StringExtensions.Shorten(s, 5);

[1]: https://www.wikiod.com/es/docs/c%23/68/linq-queries

## Comprobación nula
Los métodos de extensión son métodos estáticos que se comportan como métodos de instancia. Sin embargo, a diferencia de lo que sucede cuando se llama a un método de instancia en una referencia `null`, cuando se llama a un método de extensión con una referencia `null`, no arroja una [`NullReferenceException`][1]. Esto puede ser muy útil en algunos escenarios.

Por ejemplo, considere la siguiente clase estática:

    public static class StringExtensions
    {
        public static string EmptyIfNull(this string text)
        {
            return text ?? String.Empty;
        }

        public static string NullIfEmpty(this string text)
        {
            return String.Empty == text ? null : text;
        }
    }

<!-- separar -->

    string nullString = null;
    string emptyString = nullString.EmptyIfNull();// will return ""
    string anotherNullString = emptyString.NullIfEmpty(); // will return null

[Demostración en vivo en .NET Fiddle][2]

[1]: https://msdn.microsoft.com/en-us/library/system.nullreferenceexception(v=vs.110).aspx
[2]: https://dotnetfiddle.net/jNQWqg

## Uso explícito de un método de extensión
Los métodos de extensión también se pueden usar como métodos de clase estáticos ordinarios. Esta forma de llamar a un método de extensión es más detallada, pero es necesaria en algunos casos.

    static class StringExtensions
    {
        public static string Shorten(this string text, int length) 
        {
            return text.Substring(0, length);
        }
    }

Uso:

    var newString = StringExtensions.Shorten("Hello World", 5);

# Cuándo llamar a los métodos de extensión como métodos estáticos

Todavía hay escenarios en los que necesitaría usar un método de extensión como método estático:

* Resolución de conflictos con un método miembro. Esto puede suceder si una nueva versión de una biblioteca introduce un nuevo método miembro con la misma firma. En este caso, el compilador preferirá el método miembro.
* Resolución de conflictos con otro método de extensión con la misma firma. Esto puede suceder si dos bibliotecas incluyen métodos de extensión similares y se usan espacios de nombres de ambas clases con métodos de extensión en el mismo archivo.
* Pasar el método de extensión como un grupo de métodos al parámetro de delegado.
* Haciendo tu propia encuadernación a través de `Reflection`.
* Usando el método de extensión en la ventana Inmediato en Visual Studio.

# Usando estática

Si se usa una directiva `using static` para traer miembros estáticos de una clase estática al alcance global, los métodos de extensión se omiten. Ejemplo:

    using static OurNamespace.StringExtensions; // refers to class in previous example

    // OK: extension method syntax still works.
    "Hello World".Shorten(5);
    // OK: static method syntax still works.
    OurNamespace.StringExtensions.Shorten("Hello World", 5);
    // Compile time error: extension methods can't be called as static without specifying class.
    Shorten("Hello World", 5);

Si elimina el modificador `this` del primer argumento del método `Shorten`, se compilará la última línea.


## Los métodos de extensión solo pueden ver miembros públicos (o internos) de la clase extendida
    public class SomeClass
    {
        public void DoStuff()
        {
            
        }

        protected void DoMagic()
        {
            
        }
    }

    public static class SomeClassExtensions
    {
        public static void DoStuffWrapper(this SomeClass someInstance)
        {
            someInstance.DoStuff(); // ok
        }

        public static void DoMagicWrapper(this SomeClass someInstance)
        {
            someInstance.DoMagic(); // compilation error
        }
    }

Los métodos de extensión son solo un azúcar sintáctico, y en realidad no son miembros de la clase que extienden. Esto significa que no pueden romper la encapsulación; solo tienen acceso a campos, propiedades y métodos "públicos" (o cuando se implementan en el mismo ensamblado, "internos").

## Métodos de extensión genéricos
Al igual que otros métodos, los métodos de extensión pueden usar genéricos. Por ejemplo:

    static class Extensions
    {
        public static bool HasMoreThanThreeElements<T>(this IEnumerable<T> enumerable)
        {
            return enumerable.Take(4).Count() > 3;
        }
    }
y llamarlo sería como:

    IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
    var hasMoreThanThreeElements = numbers.HasMoreThanThreeElements();

[Ver demostración][1]

Del mismo modo para múltiples argumentos de tipo:

    public static TU GenericExt<T, TU>(this T obj)
    {
         TU ret = default(TU);
         // do some stuff with obj
         return ret;
    }

Llamarlo sería como:

    IEnumerable<int> numbers = new List<int> {1,2,3,4,5,6};
    var result = numbers.GenericExt<IEnumerable<int>,String>();

[Ver demostración][2]

También puede crear métodos de extensión para tipos parcialmente enlazados en tipos multigenéricos:

    class MyType<T1, T2>
    {
    }
    
    static class Extensions
    {
        public static void Example<T>(this MyType<int, T> test)
        {        
        }
    }

Llamarlo sería como:

    MyType<int, string> t = new MyType<int, string>();
    t.Example();

[Ver demostración][4]

También puede especificar restricciones de tipo con [`where`][3] :

    public static bool IsDefault<T>(this T obj) where T : struct, IEquatable<T>
    {
         return EqualityComparer<T>.Default.Equals(obj, default(T));
    }

Código de llamada:

    int number = 5;
    var IsDefault = number.IsDefault();

[Ver demostración][5]


[1]: https://dotnetfiddle.net/UlCa3i
[2]: https://dotnetfiddle.net/aMNO0X
[3]: https://www.wikiod.com/es/docs/c%23/26/keywords/8137/where-type-constraints#t=201607221442171394675
[4]: https://dotnetfiddle.net/1FjUOH
[5]: https://dotnetfiddle.net/Jom3cS

## Métodos de extensión para encadenar
Cuando un método de extensión devuelve un valor que tiene el mismo tipo que su argumento `this`, se puede usar para "encadenar" una o más llamadas de método con una firma compatible. Esto puede ser útil para tipos primitivos y/o sellados, y permite la creación de las denominadas API "fluidas" si los nombres de los métodos se leen como lenguaje humano natural.

    void Main()
    {
        int result = 5.Increment().Decrement().Increment(); 
        // result is now 6
    }
    
    public static class IntExtensions 
    {
        public static int Increment(this int number) {
            return ++number;
        }

        public static int Decrement(this int number) {
            return --number;
        }
    }

O así

    void Main()
    {
        int[] ints = new[] { 1, 2, 3, 4, 5, 6};
        int[] a = ints.WhereEven();
        //a is { 2, 4, 6 };
        int[] b = ints.WhereEven().WhereGreaterThan(2);
        //b is { 4, 6 };
    }
    
    public static class IntArrayExtensions
    {
        public static int[] WhereEven(this int[] array)
        {
            //Enumerable.* extension methods use a fluent approach
            return array.Where(i => (i%2) == 0).ToArray();
        }
    
        public static int[] WhereGreaterThan(this int[] array, int value)
        {
            return array.Where(i => i > value).ToArray();
        }
    }

## Métodos de extensión con enumeración
Los métodos de extensión son útiles para agregar funcionalidad a las enumeraciones.

Un uso común es implementar un método de conversión.

    public enum YesNo
    {
        Yes,
        No,
    }
    
    public static class EnumExtentions
    {
        public static bool ToBool(this YesNo yn)
        {
            return yn == YesNo.Yes;
        }
        public static YesNo ToYesNo(this bool yn)
        {
            return yn ? YesNo.Yes : YesNo.No;
        }
    }

Ahora puede convertir rápidamente su valor de enumeración a un tipo diferente. En este caso un bool.

    bool yesNoBool = YesNo.Yes.ToBool(); // yesNoBool == true
    YesNo yesNoEnum = false.ToYesNo();   // yesNoEnum == YesNo.No


Alternativamente, los métodos de extensión se pueden usar para agregar propiedades como métodos.

    public enum Element
    {
        Hydrogen,
        Helium,
        Lithium,
        Beryllium,
        Boron,
        Carbon,
        Nitrogen,
        Oxygen
        //Etc
    }

    public static class ElementExtensions
    {
        public static double AtomicMass(this Element element)
        {
            switch(element)
            {
                case Element.Hydrogen:  return 1.00794;
                case Element.Helium:    return 4.002602;
                case Element.Lithium:   return 6.941;
                case Element.Beryllium: return 9.012182;
                case Element.Boron:     return 10.811;
                case Element.Carbon:    return 12.0107;
                case Element.Nitrogen:  return 14.0067;
                case Element.Oxygen:    return 15.9994;
                //Etc
            }
            return double.Nan;
        }
    }

    var massWater = 2*Element.Hydrogen.AtomicMass() + Element.Oxygen.AtomicMass();

## Los métodos de extensión se envían según el tipo estático
El tipo estático (tiempo de compilación) se usa en lugar del dinámico (tipo de tiempo de ejecución) para hacer coincidir los parámetros.

    public class Base 
    { 
        public virtual string GetName()
        {
            return "Base";
        }
    }

    public class Derived : Base
    { 
        public override string GetName()
        {
            return "Derived";
        }
    }

    public static class Extensions
    {
        public static string GetNameByExtension(this Base item)
        {
            return "Base";
        }

        public static string GetNameByExtension(this Derived item)
        {
            return "Derived";
        }
    }

    public static class Program   
    {
        public static void Main()
        {
            Derived derived = new Derived();
            Base @base = derived;

            // Use the instance method "GetName"
            Console.WriteLine(derived.GetName()); // Prints "Derived"
            Console.WriteLine(@base.GetName()); // Prints "Derived"

            // Use the static extension method "GetNameByExtension"
            Console.WriteLine(derived.GetNameByExtension()); // Prints "Derived"
            Console.WriteLine(@base.GetNameByExtension()); // Prints "Base"
        }
    }
[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/7BGp8o)

Además, el envío basado en el tipo estático no permite llamar a un método de extensión en un objeto `dinámico`:

    public class Person
    {
        public string Name { get; set; }
    }
    
    public static class ExtenionPerson
    {
        public static string GetPersonName(this Person person)
        {
            return person.Name;
        }
    }
    
    dynamic person = new Person { Name = "Jon" };
    var name = person.GetPersonName(); // RuntimeBinderException is thrown

## Métodos de extensión en interfaces
Una característica útil de los métodos de extensión es que puede crear métodos comunes para una interfaz. Normalmente, una interfaz no puede tener implementaciones compartidas, pero con métodos de extensión sí pueden.

    public interface IVehicle
    {
        int MilesDriven { get; set; }
    }
    
    public static class Extensions
    {
        public static int FeetDriven(this IVehicle vehicle)
        {
            return vehicle.MilesDriven * 5028;
        }
    }

En este ejemplo, el método `FeetDriven` se puede utilizar en cualquier `IVehicle`. Esta lógica en este método se aplicaría a todos los `IVehicle`s, por lo que se puede hacer de esta manera para que no tenga que haber un `FeetDriven` en la definición de `IVehicle` que se implementaría de la misma manera para todos los niños .

## Los métodos de extensión no son compatibles con el código dinámico.

    static class Program
    {
        static void Main()
        {
            dynamic dynamicObject = new ExpandoObject();
    
            string awesomeString = "Awesome";
    
            // Prints True
            Console.WriteLine(awesomeString.IsThisAwesome());
    
            dynamicObject.StringValue = awesomeString;
    
            // Prints True
            Console.WriteLine(StringExtensions.IsThisAwesome(dynamicObject.StringValue)); 
            
            // No compile time error or warning, but on runtime throws RuntimeBinderException
            Console.WriteLine(dynamicObject.StringValue.IsThisAwesome());
        }
    }
    
    static class StringExtensions
    {
        public static bool IsThisAwesome(this string value)
        {
            return value.Equals("Awesome");
        }
    }

> La razón por la que [llamar a los métodos de extensión desde el código dinámico] no funciona es porque en los métodos de extensión de código normales y no dinámicos funcionan haciendo una búsqueda completa de todas las clases conocidas por el compilador para una clase estática que tiene un método de extensión que partidos. La búsqueda se realiza en orden según el anidamiento del espacio de nombres y las directivas `using` disponibles en cada espacio de nombres.
> 
> Eso significa que para obtener una invocación de método de extensión dinámica resuelta correctamente, de alguna manera el DLR tiene que saber *en tiempo de ejecución* cuáles eran todos los anidamientos de espacios de nombres y `usando` directivas *en su código fuente*. No tenemos un mecanismo práctico para codificar toda esa información en el sitio de la llamada. Consideramos inventar un mecanismo de este tipo, pero decidimos que era demasiado costoso y producía demasiado riesgo de cronograma para que valiera la pena.

[Fuente](http://stackoverflow.com/a/5313149/1610754)

## Métodos de extensión en combinación con interfaces
Es muy conveniente usar métodos de extensión con interfaces, ya que la implementación se puede almacenar fuera de la clase y todo lo que se necesita para agregar alguna funcionalidad a la clase es decorar la clase con la interfaz.

    public interface IInterface
    {
       string Do()
    }

    public static class ExtensionMethods{
        public static string DoWith(this IInterface obj){
          //does something with IInterface instance
        }
    }

    public class Classy : IInterface
    {
       // this is a wrapper method; you could also call DoWith() on a Classy instance directly,
       // provided you import the namespace containing the extension method
       public Do(){
           return this.DoWith();
       }
    }


usar como:

     var classy = new Classy();
     classy.Do(); // will call the extension
     classy.DoWith(); // Classy implements IInterface so it can also be called this way

## Las extensiones y las interfaces juntas permiten el código DRY y la funcionalidad similar a mixin
Los métodos de extensión le permiten simplificar las definiciones de su interfaz al incluir solo la funcionalidad principal requerida en la interfaz misma y permitirle definir métodos de conveniencia y sobrecargas como métodos de extensión. Las interfaces con menos métodos son más fáciles de implementar en nuevas clases. Mantener las sobrecargas como extensiones en lugar de incluirlas directamente en la interfaz le evita copiar el código repetitivo en cada implementación, lo que lo ayuda a mantener su código SECO. De hecho, esto es similar al patrón mixin que C# no admite.

Las extensiones de `System.Linq.Enumerable` a `IEnumerable<T>` son un gran ejemplo de esto. `IEnumerable<T>` solo requiere que la clase de implementación implemente dos métodos: `GetEnumerator()` genérico y no genérico. Pero `System.Linq.Enumerable` proporciona innumerables utilidades útiles como extensiones que permiten un consumo claro y conciso de `IEnumerable<T>`.

La siguiente es una interfaz muy simple con sobrecargas convenientes proporcionadas como extensiones.

    public interface ITimeFormatter
    {
       string Format(TimeSpan span);
    }

    public static class TimeFormatter
    {
        // Provide an overload to *all* implementers of ITimeFormatter.
        public static string Format(
            this ITimeFormatter formatter,
            int millisecondsSpan)
            => formatter.Format(TimeSpan.FromMilliseconds(millisecondsSpan));
    }

    // Implementations only need to provide one method. Very easy to
    // write additional implementations.
    public class SecondsTimeFormatter : ITimeFormatter
    {
       public string Format(TimeSpan span)
       {
           return $"{(int)span.TotalSeconds}s";
       }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var formatter = new SecondsTimeFormatter();
            // Callers get two method overloads!
            Console.WriteLine($"4500ms is rougly {formatter.Format(4500)}");
            var span = TimeSpan.FromSeconds(5);
            Console.WriteLine($"{span} is formatted as {formatter.Format(span)}");
        }
    }


## Ejemplo del método de extensión IList<T>: comparación de 2 listas
Puede usar el siguiente método de extensión para comparar el contenido de dos instancias de IList< T > del mismo tipo.

De forma predeterminada, los elementos se comparan en función de su orden dentro de la lista y los elementos en sí, al pasar false al parámetro `isOrdered` se compararán solo los elementos en sí, independientemente de su orden.

Para que este método funcione, el tipo genérico (`T`) debe anular los métodos `Equals` y `GetHashCode`.

**Uso:**

    List<string> list1 = new List<string> {"a1", "a2", null, "a3"};
    List<string> list2 = new List<string> {"a1", "a2", "a3", null};

    list1.Compare(list2);//this gives false
    list1.Compare(list2, false);//this gives true. they are equal when the order is disregarded

**Método:**

    public static bool Compare<T>(this IList<T> list1, IList<T> list2, bool isOrdered = true) 
    {
        if (list1 == null && list2 == null)
            return true;
        if (list1 == null || list2 == null || list1.Count != list2.Count)
            return false;

        if (isOrdered)
        {
            for (int i = 0; i < list2.Count; i++)
            {
                var l1 = list1[i]; 
                var l2 = list2[i];
                if (
                     (l1 == null && l2 != null) || 
                     (l1 != null && l2 == null) || 
                     (!l1.Equals(l2)))
                {
                        return false;
                }
            }
            return true;
        }
        else
        {
            List<T> list2Copy = new List<T>(list2);
            //Can be done with Dictionary without O(n^2)
            for (int i = 0; i < list1.Count; i++)
            {
                if (!list2Copy.Remove(list1[i]))
                    return false;
            }
            return true;
        }
    }

## Métodos de extensión como contenedores fuertemente tipados
Los métodos de extensión se pueden usar para escribir envoltorios fuertemente tipados para objetos similares a diccionarios. Por ejemplo, un caché, `HttpContext.Items` en cetera...

    public static class CacheExtensions
    {
        public static void SetUserInfo(this Cache cache, UserInfo data) => 
            cache["UserInfo"] = data;

        public static UserInfo GetUserInfo(this Cache cache) => 
            cache["UserInfo"] as UserInfo;
    }

Este enfoque elimina la necesidad de usar literales de cadena como claves en todo el código base, así como la necesidad de convertir al tipo requerido durante la operación de lectura. En general, crea una forma más segura y fuertemente tipada de interactuar con objetos tan poco tipificados como los diccionarios.

## Uso de métodos de extensión para crear hermosas clases de mapeador
Podemos crear mejores clases de mapeador con métodos de extensión,
Supongamos que si tengo algunas clases de DTO como

     public class UserDTO
     {
            public AddressDTO Address { get; set; }
     }
    
     public class AddressDTO
     {
            public string Name { get; set; }
     }

y necesito mapear a las clases de modelo de vista correspondientes

    public class UserViewModel
    {
        public AddressViewModel Address { get; set; }
    }
    
    public class AddressViewModel
    {
        public string Name { get; set; }
    }

entonces puedo crear mi clase de mapeador como a continuación

    public static class ViewModelMapper
    {
          public static UserViewModel ToViewModel(this UserDTO user)
          {
                return user == null ?
                    null :
                    new UserViewModel()
                    {
                        Address = user.Address.ToViewModel()
                        // Job = user.Job.ToViewModel(),
                        // Contact = user.Contact.ToViewModel() .. and so on
                    };
          }
    
          public static AddressViewModel ToViewModel(this AddressDTO userAddr)
          {
                return userAddr == null ?
                    null :
                    new AddressViewModel()
                    {
                        Name = userAddr.Name
                    };
          }
    }

Entonces, finalmente, puedo invocar a mi mapeador como se muestra a continuación.

        UserDTO userDTOObj = new UserDTO() {
                Address = new AddressDTO() {
                    Name = "Address of the user"
                }
            };

        UserViewModel user = userDTOObj.ToViewModel(); // My DTO mapped to Viewmodel


La belleza aquí es que todo el método de mapeo tiene un nombre común (ToViewModel) y podemos reutilizarlo de varias maneras.

## Uso de métodos de extensión para crear nuevos tipos de colección (por ejemplo, DictList)
Puede crear métodos de extensión para mejorar la usabilidad de las colecciones anidadas, como un `Diccionario` con un valor `List<T>`.

Considere los siguientes métodos de extensión:

    public static class DictListExtensions
    {
        public static void Add<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
                where TCollection : ICollection<TValue>, new()
        {
            TCollection list;
            if (!dict.TryGetValue(key, out list))
            {
                list = new TCollection();
                dict.Add(key, list);
            }

            list.Add(value);
        }

        public static bool Remove<TKey, TValue, TCollection>(this Dictionary<TKey, TCollection> dict, TKey key, TValue value)
            where TCollection : ICollection<TValue>
        {
            TCollection list;
            if (!dict.TryGetValue(key, out list))
            {
                return false;
            }

            var ret = list.Remove(value);
            if (list.Count == 0)
            {
                dict.Remove(key);
            }
            return ret;
        }
    }

puede utilizar los métodos de extensión de la siguiente manera:

    var dictList = new Dictionary<string, List<int>>();

    dictList.Add("example", 5);
    dictList.Add("example", 10);
    dictList.Add("example", 15);
    
    Console.WriteLine(String.Join(", ", dictList["example"])); // 5, 10, 15

    dictList.Remove("example", 5);
    dictList.Remove("example", 10);
    
    Console.WriteLine(String.Join(", ", dictList["example"])); // 15
    
    dictList.Remove("example", 15);
    
    Console.WriteLine(dictList.ContainsKey("example")); // False

[Ver demostración](https://dotnetfiddle.net/UbdQuC)

## Métodos de extensión para manejar casos especiales

Los métodos de extensión se pueden usar para "ocultar" el procesamiento de reglas comerciales poco elegantes que, de otro modo, requerirían saturar una función de llamada con declaraciones si/entonces. Esto es similar y análogo al manejo de nulos con métodos de extensión. Por ejemplo,

    public static class CakeExtensions
    {
        public static Cake EnsureTrueCake(this Cake cake)
        {
            //If the cake is a lie, substitute a cake from grandma, whose cakes aren't as tasty but are known never to be lies. If the cake isn't a lie, don't do anything and return it.
            return CakeVerificationService.IsCakeLie(cake) ? GrandmasKitchen.Get1950sCake() : cake;
        }
    }

<!-- separar -->

    Cake myCake = Bakery.GetNextCake().EnsureTrueCake();
    myMouth.Eat(myCake);//Eat the cake, confident that it is not a lie.


## Uso de métodos de extensión con métodos estáticos y devoluciones de llamada
Considere usar métodos de extensión como funciones que envuelven otro código, aquí hay un gran ejemplo que usa un método estático y un método de extensión para envolver la construcción Try Catch. Haz tu código a prueba de balas...

    using System;
    using System.Diagnostics;
    
    namespace Samples
    {
        /// <summary>
        /// Wraps a try catch statement as a static helper which uses 
        /// Extension methods for the exception
        /// </summary>
        public static class Bullet
        {
            /// <summary>
            /// Wrapper for Try Catch Statement
            /// </summary>
            /// <param name="code">Call back for code</param>
            /// <param name="error">Already handled and logged exception</param>
            public static void Proof(Action code, Action<Exception> error)
            {
                try
                {
                    code();
                }
                catch (Exception iox)
                {
                    //extension method used here
                    iox.Log("BP2200-ERR-Unexpected Error");
                    //callback, exception already handled and logged
                    error(iox);
                }
            }
            /// <summary>
            /// Example of a logging method helper, this is the extension method
            /// </summary>
            /// <param name="error">The Exception to log</param>
            /// <param name="messageID">A unique error ID header</param>
            public static void Log(this Exception error, string messageID)
            {
                Trace.WriteLine(messageID);
                Trace.WriteLine(error.Message);
                Trace.WriteLine(error.StackTrace);
                Trace.WriteLine("");
            }
        }
        /// <summary>
        /// Shows how to use both the wrapper and extension methods.
        /// </summary>
        public class UseBulletProofing
        {
            public UseBulletProofing()
            {
                var ok = false;
                var result = DoSomething();
                if (!result.Contains("ERR"))
                {
                    ok = true;
                    DoSomethingElse();
                }
            }
    
            /// <summary>
            /// How to use Bullet Proofing in your code.
            /// </summary>
            /// <returns>A string</returns>
            public string DoSomething()
            {
                string result = string.Empty;
                //Note that the Bullet.Proof method forces this construct.
                Bullet.Proof(() =>
                {
                    //this is the code callback
                    result = "DST5900-INF-No Exceptions in this code";
                }, error =>
                {
                    //error is the already logged and handled exception
                    //determine the base result
                    result = "DTS6200-ERR-An exception happened look at console log";
                    if (error.Message.Contains("SomeMarker"))
                    {
                        //filter the result for Something within the exception message
                        result = "DST6500-ERR-Some marker was found in the exception";
                    }
                });
                return result;
            }
    
            /// <summary>
            /// Next step in workflow
            /// </summary>
            public void DoSomethingElse()
            {
                //Only called if no exception was thrown before
            }
        }
    }

