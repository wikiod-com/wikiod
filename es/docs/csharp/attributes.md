---
title: "Atributos"
slug: "atributos"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Creando un atributo personalizado
    //1) All attributes should be inherited from System.Attribute
    //2) You can customize your attribute usage (e.g. place restrictions) by using System.AttributeUsage Attribute
    //3) You can use this attribute only via reflection in the way it is supposed to be used
    //4) MethodMetadataAttribute is just a name. You can use it without "Attribute" postfix - e.g. [MethodMetadata("This text could be retrieved via reflection")].
    //5) You can overload an attribute constructors
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Class)]
    public class MethodMetadataAttribute : System.Attribute
    {
        //this is custom field given just for an example
        //you can create attribute without any fields
        //even an empty attribute can be used - as marker
        public string Text { get; set; }
    
        //this constructor could be used as [MethodMetadata]
        public MethodMetadataAttribute ()
        {
        }
    
        //This constructor could be used as [MethodMetadata("String")]
        public MethodMetadataAttribute (string text)
        {
            Text = text;
        }
    }

## Leer un atributo
El método `GetCustomAttributes` devuelve una matriz de atributos personalizados aplicados al miembro. Después de recuperar esta matriz, puede buscar uno o más atributos específicos.

    var attribute = typeof(MyClass).GetCustomAttributes().OfType<MyCustomAttribute>().Single();

O iterar a través de ellos

    foreach(var attribute in typeof(MyClass).GetCustomAttributes()) {
        Console.WriteLine(attribute.GetType());
    }

El método de extensión `GetCustomAttribute` de `System.Reflection.CustomAttributeExtensions` recupera un atributo personalizado de un tipo específico, se puede aplicar a cualquier `MemberInfo`.

    var attribute = (MyCustomAttribute) typeof(MyClass).GetCustomAttribute(typeof(MyCustomAttribute));

`GetCustomAttribute` también tiene una firma genérica para especificar el tipo de atributo a buscar.

    var attribute = typeof(MyClass).GetCustomAttribute<MyCustomAttribute>();

El argumento booleano `heredar` se puede pasar a ambos métodos. Si este valor se establece en "verdadero", los ancestros del elemento también se inspeccionarán.

## Usar un atributo
    [StackDemo(Text = "Hello, World!")]
    public class MyClass
    {
        [StackDemo("Hello, World!")]
        static void MyMethod()
        {
        }
    }

## Atributo de visualización del depurador
Agregar el atributo `DebuggerDisplay` cambiará la forma en que el depurador muestra la clase cuando se desplaza sobre ella.

El depurador evaluará las expresiones que están envueltas en `{}`. Puede ser una propiedad simple como en el siguiente ejemplo o una lógica más compleja.

    
    [DebuggerDisplay("{StringProperty} - {IntProperty}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    }
    

[![Ejemplo de visualización del depurador][1]][1]

Agregar `,nq` antes del corchete de cierre elimina las comillas al generar una cadena.

    [DebuggerDisplay("{StringProperty,nq} - {IntProperty}")]
Aunque las expresiones generales están permitidas en `{}`, no se recomiendan. El atributo `DebuggerDisplay` se escribirá en los metadatos del ensamblado como una cadena. No se comprueba la validez de las expresiones en `{}`. Por lo tanto, un atributo `DebuggerDisplay` que contenga una lógica más compleja que, por ejemplo, alguna aritmética simple podría funcionar bien en C#, pero la misma expresión evaluada en VB.NET probablemente no sea sintácticamente válida y produzca un error durante la depuración.

Una forma de hacer que `DebuggerDisplay` sea más independiente del lenguaje es escribir la expresión en un método o propiedad y llamarla en su lugar.

    [DebuggerDisplay("{DebuggerDisplay(),nq}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    
       private string DebuggerDisplay()
        {
            return $"{StringProperty} - {IntProperty}"";
        }
    }

Uno podría querer que `DebuggerDisplay` muestre todas o solo algunas de las propiedades y, al depurar e inspeccionar, también el tipo del objeto.
El siguiente ejemplo también rodea el método auxiliar con `#if DEBUG` ya que `DebuggerDisplay` se usa en entornos de depuración.

    [DebuggerDisplay("{DebuggerDisplay(),nq}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    
    #if DEBUG
       private string DebuggerDisplay()
        {
            return
                $"ObjectId:{this.ObjectId}, StringProperty:{this.StringProperty}, Type:{this.GetType()}";
        }
        #endif
    }

[1]: http://i.stack.imgur.com/6JjJs.png

## Atributos de información de la persona que llama
Los atributos de información de la persona que llama se pueden usar para transmitir información sobre el invocador al método invocado. La declaración se ve así:
    
    using System.Runtime.CompilerServices;

    public void LogException(Exception ex,
                             [CallerMemberName]string callerMemberName = "",
                             [CallerLineNumber]int callerLineNumber = 0,
                             [CallerFilePath]string callerFilePath = "")
    {
        //perform logging
    }

Y la invocación queda así:

    public void Save(DBContext context)
    {
        try
        {
            context.SaveChanges();
        }
        catch (Exception ex)
        {
            LogException(ex);
        }
    }
    

Tenga en cuenta que solo el primer parámetro se pasa explícitamente al método `LogException`, mientras que el resto de ellos se proporcionarán en tiempo de compilación con los valores relevantes.

El parámetro `callerMemberName` recibirá el valor `"Save"` - el nombre del método de llamada.

El parámetro `callerLineNumber` recibirá el número de cualquier línea en la que esté escrita la llamada al método `LogException`.

Y el parámetro 'callerFilePath' recibirá la ruta completa del archivo en el que se declara el método `Save`.


## Leer un atributo desde la interfaz
No existe una forma sencilla de obtener atributos de una interfaz, ya que las clases no heredan atributos de una interfaz. Siempre que implemente una interfaz o anule miembros en una clase derivada, debe volver a declarar los atributos.
Entonces, en el ejemplo a continuación, la salida sería `Verdadero` en los tres casos.

    using System;
    using System.Linq;
    using System.Reflection;

    namespace InterfaceAttributesDemo {
        
        [AttributeUsage(AttributeTargets.Interface, Inherited = true)]
        class MyCustomAttribute : Attribute {
            public string Text { get; set; }
        }
        
        [MyCustomAttribute(Text = "Hello from interface attribute")]
        interface IMyClass {
            void MyMethod();
        }
        
        class MyClass : IMyClass {
            public void MyMethod() { }
        }
        
        public class Program {
            public static void Main(string[] args) {
                GetInterfaceAttributeDemo();
            }
            
            private static void GetInterfaceAttributeDemo() {
                var attribute1 = (MyCustomAttribute) typeof(MyClass).GetCustomAttribute(typeof(MyCustomAttribute), true);
                Console.WriteLine(attribute1 == null); // True
                
                var attribute2 = typeof(MyClass).GetCustomAttributes(true).OfType<MyCustomAttribute>().SingleOrDefault();
                Console.WriteLine(attribute2 == null); // True
                
                var attribute3 = typeof(MyClass).GetCustomAttribute<MyCustomAttribute>(true);
                Console.WriteLine(attribute3 == null); // True
            }
        }
    }

Una forma de recuperar atributos de interfaz es buscarlos a través de todas las interfaces implementadas por una clase.

    var attribute = typeof(MyClass).GetInterfaces().SelectMany(x => x.GetCustomAttributes().OfType<MyCustomAttribute>()).SingleOrDefault();
    Console.WriteLine(attribute == null); // False
    Console.WriteLine(attribute.Text); // Hello from interface attribute


## Atributo obsoleto
System.Obsolete es un atributo que se usa para marcar un tipo o un miembro que tiene una versión mejor y, por lo tanto, no debe usarse.

    [Obsolete("This class is obsolete. Use SomeOtherClass instead.")]
    class SomeClass
    {
        //
    }

En caso de que se use la clase anterior, el compilador mostrará la advertencia "Esta clase está obsoleta. Use SomeOtherClass en su lugar".



