---
title: "Serialización binaria"
slug: "serializacion-binaria"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

El motor de serialización binaria es parte del marco .NET, pero los ejemplos que se dan aquí son específicos de C#. En comparación con otros motores de serialización integrados en .NET Framework, el serializador binario es rápido y eficiente y, por lo general, requiere muy poco código adicional para que funcione. Sin embargo, también es menos tolerante a los cambios de código; es decir, si serializa un objeto y luego realiza un ligero cambio en la definición del objeto, es probable que no se deserialice correctamente.

## Carpeta de serialización
La carpeta le brinda la oportunidad de inspeccionar qué tipos se están cargando en el dominio de su aplicación

Crear una clase heredada de SerializationBinder

    class MyBinder : SerializationBinder
    {
        public override Type BindToType(string assemblyName, string typeName)
        {
            if (typeName.Equals("BinarySerializationExample.Item"))
                return typeof(Item);
            return null;
        }
    }


Ahora podemos comprobar qué tipos se están cargando y en base a ello decidir qué es lo que realmente queremos recibir.

Para usar un archivador, debe agregarlo al BinaryFormatter.

    object DeserializeData(byte[] bytes)
    {
        var binaryFormatter = new BinaryFormatter();
        binaryFormatter.Binder = new MyBinder();

        using (var memoryStream = new MemoryStream(bytes))
            return binaryFormatter.Deserialize(memoryStream);
    }


La solución completa

    using System;
    using System.IO;
    using System.Runtime.Serialization;
    using System.Runtime.Serialization.Formatters.Binary;
    
    namespace BinarySerializationExample
    {
        class MyBinder : SerializationBinder
        {
            public override Type BindToType(string assemblyName, string typeName)
            {
                if (typeName.Equals("BinarySerializationExample.Item"))
                    return typeof(Item);
                return null;
            }
        }
    
        [Serializable]
        public class Item
        {
            private string _name;
    
            public string Name
            {
                get { return _name; }
                set { _name = value; }
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                var item = new Item
                {
                    Name = "Orange"
                };
    
                var bytes = SerializeData(item);    
                var deserializedData = (Item)DeserializeData(bytes);
            }
    
            private static byte[] SerializeData(object obj)
            {
                var binaryFormatter = new BinaryFormatter();
                using (var memoryStream = new MemoryStream())
                {
                    binaryFormatter.Serialize(memoryStream, obj);
                    return memoryStream.ToArray();
                }
            }
    
            private static object DeserializeData(byte[] bytes)
            {
                var binaryFormatter = new BinaryFormatter
                {
                    Binder = new MyBinder()
                };
    
                using (var memoryStream = new MemoryStream(bytes))
                    return binaryFormatter.Deserialize(memoryStream);
            }
        }
    }



## Controlar el comportamiento de serialización con atributos
Si usa el atributo `[NonSerialized]`, ese miembro siempre tendrá su valor predeterminado después de la deserialización (por ejemplo, 0 para `int`, nulo para `string`, false para `bool`, etc.), independientemente de cualquier inicialización realizada en el propio objeto (constructores, declaraciones, etc.). Para compensar, se proporcionan los atributos `[OnDeserializing]` (llamado justo ANTES de la deserialización) y `[OnDeserialized]` (llamado justo DESPUÉS de la deserialización) junto con sus contrapartes, `[OnSerializing]` y `[OnSerialized]`.

Supongamos que queremos agregar una "Calificación" a nuestro Vector y queremos asegurarnos de que el valor siempre comience en 1. De la forma en que está escrito a continuación, será 0 después de ser deserializado:

    [Serializable]
    public class Vector
    {
        public int X;
        public int Y;
        public int Z;
    
        [NonSerialized]
        public decimal Rating = 1M;

        public Vector()
        {
            Rating = 1M;
        }

        public Vector(decimal initialRating)
        {
            Rating = initialRating;
        }
    }

Para solucionar este problema, simplemente podemos agregar el siguiente método dentro de la clase para establecerlo en 1:

    [OnDeserializing]
    void OnDeserializing(StreamingContext context)
    {
        Rating = 1M;
    }

O, si queremos configurarlo en un valor calculado, podemos esperar a que termine de deserializarse y luego configurarlo:

    [OnDeserialized]
    void OnDeserialized(StreamingContext context)
    {
        Rating = 1 + ((X+Y+Z)/3);
    }

De manera similar, podemos controlar cómo se escriben las cosas usando `[OnSerializing]` y `[OnSerialized]`.

## Algunas trampas en la compatibilidad con versiones anteriores
Este pequeño ejemplo muestra cómo puedes perder la retrocompatibilidad en tus programas si no te preocupas de antemano por esto. Y formas de obtener más control del proceso de serialización.

En primer lugar, escribiremos un ejemplo de la primera versión del programa:

Versión 1


    [Serializable]
    class Data
    {
        [OptionalField]
        private int _version;
        
        public int Version
        {
            get { return _version; }
            set { _version = value; }
        }
    }


Y ahora, supongamos que en la segunda versión del programa se agregó una nueva clase. Y necesitamos almacenarlo en una matriz.

Ahora el código se verá así:

Versión 2


    [Serializable]
    class NewItem
    {
        [OptionalField]
        private string _name;
    
        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }
    }
    
    [Serializable]
    class Data
    {
        [OptionalField]
        private int _version;
    
        public int Version
        {
            get { return _version; }
            set { _version = value; }
        }
    
        [OptionalField]
        private List<NewItem> _newItems;
    
        public List<NewItem> NewItems
        {
            get { return _newItems; }
            set { _newItems = value; }
        }
    }

Y código para serializar y deserializar


    private static byte[] SerializeData(object obj)
    {
        var binaryFormatter = new BinaryFormatter();
        using (var memoryStream = new MemoryStream())
        {
            binaryFormatter.Serialize(memoryStream, obj);
            return memoryStream.ToArray();
        }
    }
    
    private static object DeserializeData(byte[] bytes)
    {
        var binaryFormatter = new BinaryFormatter();
        using (var memoryStream = new MemoryStream(bytes))
            return binaryFormatter.Deserialize(memoryStream);
    }

Entonces, ¿qué pasaría si serializas los datos en el programa de v2 e intentas deserializarlos en el programa de v1?

Obtienes una excepción:

    System.Runtime.Serialization.SerializationException was unhandled
    Message=The ObjectManager found an invalid number of fixups. This usually indicates a problem in the Formatter.Source=mscorlib
    StackTrace:
       at System.Runtime.Serialization.ObjectManager.DoFixups()
       at System.Runtime.Serialization.Formatters.Binary.ObjectReader.Deserialize(HeaderHandler handler, __BinaryParser serParser, Boolean fCheck, Boolean isCrossAppDomain, IMethodCallMessage methodCallMessage)
       at System.Runtime.Serialization.Formatters.Binary.BinaryFormatter.Deserialize(Stream serializationStream, HeaderHandler handler, Boolean fCheck, Boolean isCrossAppDomain, IMethodCallMessage methodCallMessage)
       at System.Runtime.Serialization.Formatters.Binary.BinaryFormatter.Deserialize(Stream serializationStream)
       at Microsoft.Samples.TestV1.Main(String[] args) in c:\Users\andrew\Documents\Visual Studio 2013\Projects\vts\CS\V1 Application\TestV1Part2\TestV1Part2.cs:line 29
       at System.AppDomain._nExecuteAssembly(Assembly assembly, String[] args)
       at Microsoft.VisualStudio.HostingProcess.HostProc.RunUsersAssembly()
       at System.Threading.ExecutionContext.Run(ExecutionContext executionContext, ContextCallback callback, Object state)
       at System.Threading.ThreadHelper.ThreadStart()


¿Por qué?

El ObjectManager tiene una lógica diferente para resolver las dependencias de matrices y tipos de valor y referencia. Agregamos una matriz de nuevo tipo de referencia que está ausente en nuestro ensamblaje.

Cuando ObjectManager intenta resolver las dependencias, genera el gráfico. Cuando ve la matriz, no puede corregirla de inmediato, por lo que crea una referencia ficticia y luego corrige la matriz más tarde.

Y dado que este tipo no está en el ensamblado y las dependencias no se pueden arreglar. Por alguna razón, no elimina la matriz de la lista de elementos para las correcciones y, al final, arroja una excepción "IncorrectNumberOfFixups".

Son algunos 'trampas' en el proceso de serialización. Por alguna razón, no funciona correctamente solo para matrices de nuevos tipos de referencia.

    A Note:
    Similar code will work correctly if you do not use arrays with new classes

¿Y la primera forma de arreglarlo y mantener la compatibilidad?

- Utilice una colección de nuevas estructuras en lugar de clases o utilice un
diccionario(clases posibles), porque un diccionario es una colección
de keyvaluepair (su estructura)
- Use ISerializable, si no puede cambiar el código anterior



## Haciendo un objeto serializable
Agregue el atributo `[Serializable]` para marcar un objeto completo para la serialización binaria:

    [Serializable]
    public class Vector
    {
        public int X;
        public int Y;
        public int Z;

        [NonSerialized]
        public decimal DontSerializeThis;

        [OptionalField]
        public string Name;
    }

Todos los miembros se serializarán a menos que optemos explícitamente por no utilizar el atributo `[NonSerialized]`. En nuestro ejemplo, `X`, `Y`, `Z` y `Name` están serializados.

Todos los miembros deben estar presentes en la deserialización a menos que estén marcados con `[NonSerialized]` o `[OptionalField]`. En nuestro ejemplo, `X`, `Y` y `Z` son obligatorios y la deserialización fallará si no están presentes en la secuencia. `DontSerializeThis` siempre se establecerá en `default(decimal)` (que es 0). Si `Name` está presente en la transmisión, entonces se establecerá en ese valor; de lo contrario, se establecerá en `default(string)` (que es nulo). El propósito de `[OptionalField]` es proporcionar un poco de tolerancia a la versión.

## Sustitutos de serialización (Implementación de ISerializationSurrogate)
Implementa un selector sustituto de serialización que permite que un objeto realice la serialización y deserialización de otro

También permite serializar o deserializar correctamente una clase que no es en sí misma serializable


Implementar la interfaz ISerializationSurrogate

    public class ItemSurrogate : ISerializationSurrogate
    {
        public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
        {
            var item = (Item)obj;
            info.AddValue("_name", item.Name);
        }
    
        public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
        {
            var item = (Item)obj;
            item.Name = (string)info.GetValue("_name", typeof(string));
            return item;
        }
    }

Luego, debe informar a su IFormatter sobre los sustitutos definiendo e inicializando un SurrogateSelector y asignándolo a su IFormatter.



    var surrogateSelector = new SurrogateSelector();
    surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());    
    var binaryFormatter = new BinaryFormatter
    {
        SurrogateSelector = surrogateSelector
    };



Incluso si la clase no está marcada como serializable.

    //this class is not serializable
    public class Item
    {
        private string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }
    }



La solución completa


    using System;
    using System.IO;
    using System.Runtime.Serialization;
    using System.Runtime.Serialization.Formatters.Binary;
    
    namespace BinarySerializationExample
    {
        class Item
        {
            private string _name;
    
            public string Name
            {
                get { return _name; }
                set { _name = value; }
            }
        }
    
        class ItemSurrogate : ISerializationSurrogate
        {
            public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
            {
                var item = (Item)obj;
                info.AddValue("_name", item.Name);
            }
    
            public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
            {
                var item = (Item)obj;
                item.Name = (string)info.GetValue("_name", typeof(string));
                return item;
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                var item = new Item
                {
                    Name = "Orange"
                };
    
                var bytes = SerializeData(item);
                var deserializedData = (Item)DeserializeData(bytes);
            }
    
            private static byte[] SerializeData(object obj)
            {
                var surrogateSelector = new SurrogateSelector();
                surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());
    
                var binaryFormatter = new BinaryFormatter
                {
                    SurrogateSelector = surrogateSelector
                };
    
                using (var memoryStream = new MemoryStream())
                {
                    binaryFormatter.Serialize(memoryStream, obj);
                    return memoryStream.ToArray();
                }
            }
    
            private static object DeserializeData(byte[] bytes)
            {
                var surrogateSelector = new SurrogateSelector();
                surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());
    
                var binaryFormatter = new BinaryFormatter
                {
                    SurrogateSelector = surrogateSelector
                };
    
                using (var memoryStream = new MemoryStream(bytes))
                    return binaryFormatter.Deserialize(memoryStream);
            }
        }
    }



## Agregar más control implementando ISerializable
Eso obtendría más control sobre la serialización, cómo guardar y cargar tipos

Implemente la interfaz ISerializable y cree un constructor vacío para compilar

    [Serializable]
    public class Item : ISerializable
    {
        private string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

        public Item ()
        {

        }

        protected Item (SerializationInfo info, StreamingContext context)
        {
            _name = (string)info.GetValue("_name", typeof(string));
        }

        public void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.AddValue("_name", _name, typeof(string));
        }
    }



Para la serialización de datos, puede especificar el nombre deseado y el tipo deseado

    info.AddValue("_name", _name, typeof(string));

Cuando los datos se deserialicen, podrá leer el tipo deseado

    _name = (string)info.GetValue("_name", typeof(string));


