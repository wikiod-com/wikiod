---
title: "XmlSerializer"
slug: "xmlserializer"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

No utilice `XmlSerializer` para analizar `HTML`. Para esto, hay bibliotecas especiales disponibles como el [HTML Agility Pack][1]


[1]: https://htmlagilitypack.codeplex.com "Paquete de agilidad HTML"

## Formato: formato de fecha y hora personalizado
    public class Dog
    {
        private const string _birthStringFormat = "yyyy-MM-dd";

        [XmlIgnore]
        public DateTime Birth {get; set;}

        [XmlElement(ElementName="Birth")]
        public string BirthString
        {
            get { return Birth.ToString(_birthStringFormat); }
            set { Birth = DateTime.ParseExact(value, _birthStringFormat, CultureInfo.InvariantCulture); }
        }
    }


## Serializar objeto
    public void SerializeFoo(string fileName, Foo foo)
    {
        var serializer = new XmlSerializer(typeof(Foo));
        using (var stream = File.Open(fileName, FileMode.Create))
        {
            serializer.Serialize(stream, foo);
        }
    }


## Deserializar objeto
    public Foo DeserializeFoo(string fileName)
    {
        var serializer = new XmlSerializer(typeof(Foo));
        using (var stream = File.OpenRead(fileName))
        {
            return (Foo)serializer.Deserialize(stream);
        }
    }

## Comportamiento: Asignar nombre de matriz a propiedad (XmlArray)
    <Store>
        <Articles>
            <Product/>
            <Product/>
        </Articles>
    </Store>

-

    
    public class Store
    {
        [XmlArray("Articles")]
        public List<Product> Products {get; set; }
    }

## Comportamiento: Asignar nombre de elemento a propiedad
    <Foo>
        <Dog/>
    </Foo>

-

    public class Foo
    {
        // Using XmlElement
        [XmlElement(Name="Dog")]
        public Animal Cat { get; set; }
    }



## Construcción eficiente de múltiples serializadores con tipos derivados especificados dinámicamente
## De dónde venimos

A veces, no podemos proporcionar todos los metadatos necesarios para el marco XmlSerializer en el atributo. Supongamos que tenemos una clase base de objetos serializados y algunas de las clases derivadas son desconocidas para la clase base. No podemos colocar un atributo para todas las clases que no se conocen en el momento del diseño del tipo base. Podríamos tener otro equipo desarrollando algunas de las clases derivadas.

## Qué podemos hacer

Podemos usar `new XmlSerializer(type,knownTypes)`, pero sería una operación O(N^2) para N serializadores, al menos para descubrir todos los tipos provistos en los argumentos:

    // Beware of the N^2 in terms of the number of types.
    var allSerializers = allTypes.Select(t => new XmlSerializer(t, allTypes));
    var serializerDictionary = Enumerable.Range(0, allTypes.Length)
        .ToDictionary (i => allTypes[i], i => allSerializers[i])

En este ejemplo, el tipo Base no conoce sus tipos derivados, lo cual es normal en OOP.

## Hacerlo de manera eficiente

Afortunadamente, existe un método que aborda este problema en particular: proporciona tipos conocidos para múltiples serializadores de manera eficiente:

[Método System.Xml.Serialization.XmlSerializer.FromTypes (Tipo\[\])][1]

> El método FromTypes le permite crear de manera eficiente una matriz de objetos XmlSerializer para procesar una matriz de objetos Type.

    var allSerializers = XmlSerializer.FromTypes(allTypes);
    var serializerDictionary = Enumerable.Range(0, allTypes.Length)
        .ToDictionary(i => allTypes[i], i => allSerializers[i]);

Aquí hay una muestra de código completa:

    using System;
    using System.Collections.Generic;
    using System.Xml.Serialization;
    using System.Linq;
    using System.Linq;
                        
    public class Program
    {
        public class Container
        {
            public Base Base { get; set; }
        }
        
        public class Base
        {
            public int JustSomePropInBase { get; set; }
        }
        
        public class Derived : Base
        {
            public int JustSomePropInDerived { get; set; }
        }
        
        public void Main()
        {
            var sampleObject = new Container { Base = new Derived() };
            var allTypes = new[] { typeof(Container), typeof(Base), typeof(Derived) };
            
            Console.WriteLine("Trying to serialize without a derived class metadata:");
            SetupSerializers(allTypes.Except(new[] { typeof(Derived) }).ToArray());
            try
            {
                Serialize(sampleObject);
            }
            catch (InvalidOperationException e)
            {
                Console.WriteLine();
                Console.WriteLine("This error was anticipated,");
                Console.WriteLine("we have not supplied a derived class.");
                Console.WriteLine(e);
            }
            Console.WriteLine("Now trying to serialize with all of the type information:");
            SetupSerializers(allTypes);
            Serialize(sampleObject);
            Console.WriteLine();
            Console.WriteLine("Slides down well this time!");
        }
    
        static void Serialize<T>(T o)
        {
            serializerDictionary[typeof(T)].Serialize(Console.Out, o);
        }
    
        private static Dictionary<Type, XmlSerializer> serializerDictionary;
        
        static void SetupSerializers(Type[] allTypes)
        {
            var allSerializers = XmlSerializer.FromTypes(allTypes);
            serializerDictionary = Enumerable.Range(0, allTypes.Length)
                .ToDictionary(i => allTypes[i], i => allSerializers[i]);
        }
    }

Producción:

    Trying to serialize without a derived class metadata:
    <?xml version="1.0" encoding="utf-16"?>
    <Container xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    This error was anticipated,
    we have not supplied a derived class.
    System.InvalidOperationException: There was an error generating the XML document. ---> System.InvalidOperationException: The type Program+Derived was not expected. Use the XmlInclude or SoapInclude attribute to specify types that are not known statically.
       at Microsoft.Xml.Serialization.GeneratedAssembly.XmlSerializationWriter1.Write2_Base(String n, String ns, Base o, Boolean isNullable, Boolean needType)
       at Microsoft.Xml.Serialization.GeneratedAssembly.XmlSerializationWriter1.Write3_Container(String n, String ns, Container o, Boolean isNullable, Boolean needType)
       at Microsoft.Xml.Serialization.GeneratedAssembly.XmlSerializationWriter1.Write4_Container(Object o)
       at System.Xml.Serialization.XmlSerializer.Serialize(XmlWriter xmlWriter, Object o, XmlSerializerNamespaces namespaces, String encodingStyle, String id)
       --- End of inner exception stack trace ---
       at System.Xml.Serialization.XmlSerializer.Serialize(XmlWriter xmlWriter, Object o, XmlSerializerNamespaces namespaces, String encodingStyle, String id)
       at System.Xml.Serialization.XmlSerializer.Serialize(XmlWriter xmlWriter, Object o, XmlSerializerNamespaces namespaces, String encodingStyle)
       at System.Xml.Serialization.XmlSerializer.Serialize(XmlWriter xmlWriter, Object o, XmlSerializerNamespaces namespaces)
       at Program.Serialize[T](T o)
       at Program.Main()
    Now trying to serialize with all of the type information:
    <?xml version="1.0" encoding="utf-16"?>
    <Container xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
      <Base xsi:type="Derived">
        <JustSomePropInBase>0</JustSomePropInBase>
        <JustSomePropInDerived>0</JustSomePropInDerived>
      </Base>
    </Container>
    Slides down well this time!

### Qué hay en la salida

Este mensaje de error recomienda lo que tratamos de evitar (o lo que no podemos hacer en algunos escenarios): hacer referencia a tipos derivados de la clase base:

`Utilice el atributo XmlInclude o SoapInclude para especificar tipos que no se conocen estáticamente.`

Así es como obtenemos nuestra clase derivada en el XML:

`<Base xsi:tipo="Derivado">`

`Base` corresponde al tipo de propiedad declarada en el tipo `Contenedor`, siendo `Derivado` el tipo de la instancia realmente suministrada.

Aquí hay un [violín de ejemplo] en funcionamiento (https://dotnetfiddle.net/hufepI)

[1]: https://msdn.microsoft.com/en-us/library/system.xml.serialization.xmlserializer.fromtypes(v=vs.110).aspx#Anchor_1

