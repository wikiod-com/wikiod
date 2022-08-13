---
title: "XmlSerializer"
slug: "xmlserializer"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

Não use o `XmlSerializer` para analisar `HTML`. Para isso, estão disponíveis bibliotecas especiais como o [HTML Agility Pack][1]


[1]: https://htmlagilitypack.codeplex.com "Pacote de agilidade HTML"

## Formatação: formato personalizado DateTime
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


## Serialize o objeto
    public void SerializeFoo(string fileName, Foo foo)
    {
        var serializer = new XmlSerializer(typeof(Foo));
        using (var stream = File.Open(fileName, FileMode.Create))
        {
            serializer.Serialize(stream, foo);
        }
    }


## Desserializar objeto
    public Foo DeserializeFoo(string fileName)
    {
        var serializer = new XmlSerializer(typeof(Foo));
        using (var stream = File.OpenRead(fileName))
        {
            return (Foo)serializer.Deserialize(stream);
        }
    }

## Comportamento: Mapeie o nome do array para a propriedade (XmlArray)
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

## Comportamento: Mapear o nome do elemento para a propriedade
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



## Construindo com eficiência vários serializadores com tipos derivados especificados dinamicamente
## De onde viemos

Às vezes, não podemos fornecer todos os metadados necessários para a estrutura XmlSerializer no atributo. Suponha que temos uma classe base de objetos serializados e algumas das classes derivadas são desconhecidas para a classe base. Não podemos colocar um atributo para todas as classes que não são conhecidas no tempo de design do tipo base. Poderíamos ter outra equipe desenvolvendo algumas das classes derivadas.

## O que podemos fazer

Podemos usar `new XmlSerializer(type, knownTypes)`, mas isso seria uma operação O(N^2) para N serializadores, pelo menos para descobrir todos os tipos fornecidos em argumentos:

    // Beware of the N^2 in terms of the number of types.
    var allSerializers = allTypes.Select(t => new XmlSerializer(t, allTypes));
    var serializerDictionary = Enumerable.Range(0, allTypes.Length)
        .ToDictionary (i => allTypes[i], i => allSerializers[i])

Neste exemplo, o tipo Base não está ciente de seus tipos derivados, o que é normal na OOP.

## Fazendo isso com eficiência

Felizmente, existe um método que resolve esse problema específico - fornecendo tipos conhecidos para vários serializadores de forma eficiente:

[System.Xml.Serialization.XmlSerializer.FromTypes Method (Type\[\])][1]

> O método FromTypes permite criar com eficiência uma matriz de objetos XmlSerializer para processar uma matriz de objetos Type.

    var allSerializers = XmlSerializer.FromTypes(allTypes);
    var serializerDictionary = Enumerable.Range(0, allTypes.Length)
        .ToDictionary(i => allTypes[i], i => allSerializers[i]);

Aqui está um exemplo de código completo:

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

Resultado:

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

### O que está na saída

Esta mensagem de erro recomenda o que tentamos evitar (ou o que não podemos fazer em alguns cenários) - referenciando tipos derivados da classe base:

`Use o atributo XmlInclude ou SoapInclude para especificar tipos que não são conhecidos estaticamente.`

É assim que obtemos nossa classe derivada no XML:

`<Base xsi:type="Derived">`

`Base` corresponde ao tipo de propriedade declarado no tipo `Container`, e `Derived` é o tipo da instância realmente fornecida.

Aqui está um [exemplo de violino](https://dotnetfiddle.net/hufepI) funcionando

[1]: https://msdn.microsoft.com/en-us/library/system.xml.serialization.xmlserializer.fromtypes(v=vs.110).aspx#Anchor_1

