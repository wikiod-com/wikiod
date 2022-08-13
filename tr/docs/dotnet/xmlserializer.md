---
title: "XmlSerializer"
slug: "xmlserializer"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

"HTML"yi ayrıştırmak için "XmlSerializer"ı kullanmayın. Bunun için [HTML Agility Pack][1] gibi özel kütüphaneler mevcuttur.


[1]: https://htmlagilitypack.codeplex.com "HTML Çeviklik Paketi"

## Biçimlendirme: Özel DateTime biçimi
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


## Nesneyi seri hale getir
    public void SerializeFoo(string fileName, Foo foo)
    {
        var serializer = new XmlSerializer(typeof(Foo));
        using (var stream = File.Open(fileName, FileMode.Create))
        {
            serializer.Serialize(stream, foo);
        }
    }


## Nesneyi seri durumdan çıkar
    public Foo DeserializeFoo(string fileName)
    {
        var serializer = new XmlSerializer(typeof(Foo));
        using (var stream = File.OpenRead(fileName))
        {
            return (Foo)serializer.Deserialize(stream);
        }
    }

## Davranış: Dizi adını özellik ile eşleyin (XmlArray)
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

## Davranış: Öğe adını Özelliğe Eşle
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



## Dinamik olarak belirtilen türetilmiş türlerle birden çok serileştiriciyi verimli bir şekilde oluşturma
## Nereden Geldik

Bazen, öznitelikte XmlSerializer çerçevesi için gereken tüm gerekli meta verileri sağlayamayız. Serileştirilmiş nesnelerden oluşan bir temel sınıfımız olduğunu ve bazı türetilmiş sınıfların temel sınıf tarafından bilinmediğini varsayalım. Temel türün tasarım zamanında bilinmeyen tüm sınıflar için bir öznitelik yerleştiremeyiz. Türetilmiş sınıflardan bazılarını geliştiren başka bir ekibimiz olabilir.

## Ne yapabiliriz

`new XmlSerializer(type,knownTypes)` kullanabiliriz, ancak bu, en azından argümanlarda sağlanan tüm türleri keşfetmek için N serileştiriciler için bir O(N^2) işlemi olacaktır:

    // Beware of the N^2 in terms of the number of types.
    var allSerializers = allTypes.Select(t => new XmlSerializer(t, allTypes));
    var serializerDictionary = Enumerable.Range(0, allTypes.Length)
        .ToDictionary (i => allTypes[i], i => allSerializers[i])

Bu örnekte, Base türü, OOP'de normal olan türetilmiş türlerinin farkında değildir.

## Verimli bir şekilde yapmak

Neyse ki, bu özel sorunu ele alan bir yöntem var - birden çok serileştirici için bilinen türleri verimli bir şekilde sağlamak:

[System.Xml.Serialization.XmlSerializer.FromTypes Yöntemi (Type\[\])][1]

> FromTypes yöntemi, bir Type nesnesi dizisini işlemek için verimli bir şekilde bir XmlSerializer nesne dizisi oluşturmanıza olanak tanır.

    var allSerializers = XmlSerializer.FromTypes(allTypes);
    var serializerDictionary = Enumerable.Range(0, allTypes.Length)
        .ToDictionary(i => allTypes[i], i => allSerializers[i]);

İşte tam bir kod örneği:

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

Çıktı:

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

### Çıktıda neler var

Bu hata mesajı, nelerden kaçınmaya çalıştığımızı (veya bazı senaryolarda ne yapamadığımızı) önerir - temel sınıftan türetilmiş türlere atıfta bulunur:

`Statik olarak bilinmeyen türleri belirtmek için XmlInclude veya SoapInclude özniteliğini kullanın.`

XML'de türetilmiş sınıfımızı şu şekilde elde ederiz:

`<Base xsi:type="Türetilen">`

"Temel", "Kapsayıcı" türünde bildirilen özellik türüne karşılık gelir ve "Türetilmiş", fiilen sağlanan örneğin türüdür.

İşte çalışan bir [örnek keman](https://dotnetfiddle.net/hufepI)

[1]: https://msdn.microsoft.com/en-us/library/system.xml.serialization.xmlserializer.fromtypes(v=vs.110).aspx#Anchor_1

