---
title: "XmlSerializerXmlSerializer"
slug: "xmlserializerxmlserializer"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

N'utilisez pas `XmlSerializer` pour analyser `HTML`. Pour cela, des bibliothèques spéciales sont disponibles comme le [HTML Agility Pack][1]


[1] : https://htmlagilitypack.codeplex.com "Pack Agilité HTML"

## Formatage : format DateHeure personnalisé
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


## Sérialiser l'objet
    public void SerializeFoo(string fileName, Foo foo)
    {
        var serializer = new XmlSerializer(typeof(Foo));
        using (var stream = File.Open(fileName, FileMode.Create))
        {
            serializer.Serialize(stream, foo);
        }
    }


## Désérialiser l'objet
    public Foo DeserializeFoo(string fileName)
    {
        var serializer = new XmlSerializer(typeof(Foo));
        using (var stream = File.OpenRead(fileName))
        {
            return (Foo)serializer.Deserialize(stream);
        }
    }

## Comportement : mapper le nom du tableau à la propriété (XmlArray)
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

## Comportement : mapper le nom de l'élément à la propriété
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



## Construire efficacement plusieurs sérialiseurs avec des types dérivés spécifiés dynamiquement
## D'où nous venons

Parfois, nous ne pouvons pas fournir toutes les métadonnées requises pour le framework XmlSerializer dans l'attribut. Supposons que nous ayons une classe de base d'objets sérialisés et que certaines des classes dérivées soient inconnues de la classe de base. Nous ne pouvons pas placer un attribut pour toutes les classes qui ne sont pas connues au moment de la conception du type de base. Nous pourrions avoir une autre équipe développant certaines des classes dérivées.

## Que pouvons-nous faire

Nous pouvons utiliser `new XmlSerializer(type,knownTypes)`, mais ce serait une opération O(N^2) pour N sérialiseurs, au moins pour découvrir tous les types fournis en arguments :

    // Beware of the N^2 in terms of the number of types.
    var allSerializers = allTypes.Select(t => new XmlSerializer(t, allTypes));
    var serializerDictionary = Enumerable.Range(0, allTypes.Length)
        .ToDictionary (i => allTypes[i], i => allSerializers[i])

Dans cet exemple, le type Base n'est pas conscient de ses types dérivés, ce qui est normal en POO.

## Le faire efficacement

Heureusement, il existe une méthode qui résout ce problème particulier - en fournissant efficacement des types connus pour plusieurs sérialiseurs :

[Méthode System.Xml.Serialization.XmlSerializer.FromTypes (Type\[\])][1]

> La méthode FromTypes vous permet de créer efficacement un tableau d'objets XmlSerializer pour traiter un tableau d'objets Type.

    var allSerializers = XmlSerializer.FromTypes(allTypes);
    var serializerDictionary = Enumerable.Range(0, allTypes.Length)
        .ToDictionary(i => allTypes[i], i => allSerializers[i]);

Voici un exemple de code complet :

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

Production:

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

### Contenu de la sortie

Ce message d'erreur recommande ce que nous avons essayé d'éviter (ou ce que nous ne pouvons pas faire dans certains scénarios) - référencer les types dérivés de la classe de base :

`Utilisez l'attribut XmlInclude ou SoapInclude pour spécifier des types qui ne sont pas connus statiquement.`

Voici comment nous obtenons notre classe dérivée dans le XML :

`<Base xsi:type="Dérivé">`

`Base` correspond au type de propriété déclaré dans le type `Container`, et `Derived` étant le type de l'instance réellement fournie.

Voici un [exemple de violon] fonctionnel (https://dotnetfiddle.net/hufepI)

[1] : https://msdn.microsoft.com/en-us/library/system.xml.serialization.xmlserializer.fromtypes(v=vs.110).aspx#Anchor_1

