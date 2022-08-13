---
title: "Sérialisation binaire"
slug: "serialisation-binaire"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

Le moteur de sérialisation binaire fait partie du framework .NET, mais les exemples donnés ici sont spécifiques à C#. Par rapport aux autres moteurs de sérialisation intégrés au framework .NET, le sérialiseur binaire est rapide et efficace et nécessite généralement très peu de code supplémentaire pour le faire fonctionner. Cependant, il est également moins tolérant aux changements de code ; c'est-à-dire que si vous sérialisez un objet, puis modifiez légèrement la définition de l'objet, il ne se désérialisera probablement pas correctement.

## Classeur de sérialisation
Le classeur vous permet d'inspecter les types chargés dans votre domaine d'application

Créer une classe héritée de SerializationBinder

    class MyBinder : SerializationBinder
    {
        public override Type BindToType(string assemblyName, string typeName)
        {
            if (typeName.Equals("BinarySerializationExample.Item"))
                return typeof(Item);
            return null;
        }
    }


Maintenant, nous pouvons vérifier quels types sont en cours de chargement et, sur cette base, décider ce que nous voulons vraiment recevoir

Pour utiliser un classeur, vous devez l'ajouter à BinaryFormatter.

    object DeserializeData(byte[] bytes)
    {
        var binaryFormatter = new BinaryFormatter();
        binaryFormatter.Binder = new MyBinder();

        using (var memoryStream = new MemoryStream(bytes))
            return binaryFormatter.Deserialize(memoryStream);
    }


La solution complète

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



## Contrôler le comportement de sérialisation avec des attributs
Si vous utilisez l'attribut `[NonSerialized]`, alors ce membre aura toujours sa valeur par défaut après la désérialisation (ex. 0 pour un `int`, null pour `string`, false pour un `bool`, etc.), indépendamment de toute initialisation effectuée dans l'objet lui-même (constructeurs, déclarations, etc.). Pour compenser, les attributs `[OnDeserializing]` (appelé juste AVANT la désérialisation) et `[OnDeserialized]` (appelé juste APRÈS la désérialisation) ainsi que leurs homologues, `[OnSerializing]` et `[OnSerialized]` sont fournis.

Supposons que nous voulions ajouter un "Rating" à notre Vector et que nous voulions nous assurer que la valeur commence toujours à 1. Comme c'est écrit ci-dessous, ce sera 0 après avoir été désérialisé :

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

Pour résoudre ce problème, nous pouvons simplement ajouter la méthode suivante à l'intérieur de la classe pour la définir sur 1 :

    [OnDeserializing]
    void OnDeserializing(StreamingContext context)
    {
        Rating = 1M;
    }

Ou, si nous voulons le définir sur une valeur calculée, nous pouvons attendre que la désérialisation soit terminée, puis le définir :

    [OnDeserialized]
    void OnDeserialized(StreamingContext context)
    {
        Rating = 1 + ((X+Y+Z)/3);
    }

De même, nous pouvons contrôler la façon dont les choses sont écrites en utilisant `[OnSerializing]` et `[OnSerialized]`.

## Quelques pièges dans la rétrocompatibilité
Ce petit exemple montre comment vous pouvez perdre la rétrocompatibilité dans vos programmes si vous ne vous en occupez pas à l'avance. Et des moyens de mieux contrôler le processus de sérialisation

Dans un premier temps, nous allons écrire un exemple de la première version du programme :

Version 1


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


Et maintenant, supposons que dans la deuxième version du programme a ajouté une nouvelle classe. Et nous devons le stocker dans un tableau.

Maintenant, le code ressemblera à ceci :

Version 2


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

Et code pour sérialiser et désérialiser


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

Et donc, que se passerait-il lorsque vous sérialiseriez les données dans le programme de la v2 et tenteriez de les désérialiser dans le programme de la v1 ?

Vous obtenez une exception :

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


Pourquoi?

L'ObjectManager a une logique différente pour résoudre les dépendances pour les tableaux et pour les types référence et valeur. Nous avons ajouté un tableau du nouveau type de référence qui est absent de notre assemblage.

Lorsque ObjectManager tente de résoudre les dépendances, il construit le graphique. Lorsqu'il voit le tableau, il ne peut pas le réparer immédiatement, de sorte qu'il crée une référence factice, puis corrige le tableau plus tard.

Et puisque ce type n'est pas dans l'assembly et que les dépendances ne peuvent pas être corrigées. Pour une raison quelconque, il ne supprime pas le tableau de la liste des éléments pour les correctifs et à la fin, il lève une exception "IncorrectNumberOfFixups".

Il s'agit de quelques « pièges » dans le processus de sérialisation. Pour une raison quelconque, cela ne fonctionne pas correctement uniquement pour les tableaux de nouveaux types de référence.

    A Note:
    Similar code will work correctly if you do not use arrays with new classes

Et la première façon de le réparer et de maintenir la compatibilité ?

- Utilisez une collection de nouvelles structures plutôt que des classes ou utilisez un
dictionnaire (classes possibles), car un dictionnaire c'est une collection
de keyvaluepair (c'est la structure)
- Utilisez ISerializable, si vous ne pouvez pas changer l'ancien code



## Rendre un objet sérialisable
Ajoutez l'attribut `[Serializable]` pour marquer un objet entier pour la sérialisation binaire :

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

Tous les membres seront sérialisés à moins que nous nous désactivions explicitement à l'aide de l'attribut `[NonSerialized]`. Dans notre exemple, `X`, `Y`, `Z` et `Name` sont tous sérialisés.

Tous les membres doivent être présents lors de la désérialisation, sauf s'ils sont marqués par `[NonSerialized]` ou `[OptionalField]`. Dans notre exemple, 'X', 'Y' et 'Z' sont tous requis et la désérialisation échouera s'ils ne sont pas présents dans le flux. `DontSerializeThis` sera toujours défini sur `default(decimal)` (qui est 0). Si `Name` est présent dans le flux, il sera défini sur cette valeur, sinon il sera défini sur `default(string)` (qui est nul). Le but de `[OptionalField]` est de fournir un peu de tolérance de version.

## Substituts de sérialisation (Implémentation de ISerializationSurrogate)
Implémente un sélecteur de substitution de sérialisation qui permet à un objet d'effectuer la sérialisation et la désérialisation d'un autre

Permet également de sérialiser ou désérialiser correctement une classe qui n'est pas elle-même sérialisable


Implémenter l'interface ISerializationSurrogate

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

Ensuite, vous devez informer votre IFormatter des substituts en définissant et en initialisant un SurrogateSelector et en l'affectant à votre IFormatter



    var surrogateSelector = new SurrogateSelector();
    surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());    
    var binaryFormatter = new BinaryFormatter
    {
        SurrogateSelector = surrogateSelector
    };



Même si la classe n'est pas marquée sérialisable.

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



La solution complète


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



## Ajout de plus de contrôle en implémentant ISerializable
Cela donnerait plus de contrôle sur la sérialisation, comment enregistrer et charger les types

Implémenter l'interface ISerializable et créer un constructeur vide à compiler

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



Pour la sérialisation des données, vous pouvez spécifier le nom souhaité et le type souhaité

    info.AddValue("_name", _name, typeof(string));

Lorsque les données seront désérialisées, vous pourrez lire le type souhaité

    _name = (string)info.GetValue("_name", typeof(string));


