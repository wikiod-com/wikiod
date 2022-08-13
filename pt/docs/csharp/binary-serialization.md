---
title: "Serialização Binária"
slug: "serializacao-binaria"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

O mecanismo de serialização binária faz parte da estrutura .NET, mas os exemplos fornecidos aqui são específicos para C#. Em comparação com outros mecanismos de serialização integrados à estrutura .NET, o serializador binário é rápido e eficiente e geralmente requer muito pouco código extra para funcionar. No entanto, também é menos tolerante a alterações de código; ou seja, se você serializar um objeto e, em seguida, fizer uma pequena alteração na definição do objeto, ele provavelmente não será desserializado corretamente.

## Pasta de serialização
O fichário oferece a oportunidade de inspecionar quais tipos estão sendo carregados no domínio do aplicativo

Criar uma classe herdada de SerializationBinder

    class MyBinder : SerializationBinder
    {
        public override Type BindToType(string assemblyName, string typeName)
        {
            if (typeName.Equals("BinarySerializationExample.Item"))
                return typeof(Item);
            return null;
        }
    }


Agora podemos verificar quais tipos estão carregando e com base nisso decidir o que realmente queremos receber

Para usar um fichário, você deve adicioná-lo ao BinaryFormatter.

    object DeserializeData(byte[] bytes)
    {
        var binaryFormatter = new BinaryFormatter();
        binaryFormatter.Binder = new MyBinder();

        using (var memoryStream = new MemoryStream(bytes))
            return binaryFormatter.Deserialize(memoryStream);
    }


A solução completa

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



## Controlando o comportamento de serialização com atributos
Se você usar o atributo `[NonSerialized]`, esse membro sempre terá seu valor padrão após a desserialização (ex. 0 para um `int`, null para `string`, false para um `bool` etc.), independentemente de qualquer inicialização feita no próprio objeto (construtores, declarações, etc.). Para compensar, os atributos `[OnDeserializing]` (chamado apenas ANTES de desserializar) e `[OnDeserialized]` (chamado apenas DEPOIS de desserializar) juntamente com suas contrapartes, `[OnSerializing]` e `[OnSerialized]` são fornecidos.

Suponha que queremos adicionar um "Rating" ao nosso Vetor e queremos garantir que o valor sempre comece em 1. Da forma como está escrito abaixo, será 0 após ser desserializado:

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

Para corrigir esse problema, podemos simplesmente adicionar o seguinte método dentro da classe para defini-lo como 1:

    [OnDeserializing]
    void OnDeserializing(StreamingContext context)
    {
        Rating = 1M;
    }

Ou, se quisermos defini-lo para um valor calculado, podemos esperar que termine de desserializar e depois defini-lo:

    [OnDeserialized]
    void OnDeserialized(StreamingContext context)
    {
        Rating = 1 + ((X+Y+Z)/3);
    }

Da mesma forma, podemos controlar como as coisas são escritas usando `[OnSerializing]` e `[OnSerialized]`.

## Algumas pegadinhas na compatibilidade com versões anteriores
Este pequeno exemplo mostra como você pode perder a compatibilidade com versões anteriores em seus programas se você não tomar cuidado com isso com antecedência. E maneiras de obter mais controle do processo de serialização

A princípio, escreveremos um exemplo da primeira versão do programa:

Versão 1


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


E agora, vamos supor que na segunda versão do programa foi adicionada uma nova classe. E precisamos armazená-lo em uma matriz.

Agora o código ficará assim:

Versão 2


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

E código para serializar e desserializar


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

E então, o que aconteceria quando você serializar os dados no programa da v2 e tentar desserializá-los no programa da v1?

Você recebe uma exceção:

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


Por quê?

O ObjectManager tem uma lógica diferente para resolver dependências para arrays e para tipos de referência e valor. Adicionamos uma matriz de novo tipo de referência que está ausente em nosso assembly.

Quando o ObjectManager tenta resolver dependências, ele constrói o gráfico. Quando ele vê o array, ele não pode corrigi-lo imediatamente, então ele cria uma referência fictícia e depois corrige o array mais tarde.

E como esse tipo não está no assembly e as dependências não podem ser corrigidas. Por algum motivo, ele não remove o array da lista de elementos para as correções e, no final, lança uma exceção “IncorrectNumberOfFixups”.

São algumas 'pegadinhas' no processo de serialização. Por algum motivo, não funciona corretamente apenas para arrays de novos tipos de referência.

    A Note:
    Similar code will work correctly if you do not use arrays with new classes

E a primeira maneira de corrigi-lo e manter a compatibilidade?

- Use uma coleção de novas estruturas em vez de classes ou use um
dicionário(classes possíveis), porque um dicionário é uma coleção
de keyvaluepair (sua estrutura)
- Use ISerializable, caso não consiga alterar o código antigo



## Tornando um objeto serializável
Adicione o atributo `[Serializable]` para marcar um objeto inteiro para serialização binária:

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

Todos os membros serão serializados, a menos que desativemos explicitamente o uso do atributo `[NonSerialized]`. Em nosso exemplo, `X`, `Y`, `Z` e `Name` são todos serializados.

Todos os membros devem estar presentes na desserialização, a menos que estejam marcados com `[NonSerialized]` ou `[OptionalField]`. Em nosso exemplo, `X`, `Y` e `Z` são todos necessários e a desserialização falhará se eles não estiverem presentes no fluxo. `DontSerializeThis` sempre será definido como `default(decimal)` (que é 0). Se `Name` estiver presente no stream, ele será definido com esse valor, caso contrário, será definido como `default(string)` (que é nulo). A finalidade de `[OptionalField]` é fornecer um pouco de tolerância de versão.

## Substitutos de serialização (Implementação de ISerializationSurrogate)
Implementa um seletor substituto de serialização que permite que um objeto execute a serialização e desserialização de outro

Também permite serializar ou desserializar corretamente uma classe que não é serializável


Implementar a interface ISerializationSurrogate

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

Então você precisa informar seu IFormatter sobre os substitutos definindo e inicializando um SurrogateSelector e atribuindo-o ao seu IFormatter



    var surrogateSelector = new SurrogateSelector();
    surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());    
    var binaryFormatter = new BinaryFormatter
    {
        SurrogateSelector = surrogateSelector
    };



Mesmo que a classe não esteja marcada como serializável.

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



A solução completa


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



## Adicionando mais controle implementando ISerializable
Isso teria mais controle sobre a serialização, como salvar e carregar tipos

Implemente a interface ISerializable e crie um construtor vazio para compilar

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



Para serialização de dados, você pode especificar o nome desejado e o tipo desejado

    info.AddValue("_name", _name, typeof(string));

Quando os dados forem desserializados, você poderá ler o tipo desejado

    _name = (string)info.GetValue("_name", typeof(string));


