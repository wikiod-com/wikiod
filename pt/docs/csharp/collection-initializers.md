---
title: "Inicializadores de coleção"
slug: "inicializadores-de-colecao"
draft: false
images: []
weight: 9759
type: docs
toc: true
---

O único requisito para que um objeto seja inicializado usando este açúcar sintático é que o tipo implemente `System.Collections.IEnumerable` e o método `Add`. Embora o chamemos de inicializador de coleção, o objeto *não* precisa ser uma coleção.

## Inicializadores de coleção
Inicialize um tipo de coleção com valores:

    var stringList = new List<string>
    {
        "foo",
        "bar",
    };

Inicializadores de coleção são açúcar sintático para chamadas `Add()`. O código acima é equivalente a:

    var temp = new List<string>();
    temp.Add("foo");
    temp.Add("bar");
    var stringList = temp;

Observe que a inicialização é feita atomicamente usando uma variável temporária, para evitar condições de corrida.

Para tipos que oferecem vários parâmetros em seu método `Add()`, coloque os argumentos separados por vírgulas entre chaves:

    var numberDictionary = new Dictionary<int, string>
    {
        { 1, "One" },
        { 2, "Two" },
    };

Isso é equivalente a:

    var temp = new Dictionary<int, string>();
    temp.Add(1, "One");
    temp.Add(2, "Two");
    var numberDictionarynumberDictionary = temp;


## Inicializadores de índice C# 6
A partir do C# 6, as coleções com indexadores podem ser inicializadas especificando o índice a ser atribuído entre colchetes, seguido por um sinal de igual, seguido pelo valor a ser atribuído.

# Inicialização do dicionário

Um exemplo dessa sintaxe usando um dicionário:

    var dict = new Dictionary<string, int>
    {
        ["key1"] = 1,
        ["key2"] = 50
    };

Isso é equivalente a:

    var dict = new Dictionary<string, int>();
    dict["key1"] = 1;
    dict["key2"] = 50

A sintaxe do inicializador de coleção para fazer isso antes do C# 6 era:

    var dict = new Dictionary<string, int>
    {
        { "key1", 1 },
        { "key2", 50 }
    };
    
O que corresponderia a:

    var dict = new Dictionary<string, int>();
    dict.Add("key1", 1);
    dict.Add("key2", 50);


Portanto, há uma diferença significativa na funcionalidade, pois a nova sintaxe usa o *indexador* do objeto inicializado para atribuir valores em vez de usar seu método `Add()`. Isso significa que a nova sintaxe requer apenas um indexador disponível publicamente e funciona para qualquer objeto que tenha um.

    public class IndexableClass
    {
        public int this[int index]
        {
            set 
            { 
                Console.WriteLine("{0} was assigned to index {1}", value, index);
            }
        }
    }

    var foo = new IndexableClass
    {
        [0] = 10,
        [1] = 20
    }

Isso geraria:

> `10 foi atribuído ao índice 0`<br/>
> `20 foi atribuído ao índice 1`



## Inicializadores de coleção em classes personalizadas
Para fazer com que uma classe suporte inicializadores de coleção, ela deve implementar a interface `IEnumerable` e ter pelo menos um método `Add`. Desde o C# 6, qualquer coleção que implemente `IEnumerable` pode ser estendida com métodos `Add` personalizados usando métodos de extensão.

    class Program
    {
        static void Main()
        {
            var col = new MyCollection {
                "foo",
                { "bar", 3 },
                "baz",
                123.45d,
            };
        }
    }
    
    class MyCollection : IEnumerable
    {
        private IList list = new ArrayList();

        public void Add(string item)
        {
            list.Add(item)
        }
    
        public void Add(string item, int count)
        {
            for(int i=0;i< count;i++) {
                list.Add(item);
            }
        }
    
        public IEnumerator GetEnumerator()
        {
            return list.GetEnumerator();
        }
    }
    
    static class MyCollectionExtensions
    {
        public static void Add(this MyCollection @this, double value) => 
            @this.Add(value.ToString());
    }



## Usando o inicializador de coleção dentro do inicializador de objeto
    public class Tag
    {
        public IList<string> Synonyms { get; set; }
    }

`Synonyms` é uma propriedade do tipo coleção. Quando o objeto `Tag` é criado usando a sintaxe do inicializador de objeto, `Synonyms` também pode ser inicializado com a sintaxe do inicializador de coleção:

    Tag t = new Tag 
    {
        Synonyms = new List<string> {"c#", "c-sharp"}
    };

A propriedade de coleção pode ser somente leitura e ainda oferecer suporte à sintaxe do inicializador de coleção. Considere este exemplo modificado (a propriedade `Synonyms` agora tem um setter privado):

    public class Tag
    {
        public Tag()
        {
            Synonyms = new List<string>();
        }
        
        public IList<string> Synonyms { get; private set; }
    }

Um novo objeto `Tag` pode ser criado assim:

    Tag t = new Tag 
    {
        Synonyms = {"c#", "c-sharp"}
    };

Isso funciona porque os inicializadores de coleção são apenas açúcar sintático sobre chamadas para `Add()`. Não há nenhuma nova lista sendo criada aqui, o compilador está apenas gerando chamadas para `Add()` no objeto de saída.

## Inicializadores de coleção com matrizes de parâmetros
Você pode misturar parâmetros normais e matrizes de parâmetros:

    public class LotteryTicket : IEnumerable{
        public int[] LuckyNumbers;
        public string UserName;

        public void Add(string userName, params int[] luckyNumbers){
            UserName = userName;
            Lottery = luckyNumbers;
        }
    }

Esta sintaxe agora é possível:

    var Tickets = new List<LotteryTicket>{
        {"Mr Cool"  , 35663, 35732, 12312, 75685},
        {"Bruce"    , 26874, 66677, 24546, 36483, 46768, 24632, 24527},
        {"John Cena", 25446, 83356, 65536, 23783, 24567, 89337}
    }



