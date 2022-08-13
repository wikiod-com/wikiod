---
title: "Initialiseurs de collection"
slug: "initialiseurs-de-collection"
draft: false
images: []
weight: 9759
type: docs
toc: true
---

La seule exigence pour qu'un objet soit initialisé à l'aide de ce sucre syntaxique est que le type implémente `System.Collections.IEnumerable` et la méthode `Add`. Bien que nous l'appelions un initialiseur de collection, l'objet n'a *pas* besoin d'être une collection.

## Initialiseurs de collection
Initialisez un type de collection avec des valeurs :

    var stringList = new List<string>
    {
        "foo",
        "bar",
    };

Les initialiseurs de collection sont du sucre syntaxique pour les appels `Add()`. Le code ci-dessus est équivalent à :

    var temp = new List<string>();
    temp.Add("foo");
    temp.Add("bar");
    var stringList = temp;

Notez que l'initialisation se fait de manière atomique à l'aide d'une variable temporaire, pour éviter les conditions de concurrence.

Pour les types qui offrent plusieurs paramètres dans leur méthode `Add()`, placez les arguments séparés par des virgules entre accolades :

    var numberDictionary = new Dictionary<int, string>
    {
        { 1, "One" },
        { 2, "Two" },
    };

Cela équivaut à :

    var temp = new Dictionary<int, string>();
    temp.Add(1, "One");
    temp.Add(2, "Two");
    var numberDictionarynumberDictionary = temp;


## C# 6 Initialiseurs d'index
À partir de C# 6, les collections avec indexeurs peuvent être initialisées en spécifiant l'index à attribuer entre crochets, suivi d'un signe égal, suivi de la valeur à attribuer.

# Initialisation du dictionnaire

Un exemple de cette syntaxe utilisant un dictionnaire :

    var dict = new Dictionary<string, int>
    {
        ["key1"] = 1,
        ["key2"] = 50
    };

Cela équivaut à :

    var dict = new Dictionary<string, int>();
    dict["key1"] = 1;
    dict["key2"] = 50

La syntaxe de l'initialiseur de collection pour ce faire avant C# 6 était :

    var dict = new Dictionary<string, int>
    {
        { "key1", 1 },
        { "key2", 50 }
    };
    
Ce qui correspondrait à :

    var dict = new Dictionary<string, int>();
    dict.Add("key1", 1);
    dict.Add("key2", 50);


Il existe donc une différence significative de fonctionnalité, car la nouvelle syntaxe utilise l'*indexeur* de l'objet initialisé pour attribuer des valeurs au lieu d'utiliser sa méthode `Add()`. Cela signifie que la nouvelle syntaxe ne nécessite qu'un indexeur accessible au public et fonctionne pour tout objet qui en possède un.

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

Cela produirait:

> `10 a été affecté à l'index 0`<br/>
> `20 a été affecté à l'index 1`



## Initialiseurs de collection dans les classes personnalisées
Pour qu'une classe prenne en charge les initialiseurs de collection, elle doit implémenter l'interface `IEnumerable` et avoir au moins une méthode `Add`. Depuis C # 6, toute collection implémentant `IEnumerable` peut être étendue avec des méthodes `Add` personnalisées à l'aide de méthodes d'extension.

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



## Utilisation de l'initialiseur de collection dans l'initialiseur d'objet
    public class Tag
    {
        public IList<string> Synonyms { get; set; }
    }

`Synonyms` est une propriété de type collection. Lorsque l'objet `Tag` est créé à l'aide de la syntaxe d'initialisation d'objet, `Synonyms` peut également être initialisé avec la syntaxe d'initialisation de collection :

    Tag t = new Tag 
    {
        Synonyms = new List<string> {"c#", "c-sharp"}
    };

La propriété de collection peut être en lecture seule et toujours prendre en charge la syntaxe d'initialisation de collection. Considérez cet exemple modifié (la propriété `Synonyms` a maintenant un setter privé):

    public class Tag
    {
        public Tag()
        {
            Synonyms = new List<string>();
        }
        
        public IList<string> Synonyms { get; private set; }
    }

Un nouvel objet `Tag` peut être créé comme ceci :

    Tag t = new Tag 
    {
        Synonyms = {"c#", "c-sharp"}
    };

Cela fonctionne car les initialiseurs de collection ne sont que du sucre syntaxique sur les appels à `Add()`. Aucune nouvelle liste n'est créée ici, le compilateur génère simplement des appels à `Add()` sur l'objet sortant.

## Initialiseurs de collection avec tableaux de paramètres
Vous pouvez mélanger des paramètres normaux et des tableaux de paramètres :

    public class LotteryTicket : IEnumerable{
        public int[] LuckyNumbers;
        public string UserName;

        public void Add(string userName, params int[] luckyNumbers){
            UserName = userName;
            Lottery = luckyNumbers;
        }
    }

Cette syntaxe est désormais possible :

    var Tickets = new List<LotteryTicket>{
        {"Mr Cool"  , 35663, 35732, 12312, 75685},
        {"Bruce"    , 26874, 66677, 24546, 36483, 46768, 24632, 24527},
        {"John Cena", 25446, 83356, 65536, 23783, 24567, 89337}
    }



