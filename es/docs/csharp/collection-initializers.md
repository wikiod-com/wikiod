---
title: "Inicializadores de colección"
slug: "inicializadores-de-coleccion"
draft: false
images: []
weight: 9759
type: docs
toc: true
---

El único requisito para que un objeto se inicialice usando este azúcar sintáctico es que el tipo implemente `System.Collections.IEnumerable` y el método `Add`. Aunque lo llamamos un inicializador de colección, el objeto *no* tiene que ser una colección.

## Inicializadores de colección
Inicialice un tipo de colección con valores:

    var stringList = new List<string>
    {
        "foo",
        "bar",
    };

Los inicializadores de colección son azúcar sintáctico para las llamadas `Add()`. El código anterior es equivalente a:

    var temp = new List<string>();
    temp.Add("foo");
    temp.Add("bar");
    var stringList = temp;

Tenga en cuenta que la inicialización se realiza atómicamente utilizando una variable temporal, para evitar condiciones de carrera.

Para los tipos que ofrecen múltiples parámetros en su método `Add()`, encierre los argumentos separados por comas entre llaves:

    var numberDictionary = new Dictionary<int, string>
    {
        { 1, "One" },
        { 2, "Two" },
    };

Esto es equivalente a:

    var temp = new Dictionary<int, string>();
    temp.Add(1, "One");
    temp.Add(2, "Two");
    var numberDictionarynumberDictionary = temp;


## C# 6 Inicializadores de índice
A partir de C# 6, las colecciones con indexadores se pueden inicializar especificando el índice que se asignará entre corchetes, seguido de un signo igual y del valor que se asignará.

# Inicialización del diccionario

Un ejemplo de esta sintaxis usando un Diccionario:

    var dict = new Dictionary<string, int>
    {
        ["key1"] = 1,
        ["key2"] = 50
    };

Esto es equivalente a:

    var dict = new Dictionary<string, int>();
    dict["key1"] = 1;
    dict["key2"] = 50

La sintaxis del inicializador de colección para hacer esto antes de C# 6 era:

    var dict = new Dictionary<string, int>
    {
        { "key1", 1 },
        { "key2", 50 }
    };
    
Lo cual correspondería a:

    var dict = new Dictionary<string, int>();
    dict.Add("key1", 1);
    dict.Add("key2", 50);


Así que hay una diferencia significativa en la funcionalidad, ya que la nueva sintaxis usa el *indexador* del objeto inicializado para asignar valores en lugar de usar su método `Add()`. Esto significa que la nueva sintaxis solo requiere un indexador disponible públicamente y funciona para cualquier objeto que tenga uno.

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

Esto daría como resultado:

> `10 fue asignado al índice 0`<br/>
> `20 fue asignado al índice 1`



## Inicializadores de colección en clases personalizadas
Para crear inicializadores de colección de soporte de clase, debe implementar la interfaz `IEnumerable` y tener al menos un método `Add`. Desde C# 6, cualquier colección que implemente `IEnumerable` se puede ampliar con métodos `Add` personalizados utilizando métodos de extensión.

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



## Usando el inicializador de colección dentro del inicializador de objeto
    public class Tag
    {
        public IList<string> Synonyms { get; set; }
    }

`Synonyms` es una propiedad de tipo colección. Cuando el objeto 'Etiqueta' se crea utilizando la sintaxis de inicialización de objetos, 'Sinónimos' también se puede inicializar con la sintaxis de inicialización de colección:

    Tag t = new Tag 
    {
        Synonyms = new List<string> {"c#", "c-sharp"}
    };

La propiedad de la colección puede ser de solo lectura y aún admite la sintaxis del inicializador de la colección. Considere este ejemplo modificado (la propiedad `Synonyms` ahora tiene un setter privado):

    public class Tag
    {
        public Tag()
        {
            Synonyms = new List<string>();
        }
        
        public IList<string> Synonyms { get; private set; }
    }

Un nuevo objeto `Etiqueta` se puede crear así:

    Tag t = new Tag 
    {
        Synonyms = {"c#", "c-sharp"}
    };

Esto funciona porque los inicializadores de la colección son simplemente azúcar sintático sobre las llamadas a `Add()`. No se está creando una nueva lista aquí, el compilador solo está generando llamadas a `Add()` en el objeto existente.

## Inicializadores de colección con matrices de parámetros
Puede mezclar parámetros normales y matrices de parámetros:

    public class LotteryTicket : IEnumerable{
        public int[] LuckyNumbers;
        public string UserName;

        public void Add(string userName, params int[] luckyNumbers){
            UserName = userName;
            Lottery = luckyNumbers;
        }
    }

Esta sintaxis ahora es posible:

    var Tickets = new List<LotteryTicket>{
        {"Mr Cool"  , 35663, 35732, 12312, 75685},
        {"Bruce"    , 26874, 66677, 24546, 36483, 46768, 24632, 24527},
        {"John Cena", 25446, 83356, 65536, 23783, 24567, 89337}
    }



