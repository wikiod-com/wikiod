---
title: "Indexador"
slug: "indexador"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Sintaxe
- public ReturnType this[IndexType index] { get { ... } set { ... }}

O indexador permite que a sintaxe do tipo array acesse uma propriedade de um objeto com um índice.

- Pode ser usado em uma classe, estrutura ou interface.
- Pode ser sobrecarregado.
- Pode usar vários parâmetros.
- Pode ser usado para acessar e definir valores.
- Pode usar qualquer tipo para seu índice.

## Um indexador simples
    class Foo
    {
        private string[] cities = new[] { "Paris", "London", "Berlin" };

        public string this[int index]
        {
            get {
                return cities[index];
            }
            set {
                cities[index] = value;
            }
        }
    }


----------

**Uso:**

        var foo = new Foo();

        // access a value    
        string berlin = foo[2];

        // assign a value
        foo[0] = "Rome";

[Ver demonstração][1]


[1]: https://dotnetfiddle.net/I1usLs

## Sobrecarregar o indexador para criar um SparseArray


Ao sobrecarregar o indexador, você pode criar uma classe que parece um array, mas não é. Ele terá métodos get e set O(1), pode acessar um elemento no índice 100 e ainda ter o tamanho dos elementos dentro dele. A classe SparseArray

    class SparseArray
        {
            Dictionary<int, string> array = new Dictionary<int, string>();
    
            public string this[int i]
            {
                get
                {
                    if(!array.ContainsKey(i))
                    {
                        return null;
                    }
                    return array[i];
                }
                set
                {
                    if(!array.ContainsKey(i))
                        array.Add(i, value);
                }
            }
        }



## Indexador com 2 argumentos e interface
    interface ITable { 
        // an indexer can be declared in an interface
        object this[int x, int y] { get; set; }
    }

    class DataTable : ITable
    {
        private object[,] cells = new object[10, 10];

        /// <summary>
        /// implementation of the indexer declared in the interface
        /// </summary>
        /// <param name="x">X-Index</param>
        /// <param name="y">Y-Index</param>
        /// <returns>Content of this cell</returns>
        public object this[int x, int y]
        {
            get
            {
                return cells[x, y];
            }
            set
            {
                cells[x, y] = value;
            }
        }
    }

