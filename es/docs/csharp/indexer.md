---
title: "indexador"
slug: "indexador"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Sintaxis
- public ReturnType this[IndexType index] { get { ... } set { ... }}

Indexer permite una sintaxis similar a una matriz para acceder a una propiedad de un objeto con un índice.

- Se puede usar en una clase, estructura o interfaz.
- Se puede sobrecargar.
- Puede utilizar múltiples parámetros.
- Se puede utilizar para acceder y establecer valores.
- Puede usar cualquier tipo para su índice.

## Un indexador simple
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

[Ver demostración][1]


[1]: https://dotnetfiddle.net/I1usLs

## Sobrecargar el indexador para crear un SparseArray


Al sobrecargar el indexador, puede crear una clase que se ve y se siente como una matriz, pero no lo es. Tendrá métodos get y set O(1), puede acceder a un elemento en el índice 100 y aún así tener el tamaño de los elementos dentro de él. La clase SparseArray

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



## Indexador con 2 argumentos e interfaz
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

