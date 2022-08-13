---
title: "Indexeur"
slug: "indexeur"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Syntaxe
- public ReturnType this[IndexType index] { get { ... } set { ... }}

Indexer permet à la syntaxe de type tableau d'accéder à une propriété d'un objet avec un index.

- Peut être utilisé sur une classe, une structure ou une interface.
- Peut être surchargé.
- Peut utiliser plusieurs paramètres.
- Peut être utilisé pour accéder et définir des valeurs.
- Peut utiliser n'importe quel type pour son index.

## Un simple indexeur
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

**Usage:**

        var foo = new Foo();

        // access a value    
        string berlin = foo[2];

        // assign a value
        foo[0] = "Rome";

[Voir la démo][1]


[1] : https://dotnetfiddle.net/I1usLs

## Surcharge de l'indexeur pour créer un SparseArray


En surchargeant l'indexeur, vous pouvez créer une classe qui ressemble à un tableau mais qui ne l'est pas. Il aura O(1) méthodes get et set, pourra accéder à un élément à l'index 100, et aura toujours la taille des éléments à l'intérieur de celui-ci. La classe SparseArray

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



## Indexeur avec 2 arguments et interface
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

