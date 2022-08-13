---
title: "dizin oluşturucu"
slug: "dizin-olusturucu"
draft: false
images: []
weight: 9949
type: docs
toc: true
---

## Sözdizimi
- public ReturnType this[IndexType index] { get { ... } set { ... }}

Dizin oluşturucu, dizi benzeri sözdiziminin bir dizine sahip bir nesnenin özelliğine erişmesine izin verir.

- Bir sınıf, yapı veya arayüz üzerinde kullanılabilir.
- Aşırı yüklenebilir.
- Birden fazla parametre kullanabilir.
- Değerlere erişmek ve ayarlamak için kullanılabilir.
- Dizini için herhangi bir türü kullanabilir.

## Basit bir dizin oluşturucu
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

**Kullanım:**

        var foo = new Foo();

        // access a value    
        string berlin = foo[2];

        // assign a value
        foo[0] = "Rome";

[Demoyu Görüntüle][1]


[1]: https://dotnetfiddle.net/I1usLs

## SparseArray oluşturmak için dizin oluşturucuyu aşırı yükleme


Dizin oluşturucuyu aşırı yükleyerek, bir dizi gibi görünen ve hissettiren ancak olmayan bir sınıf oluşturabilirsiniz. O(1) get ve set yöntemlerine sahip olacak, 100 dizinindeki bir öğeye erişebilecek ve yine de içindeki öğelerin boyutuna sahip olacak. SparseArray sınıfı

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



## 2 bağımsız değişkenli ve arabirimli dizin oluşturucu
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

