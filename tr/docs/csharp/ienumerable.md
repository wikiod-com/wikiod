---
title: "Numaralandırılabilir"
slug: "numaralandrlabilir"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

"IEnumerable", sıralanabilen ArrayList gibi tüm genel olmayan koleksiyonlar için temel arabirimdir. 'IEnumerator<T>', List<> gibi tüm genel numaralandırıcılar için temel arabirimdir.

"IEnumerable", "GetEnumerator" yöntemini uygulayan bir arabirimdir. "GetEnumerator" yöntemi, foreach gibi koleksiyonda yineleme seçenekleri sağlayan bir "IEnumerator" döndürür.

IEnumerable, numaralandırılabilen tüm genel olmayan koleksiyonlar için temel arabirimdir.

## Özel Numaralandırıcı ile IEnumerable
IEnumerable arabiriminin uygulanması, sınıfların BCL koleksiyonlarıyla aynı şekilde numaralandırılmasına olanak tanır. Bu, numaralandırmanın durumunu izleyen Enumerator sınıfının genişletilmesini gerektirir.

Standart bir koleksiyon üzerinde yineleme yapmaktan başka örnekler şunları içerir:
- Bir nesne koleksiyonundan ziyade bir fonksiyona dayalı sayı aralıklarını kullanma
- Bir grafik koleksiyonunda DFS veya BFS gibi koleksiyonlar üzerinde farklı yineleme algoritmaları uygulamak


    public static void Main(string[] args) {
    
        foreach (var coffee in new CoffeeCollection()) {
            Console.WriteLine(coffee);
        }
    }

    public class CoffeeCollection : IEnumerable {
        private CoffeeEnumerator enumerator;

        public CoffeeCollection() {
            enumerator = new CoffeeEnumerator();
        }

        public IEnumerator GetEnumerator() {
            return enumerator;
        }

        public class CoffeeEnumerator : IEnumerator {
            string[] beverages = new string[3] { "espresso", "macchiato", "latte" };
            int currentIndex = -1;

            public object Current {
                get {
                    return beverages[currentIndex];
                }
            }

            public bool MoveNext() {
                currentIndex++;

                if (currentIndex < beverages.Length) {
                    return true;
                }

                return false;
            }

            public void Reset() {
                currentIndex = 0;
            }
        }
    }

## ISayılanabilir<int>
En temel biçiminde, IEnumerable<T> uygulayan bir nesne, bir dizi nesneyi temsil eder. Söz konusu nesneler, c# `foreach` anahtar sözcüğü kullanılarak yinelenebilir.

Aşağıdaki örnekte, "sequenceOfNumbers" nesnesi IEnumerable<int> öğesini uygular. Bir dizi tamsayıyı temsil eder. 'foreach' döngüsü sırayla her birini yineler.

    int AddNumbers(IEnumerable<int> sequenceOfNumbers) {
        int returnValue = 0;
        foreach(int i in sequenceOfNumbers) {
            returnValue += i;
        }
        return returnValue;
    }

