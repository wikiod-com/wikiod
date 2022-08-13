---
title: "IEnumerable"
slug: "ienumerable"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

`IEnumerable` es la interfaz base para todas las colecciones no genéricas como ArrayList que se pueden enumerar. `IEnumerator<T>` es la interfaz base para todos los enumeradores genéricos como List<>.

`IEnumerable` es una interfaz que implementa el método `GetEnumerator`. El método `GetEnumerator` devuelve un `IEnumerator` que proporciona opciones para iterar a través de la colección como foreach.

IEnumerable es la interfaz base para todas las colecciones no genéricas que se pueden enumerar

## IEnumerable con enumerador personalizado
La implementación de la interfaz IEnumerable permite que las clases se enumeren de la misma manera que las colecciones BCL. Esto requiere extender la clase Enumerator que rastrea el estado de la enumeración.

Además de iterar sobre una colección estándar, los ejemplos incluyen:
- Usar rangos de números basados ​​en una función en lugar de una colección de objetos
- Implementar diferentes algoritmos de iteración sobre colecciones, como DFS o BFS en una colección de gráficos


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

## IEnumerable<int>
En su forma más básica, un objeto que implementa IEnumerable<T> representa una serie de objetos. Los objetos en cuestión se pueden iterar usando la palabra clave `foreach` de C#.

En el siguiente ejemplo, el objeto `sequenceOfNumbers` implementa IEnumerable<int>. Representa una serie de números enteros. El ciclo `foreach` itera a través de cada uno a su vez.

    int AddNumbers(IEnumerable<int> sequenceOfNumbers) {
        int returnValue = 0;
        foreach(int i in sequenceOfNumbers) {
            returnValue += i;
        }
        return returnValue;
    }

