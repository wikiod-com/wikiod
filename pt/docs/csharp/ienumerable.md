---
title: "IEnumerable"
slug: "ienumerable"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

`IEnumerable` é a interface base para todas as coleções não genéricas como ArrayList que podem ser enumeradas. `IEnumerator<T>` é a interface base para todos os enumeradores genéricos como List<>.

`IEnumerable` é uma interface que implementa o método `GetEnumerator`. O método `GetEnumerator` retorna um `IEnumerator` que fornece opções para iterar pela coleção como foreach.

IEnumerable é a interface base para todas as coleções não genéricas que podem ser enumeradas

## IEnumerable com enumerador personalizado
A implementação da interface IEnumerable permite que as classes sejam enumeradas da mesma forma que as coleções BCL. Isso requer estender a classe Enumerator que rastreia o estado da enumeração.

Além de iterar sobre uma coleção padrão, os exemplos incluem:
- Usando intervalos de números com base em uma função em vez de uma coleção de objetos
- Implementação de diferentes algoritmos de iteração sobre coleções, como DFS ou BFS em uma coleção de gráficos


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
Em sua forma mais básica, um objeto que implementa IEnumerable<T> representa uma série de objetos. Os objetos em questão podem ser iterados usando a palavra-chave `foreach` do c#.

No exemplo abaixo, o objeto `sequenceOfNumbers` implementa IEnumerable<int>. Representa uma série de números inteiros. O loop `foreach` percorre cada um por sua vez.

    int AddNumbers(IEnumerable<int> sequenceOfNumbers) {
        int returnValue = 0;
        foreach(int i in sequenceOfNumbers) {
            returnValue += i;
        }
        return returnValue;
    }

