---
title: "IEnumerable"
slug: "ienumerable"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

`IEnumerable` est l'interface de base pour toutes les collections non génériques comme ArrayList qui peuvent être énumérées. `IEnumerator<T>` est l'interface de base pour tous les énumérateurs génériques comme List<>.

`IEnumerable` est une interface qui implémente la méthode `GetEnumerator`. La méthode `GetEnumerator` renvoie un `IEnumerator` qui fournit des options pour parcourir la collection comme foreach.

IEnumerable est l'interface de base pour toutes les collections non génériques qui peuvent être énumérées

## IEnumerable avec un énumérateur personnalisé
L'implémentation de l'interface IEnumerable permet d'énumérer les classes de la même manière que les collections BCL. Cela nécessite d'étendre la classe Enumerator qui suit l'état de l'énumération.

Outre l'itération sur une collection standard, les exemples incluent :
- Utilisation de plages de nombres basées sur une fonction plutôt que sur une collection d'objets
- Implémentation de différents algorithmes d'itération sur des collections, comme DFS ou BFS sur une collection de graphes


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
Dans sa forme la plus élémentaire, un objet qui implémente IEnumerable<T> représente une série d'objets. Les objets en question peuvent être itérés à l'aide du mot-clé c# `foreach`.

Dans l'exemple ci-dessous, l'objet `sequenceOfNumbers` implémente IEnumerable<int>. Il représente une suite d'entiers. La boucle `foreach` parcourt chacun à son tour.

    int AddNumbers(IEnumerable<int> sequenceOfNumbers) {
        int returnValue = 0;
        foreach(int i in sequenceOfNumbers) {
            returnValue += i;
        }
        return returnValue;
    }

