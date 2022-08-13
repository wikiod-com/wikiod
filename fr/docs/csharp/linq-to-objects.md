---
title: "Liaison aux objets"
slug: "liaison-aux-objets"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

LINQ to Objects fait référence à l'utilisation de requêtes LINQ avec n'importe quelle collection IEnumerable.

## Utilisation de LINQ aux objets en C#
**Une simple requête SELECT dans Linq**

    static void Main(string[] args)
    {
        string[] cars = { "VW Golf", 
                            "Opel Astra", 
                            "Audi A4", 
                            "Ford Focus", 
                            "Seat Leon", 
                            "VW Passat", 
                            "VW Polo", 
                            "Mercedes C-Class" };

        var list = from car in cars
                   select car;

        StringBuilder sb = new StringBuilder();

        foreach (string entry in list)
        {
            sb.Append(entry + "\n");
        }

        Console.WriteLine(sb.ToString());
        Console.ReadLine();
    }

Dans l'exemple ci-dessus, un tableau de chaînes (voitures) est utilisé comme une collection d'objets à interroger à l'aide de LINQ. Dans une requête LINQ, la clause from vient en premier afin d'introduire la source de données (voitures) et la variable de plage (voiture). Lorsque la requête est exécutée, la variable range servira de référence à chaque élément successif dans cars. Étant donné que le compilateur peut déduire le type de voiture, vous n'avez pas à le spécifier explicitement

Lorsque le code ci-dessus est compilé et exécuté, il produit le résultat suivant :
[![entrez la description de l'image ici][1]][1]

**SELECT avec une clause WHERE**

    var list = from car in cars
               where car.Contains("VW")
               select car;

La clause WHERE est utilisée pour interroger le tableau de chaînes (voitures) afin de trouver et de renvoyer un sous-ensemble de tableau qui satisfait la clause WHERE.

Lorsque le code ci-dessus est compilé et exécuté, il produit le résultat suivant :

[![entrez la description de l'image ici][2]][2]


**Génération d'une liste ordonnée**

    var list = from car in cars
               orderby car ascending 
               select car;

Il est parfois utile de trier les données renvoyées. La clause orderby entraînera le tri des éléments en fonction du comparateur par défaut pour le type trié.

Lorsque le code ci-dessus est compilé et exécuté, il produit le résultat suivant :

[![entrez la description de l'image ici][3]][3]


**Travailler avec un type personnalisé**

Dans cet exemple, une liste typée est créée, remplie, puis interrogée

    public class Car
    {
        public String Name { get; private set; }
        public int UnitsSold { get; private set; }

        public Car(string name, int unitsSold)
        {
            Name = name;
            UnitsSold = unitsSold;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {

            var car1 = new Car("VW Golf", 270952);
            var car2 = new Car("Opel Astra", 56079);
            var car3 = new Car("Audi A4", 52493);
            var car4 = new Car("Ford Focus", 51677);
            var car5 = new Car("Seat Leon", 42125);
            var car6 = new Car("VW Passat", 97586);
            var car7 = new Car("VW Polo", 69867);
            var car8 = new Car("Mercedes C-Class", 67549);

            var cars = new List<Car> { 
                car1, car2, car3, car4, car5, car6, car7, car8 };
            var list = from car in cars
                       select car.Name;

            foreach (var entry in list)
            {
                Console.WriteLine(entry);
            }
            Console.ReadLine();
        }
    }

Lorsque le code ci-dessus est compilé et exécuté, il produit le résultat suivant :

[![entrez la description de l'image ici][4]][4]


Jusqu'à présent, les exemples ne semblent pas étonnants car il suffit de parcourir le tableau pour faire essentiellement la même chose. Cependant, avec les quelques exemples ci-dessous, vous pouvez voir comment créer des requêtes plus complexes avec LINQ to Objects et obtenir plus avec beaucoup moins de code.

Dans l'exemple ci-dessous, nous pouvons sélectionner des voitures qui ont été vendues à plus de 60 000 unités et les trier en fonction du nombre d'unités vendues :

    var list = from car in cars
               where car.UnitsSold > 60000 
               orderby car.UnitsSold descending 
               select car;

    StringBuilder sb = new StringBuilder();

    foreach (var entry in list)
    {
        sb.AppendLine($"{entry.Name} - {entry.UnitsSold}");
    }
    Console.WriteLine(sb.ToString());

Lorsque le code ci-dessus est compilé et exécuté, il produit le résultat suivant :
[![entrez la description de l'image ici][5]][5]


Dans l'exemple ci-dessous, nous pouvons sélectionner les voitures qui ont vendu un nombre impair d'unités et les classer par ordre alphabétique sur son nom :

    var list = from car in cars
               where car.UnitsSold % 2 != 0 
               orderby car.Name ascending 
               select car;

Lorsque le code ci-dessus est compilé et exécuté, il produit le résultat suivant :
[![entrez la description de l'image ici][6]][6]


[1] : https://i.stack.imgur.com/lG65Q.png
[2] : https://i.stack.imgur.com/llGXx.png
[3] : https://i.stack.imgur.com/ODH55.png
[4] : https://i.stack.imgur.com/0jUOC.png
[5] : https://i.stack.imgur.com/ZDeTt.png
[6] : https://i.stack.imgur.com/fJnTp.png

## Comment LINQ to Object exécute les requêtes
Les requêtes LINQ ne s'exécutent pas immédiatement. Lorsque vous créez la requête, vous stockez simplement la requête pour une exécution future. Ce n'est que lorsque vous demandez réellement d'itérer la requête que la requête est exécutée (par exemple, dans une boucle for, lors de l'appel de ToList, Count, Max, Average, First, etc.)

Ceci est considéré comme une *exécution différée*. Cela vous permet de créer la requête en plusieurs étapes, en la modifiant éventuellement en fonction d'instructions conditionnelles, puis de l'exécuter ultérieurement uniquement lorsque vous avez besoin du résultat.

Étant donné le code:

    var query = from n in numbers 
                where n % 2 != 0
                select n;

L'exemple ci-dessus stocke uniquement la requête dans la variable `query`. Il n'exécute pas la requête elle-même.

L'instruction `foreach` force l'exécution de la requête :

    foreach(var n in query) {
        Console.WriteLine($"Number selected {n}");
    }

Certaines méthodes LINQ déclencheront également l'exécution de la requête, `Count`, `First`, `Max`, `Average`. Ils renvoient des valeurs uniques. `ToList` et `ToArray` collectent le résultat et les transforment respectivement en une liste ou un tableau.

Sachez qu'il vous est possible de parcourir la requête plusieurs fois si vous appelez plusieurs fonctions LINQ sur la même requête. Cela pourrait vous donner des résultats différents à chaque appel. Si vous ne souhaitez travailler qu'avec un seul ensemble de données, veillez à l'enregistrer dans une liste ou un tableau.



