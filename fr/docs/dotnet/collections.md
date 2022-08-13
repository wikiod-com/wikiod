---
title: "Collections"
slug: "collections"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Il existe plusieurs types de collecte :

- 'Tableau'
- 'Liste'
- 'File d'attente'
- `Liste triée`
- 'Empiler'
- [Dictionnaire][1]


[1] : https://www.wikiod.com/fr/dotnet/dictionnaires

## Utilisation des initialiseurs de collection
Certains types de collection peuvent être initialisés au moment de la déclaration. Par exemple, l'instruction suivante crée et initialise les "nombres" avec des entiers :

    List<int> numbers = new List<int>(){10, 9, 8, 7, 7, 6, 5, 10, 4, 3, 2, 1};

En interne, le compilateur C# convertit en fait cette initialisation en une série d'appels à la méthode Add. Par conséquent, vous ne pouvez utiliser cette syntaxe que pour les collections qui prennent en charge la méthode `Add`.

>Les classes `Stack<T>` et `Queue<T>` ne le supportent pas.

Pour les collections complexes telles que la classe `Dictionary<TKey, TValue>`, qui acceptent des paires clé/valeur, vous pouvez spécifier chaque paire clé/valeur en tant que type anonyme dans la liste d'initialisation.

    Dictionary<int, string> employee = new Dictionary<int, string>()
         {{44, "John"}, {45, "Bob"}, {47, "James"}, {48, "Franklin"}};
Le premier élément de chaque paire est la clé et le second est la valeur.

## Création d'une liste initialisée avec des types personnalisés
    public class Model
    {
        public string Name { get; set; }
        public bool? Selected { get; set; }
    }

Ici, nous avons une classe sans constructeur avec deux propriétés : `Name` et une propriété booléenne nullable `Selected`. Si nous voulions initialiser un `List<Model>`, il existe plusieurs façons de l'exécuter.

    var SelectedEmployees = new List<Model>
     {
          new Model() {Name = "Item1", Selected = true},
          new Model() {Name = "Item2", Selected = false},
          new Model() {Name = "Item3", Selected = false},
          new Model() {Name = "Item4"}
     };

Ici, nous créons plusieurs "nouvelles" instances de notre classe "Model" et les initialisons avec des données. Et si on ajoutait un constructeur ?

    public class Model
    {
    
        public Model(string name, bool? selected = false)
        {
            Name = name;
            selected = Selected;
        }
        public string Name { get; set; }
        public bool? Selected { get; set; }
    }

Cela nous permet d'initialiser notre List un *peu* différemment.

    var SelectedEmployees = new List<Model>
    {
        new Model("Mark", true),
        new Model("Alexis"),
        new Model("")
    };

Qu'en est-il d'une classe dont l'une des propriétés est une classe elle-même ?

    public class Model
    {
        public string Name { get; set; }
        public bool? Selected { get; set; }
    }
    
    public class ExtendedModel : Model
    {
        public ExtendedModel()
        {
            BaseModel = new Model();
        }
    
        public Model BaseModel { get; set; }
        public DateTime BirthDate { get; set; }
    }

Remarquez que nous avons inversé le constructeur de la classe `Model` pour simplifier un peu l'exemple.

    var SelectedWithBirthDate = new List<ExtendedModel>
    {
        new ExtendedModel()
        {
            BaseModel = new Model { Name = "Mark", Selected = true},
            BirthDate = new DateTime(2015, 11, 23)
        },
                        new ExtendedModel()
        {
            BaseModel = new Model { Name = "Random"},
            BirthDate = new DateTime(2015, 11, 23)
        }
    };

Notez que nous pouvons échanger notre `List<ExtendedModel>` avec `Collection<ExtendedModel>`, `ExtendedModel[]`, `object[]`, ou même simplement `[]`.

## File d'attente
Il existe une collection dans .Net utilisée pour gérer les valeurs dans une [`Queue`][1] qui utilise le concept [FIFO (first-in first-out)][2]. La base des files d'attente est la méthode [`Enqueue(T item)`][3] qui est utilisée pour ajouter des éléments dans la file d'attente et [`Dequeue()`][4] qui est utilisée pour obtenir le premier élément et le supprimer de la file d'attente. La version générique peut être utilisée comme le code suivant pour une file d'attente de chaînes.

Tout d'abord, ajoutez l'espace de noms :

    using System.Collections.Generic;

et l'utiliser:

    Queue<string> queue = new Queue<string>();
    queue.Enqueue("John");
    queue.Enqueue("Paul");
    queue.Enqueue("George");
    queue.Enqueue("Ringo");
    
    string dequeueValue;
    dequeueValue = queue.Dequeue(); // return John
    dequeueValue = queue.Dequeue(); // return Paul
    dequeueValue = queue.Dequeue(); // return George
    dequeueValue = queue.Dequeue(); // return Ringo

Il existe une version non générique du type, qui fonctionne avec des objets.

L'espace de noms est :

    using System.Collections;

Ajoutez un exemple de code pour une file d'attente non générique :

    Queue queue = new Queue();
    queue.Enqueue("Hello World"); // string
    queue.Enqueue(5); // int
    queue.Enqueue(1d); // double
    queue.Enqueue(true); // bool
    queue.Enqueue(new Product()); // Product object
    
    object dequeueValue;
    dequeueValue = queue.Dequeue(); // return Hello World (string)
    dequeueValue = queue.Dequeue(); // return 5 (int)
    dequeueValue = queue.Dequeue(); // return 1d (double)
    dequeueValue = queue.Dequeue(); // return true (bool)
    dequeueValue = queue.Dequeue(); // return Product (Product type)

Il existe également une méthode appelée [Peek()][5] qui renvoie l'objet au début de la file d'attente sans lui supprimer les éléments.

    Queue<int> queue = new Queue<int>();
    queue.Enqueue(10);
    queue.Enqueue(20);
    queue.Enqueue(30);
    queue.Enqueue(40);
    queue.Enqueue(50);

    foreach (int element in queue)
    {
        Console.WriteLine(i);
    }
    
La sortie (sans supprimer):

    10
    20
    30
    40
    50

[1] : https://msdn.microsoft.com/library/system.collections.queue(v=vs.110).aspx
[2] : https://en.wikipedia.org/wiki/FIFO_(informatique_et_électronique)
[3] : https://msdn.microsoft.com/library/t249c2y7(v=vs.110).aspx
[4] : https://msdn.microsoft.com/library/1c8bzx97(v=vs.110).aspx
[5] : https://msdn.microsoft.com/library/system.collections.queue.peek(v=vs.110).aspx

## Empiler
Il existe une collection dans .Net utilisée pour gérer les valeurs dans un [`Stack`][1] qui utilise le concept [LIFO (dernier entré, premier sorti)][2]. La base des piles est la méthode [`Push(T item)`][3] qui est utilisée pour ajouter des éléments dans la pile et [`Pop()`][4] qui est utilisée pour obtenir le dernier élément ajouté et supprimer de la pile. La version générique peut être utilisée comme le code suivant pour une file d'attente de chaînes.

Tout d'abord, ajoutez l'espace de noms :

    using System.Collections.Generic;

et l'utiliser:

    Stack<string> stack = new Stack<string>();
    stack.Push("John");
    stack.Push("Paul");
    stack.Push("George");
    stack.Push("Ringo");
    
    string value;
    value = stack.Pop(); // return Ringo
    value = stack.Pop(); // return George
    value = stack.Pop(); // return Paul
    value = stack.Pop(); // return John

Il existe une version non générique du type, qui fonctionne avec des objets.

L'espace de noms est :

    using System.Collections;

Et un exemple de code de pile non générique :

    Stack stack = new Stack();
    stack.Push("Hello World"); // string
    stack.Push(5); // int
    stack.Push(1d); // double
    stack.Push(true); // bool
    stack.Push(new Product()); // Product object
    
    object value;
    value = stack.Pop(); // return Product (Product type)
    value = stack.Pop(); // return true (bool)
    value = stack.Pop(); // return 1d (double)
    value = stack.Pop(); // return 5 (int)
    value = stack.Pop(); // return Hello World (string)

Il existe également une méthode appelée [Peek()][5] qui renvoie le dernier élément ajouté mais sans le supprimer de la `Stack`.

    Stack<int> stack = new Stack<int>();
    stack.Push(10);
    stack.Push(20);
    
    var lastValueAdded = stack.Peek(); // 20

Il est possible d'itérer sur les éléments de la pile et cela respectera l'ordre de la pile (LIFO).

    Stack<int> stack = new Stack<int>();
    stack.Push(10);
    stack.Push(20);
    stack.Push(30);
    stack.Push(40);
    stack.Push(50);

    foreach (int element in stack)
    {
       Console.WriteLine(element);
    }
    
La sortie (sans supprimer):

    50
    40
    30
    20
    10
    


[1] : https://msdn.microsoft.com/library/system.collections.stack(v=vs.110).aspx
[2] : https://en.wikipedia.org/wiki/Stack_(abstract_data_type)
[3] : https://msdn.microsoft.com/library/system.collections.stack.push(v=vs.110).aspx
[4] : https://msdn.microsoft.com/library/system.collections.stack.pop(v=vs.110).aspx
[5] : https://msdn.microsoft.com/library/system.collections.stack.peek(v=vs.110).aspx

