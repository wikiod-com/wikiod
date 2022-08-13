---
title: "ReadOnlyCollections"
slug: "readonlycollections"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Une `ReadOnlyCollection` fournit une vue en lecture seule à une collection existante (la 'collection source').

Les éléments ne sont pas directement ajoutés ou supprimés d'une `ReadOnlyCollection`. Au lieu de cela, ils sont ajoutés et supprimés de la collection source et `ReadOnlyCollection` reflétera ces modifications dans la source.

Le nombre et l'ordre des éléments à l'intérieur d'une `ReadOnlyCollection` ne peuvent pas être modifiés, mais les propriétés des éléments peuvent l'être et les méthodes peuvent être appelées, en supposant qu'elles soient dans la portée.

Utilisez un `ReadOnlyCollection` lorsque vous souhaitez autoriser un code externe à afficher votre collection sans pouvoir la modifier, mais toujours en mesure de modifier la collection vous-même.

Voir également
* `Collection Observable<T>`
* `ReadOnlyObservableCollection<T>`


ReadOnlyCollections vs ImmutableCollection
------------------------------------------

Une `ReadOnlyCollection` diffère d'une `ImmutableCollection` en ce sens que vous ne pouvez pas modifier une `ImmutableCollection` une fois que vous l'avez créée - elle contiendra toujours `n` éléments, et ils ne peuvent pas être remplacés ou réorganisés. Une `ReadOnlyCollection`, en revanche, ne peut pas être modifiée directement, mais des éléments peuvent toujours être ajoutés/supprimés/réorganisés à l'aide de la collection source.


## Création d'une ReadOnlyCollection
Utilisation du constructeur
-----------
Une `ReadOnlyCollection` est créée en passant un objet `IList` existant dans le constructeur :

    var groceryList = new List<string> { "Apple", "Banana" };
    var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);


Utilisation de LINQ
-----------
De plus, LINQ fournit une méthode d'extension `AsReadOnly()` pour les objets `IList` :

    var readOnlyVersion = groceryList.AsReadOnly();

Noter
----
En règle générale, vous souhaitez conserver la collection source de manière privée et autoriser l'accès public à la `ReadOnlyCollection`. Bien que vous puissiez créer une `ReadOnlyCollection` à partir d'une liste en ligne, vous ne pourrez pas modifier la collection après l'avoir créée.

    var readOnlyGroceryList = new List<string> {"Apple", "Banana"}.AsReadOnly();
    // Great, but you will not be able to update the grocery list because 
    //  you do not have a reference to the source list anymore!

Si vous vous retrouvez à faire cela, vous voudrez peut-être envisager d'utiliser une autre structure de données, telle qu'une "ImmutableCollection".

## Mise à jour d'une ReadOnlyCollection
Une `ReadOnlyCollection` ne peut pas être modifiée directement. Au lieu de cela, la collection source est mise à jour et `ReadOnlyCollection` reflétera ces modifications. C'est la caractéristique clé de la `ReadOnlyCollection`.

    var groceryList = new List<string> { "Apple", "Banana" };

    var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);

    var itemCount = readOnlyGroceryList.Count;  // There are currently 2 items

    //readOnlyGroceryList.Add("Candy");         // Compiler Error - Items cannot be added to a ReadOnlyCollection object
    groceryList.Add("Vitamins");                // ..but they can be added to the original collection

    itemCount = readOnlyGroceryList.Count;      // Now there are 3 items
    var lastItem = readOnlyGroceryList.Last();  // The last item on the read only list is now "Vitamins"

[Voir la démo][1]


[1] : https://dotnetfiddle.net/C8qQrS

## Avertissement : les éléments d'une ReadOnlyCollection ne sont pas intrinsèquement en lecture seule
Si la collection source est d'un type qui n'est pas immuable, les éléments accessibles via une `ReadOnlyCollection` peuvent être modifiés.

    public class Item
    {
        public string Name { get; set; }
        public decimal Price { get; set; }
    }

    public static void FillOrder()
    {
        // An order is generated
        var order = new List<Item>
        {
            new Item { Name = "Apple", Price = 0.50m },
            new Item { Name = "Banana", Price = 0.75m },
            new Item { Name = "Vitamins", Price = 5.50m }
        };

        // The current sub total is $6.75
        var subTotal = order.Sum(item => item.Price);

        // Let the customer preview their order
        var customerPreview = new ReadOnlyCollection<Item>(order);

        // The customer can't add or remove items, but they can change 
        //   the price of an item, even though it is a ReadOnlyCollection
        customerPreview.Last().Price = 0.25m;

        // The sub total is now only $1.50!
        subTotal = order.Sum(item => item.Price);
    }


[Voir la démo][1]


[1] : https://dotnetfiddle.net/fXE66F

