---
title: "Colecciones de solo lectura"
slug: "colecciones-de-solo-lectura"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Una `ReadOnlyCollection` proporciona una vista de solo lectura de una colección existente (la 'colección de origen').

Los elementos no se agregan ni eliminan directamente de una `ReadOnlyCollection`. En su lugar, se agregan y eliminan de la colección de origen y `ReadOnlyCollection` reflejará estos cambios en la fuente.

El número y el orden de los elementos dentro de una `ReadOnlyCollection` no se puede modificar, pero se pueden modificar las propiedades de los elementos y se pueden llamar a los métodos, suponiendo que estén dentro del alcance.

Utilice una `ReadOnlyCollection` cuando desee permitir que un código externo vea su colección sin poder modificarla, pero aún así poder modificar la colección usted mismo.

Ver también
* `ColecciónObservable<T>`
* `ReadOnlyObservableCollection<T>`


ReadOnlyCollections frente a ImmutableCollection
---------------------------------------------------------

Una `ReadOnlyCollection` se diferencia de una `ImmutableCollection` en que no puede editar una `ImmutableCollection` una vez que la creó; siempre contendrá `n` elementos, y no se pueden reemplazar ni reordenar. Una `ReadOnlyCollection`, por otro lado, no se puede editar directamente, pero los elementos aún se pueden agregar/eliminar/reordenar utilizando la colección de origen.


## Creación de una colección de solo lectura
Usando el constructor
-----------
Una `ReadOnlyCollection` se crea pasando un objeto `IList` existente al constructor:

    var groceryList = new List<string> { "Apple", "Banana" };
    var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);


Uso de LINQ
-----------
Además, LINQ proporciona un método de extensión `AsReadOnly()` para objetos `IList`:

    var readOnlyVersion = groceryList.AsReadOnly();

Nota
----
Por lo general, desea mantener la colección de origen de forma privada y permitir el acceso público a `ReadOnlyCollection`. Si bien podría crear una `ReadOnlyCollection` a partir de una lista en línea, no podría modificar la colección después de crearla.

    var readOnlyGroceryList = new List<string> {"Apple", "Banana"}.AsReadOnly();
    // Great, but you will not be able to update the grocery list because 
    //  you do not have a reference to the source list anymore!

Si se encuentra haciendo esto, es posible que desee considerar el uso de otra estructura de datos, como una `ImmutableCollection`.

## Actualización de una colección de solo lectura
Una `ReadOnlyCollection` no se puede editar directamente. En su lugar, la colección de origen se actualiza y `ReadOnlyCollection` reflejará estos cambios. Esta es la característica clave de `ReadOnlyCollection`.

    var groceryList = new List<string> { "Apple", "Banana" };

    var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);

    var itemCount = readOnlyGroceryList.Count;  // There are currently 2 items

    //readOnlyGroceryList.Add("Candy");         // Compiler Error - Items cannot be added to a ReadOnlyCollection object
    groceryList.Add("Vitamins");                // ..but they can be added to the original collection

    itemCount = readOnlyGroceryList.Count;      // Now there are 3 items
    var lastItem = readOnlyGroceryList.Last();  // The last item on the read only list is now "Vitamins"

[Ver demostración][1]


[1]: https://dotnetfiddle.net/C8qQrS

## Advertencia: los elementos de una ReadOnlyCollection no son inherentemente de solo lectura
Si la colección de origen es de un tipo que no es inmutable, se pueden modificar los elementos a los que se accede a través de una `ReadOnlyCollection`.

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


[Ver demostración][1]


[1]: https://dotnetfiddle.net/fXE66F

