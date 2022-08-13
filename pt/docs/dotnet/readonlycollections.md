---
title: "Coleções ReadOnly"
slug: "colecoes-readonly"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Um `ReadOnlyCollection` fornece uma visualização somente leitura para uma coleção existente (a 'coleção de origem').

Os itens não são adicionados ou removidos diretamente de um `ReadOnlyCollection`. Em vez disso, eles são adicionados e removidos da coleção de origem e o `ReadOnlyCollection` refletirá essas alterações na origem.

O número e a ordem dos elementos dentro de um `ReadOnlyCollection` não podem ser modificados, mas as propriedades dos elementos podem ser e os métodos podem ser chamados, desde que estejam no escopo.

Use um `ReadOnlyCollection` quando quiser permitir que código externo visualize sua coleção sem poder modificá-la, mas ainda assim poder modificar a coleção você mesmo.

Veja também
* `ObservableCollection<T>`
* `ReadOnlyObservableCollection<T>`


ReadOnlyCollections vs ImutableCollection
------------------------------------------

Um `ReadOnlyCollection` difere de um `ImmutableCollection` em que você não pode editar um `ImmutableCollection` depois de criado - ele sempre conterá `n` elementos, e eles não podem ser substituídos ou reordenados. Um `ReadOnlyCollection`, por outro lado, não pode ser editado diretamente, mas os elementos ainda podem ser adicionados/removidos/reordenados usando a coleção de origem.


## Criando uma ReadOnlyCollection
Usando o Construtor
-----------
Um `ReadOnlyCollection` é criado passando um objeto `IList` existente para o construtor:

    var groceryList = new List<string> { "Apple", "Banana" };
    var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);


Usando LINQ
-----------
Além disso, o LINQ fornece um método de extensão `AsReadOnly()` para objetos `IList`:

    var readOnlyVersion = groceryList.AsReadOnly();

Observação
----
Normalmente, você deseja manter a coleção de origem de forma privada e permitir acesso público ao `ReadOnlyCollection`. Embora você possa criar um `ReadOnlyCollection` a partir de uma lista em linha, não poderá modificar a coleção depois de criá-la.

    var readOnlyGroceryList = new List<string> {"Apple", "Banana"}.AsReadOnly();
    // Great, but you will not be able to update the grocery list because 
    //  you do not have a reference to the source list anymore!

Se você estiver fazendo isso, considere usar outra estrutura de dados, como uma `ImmutableCollection`.

## Atualizando uma ReadOnlyCollection
Um `ReadOnlyCollection` não pode ser editado diretamente. Em vez disso, a coleção de origem é atualizada e o `ReadOnlyCollection` refletirá essas alterações. Este é o principal recurso do `ReadOnlyCollection`.

    var groceryList = new List<string> { "Apple", "Banana" };

    var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);

    var itemCount = readOnlyGroceryList.Count;  // There are currently 2 items

    //readOnlyGroceryList.Add("Candy");         // Compiler Error - Items cannot be added to a ReadOnlyCollection object
    groceryList.Add("Vitamins");                // ..but they can be added to the original collection

    itemCount = readOnlyGroceryList.Count;      // Now there are 3 items
    var lastItem = readOnlyGroceryList.Last();  // The last item on the read only list is now "Vitamins"

[Ver demonstração][1]


[1]: https://dotnetfiddle.net/C8qQrS

## Aviso: os elementos em um ReadOnlyCollection não são inerentemente somente leitura
Se a coleção de origem for de um tipo não imutável, os elementos acessados ​​por meio de um `ReadOnlyCollection` podem ser modificados.

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


[Ver demonstração][1]


[1]: https://dotnetfiddle.net/fXE66F

