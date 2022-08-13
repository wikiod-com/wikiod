---
title: "ReadOnlyKoleksiyonlar"
slug: "readonlykoleksiyonlar"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Bir "ReadOnlyCollection", mevcut bir koleksiyona ("kaynak koleksiyon") salt okunur bir görünüm sağlar.

Öğeler bir 'ReadOnlyCollection'a doğrudan eklenmez veya buradan kaldırılmaz. Bunun yerine, bunlar kaynak koleksiyondan eklenir ve çıkarılır ve 'ReadOnlyCollection' bu değişiklikleri kaynağa yansıtır.

Bir ReadOnlyCollection içindeki öğelerin sayısı ve sırası değiştirilemez, ancak kapsam içinde oldukları varsayılarak öğelerin özellikleri değiştirilebilir ve yöntemler çağrılabilir.

Harici kodun koleksiyonunuzu değiştirmeden görüntülemesine izin vermek istediğinizde bir `ReadOnlyCollection' kullanın, ancak yine de koleksiyonu kendiniz değiştirebilirsiniz.

Ayrıca bakınız
* `Gözlenebilir Koleksiyon<T>`
* `ReadOnlyObservableCollection<T>`


ReadOnlyCollections ve ImmutableCollection karşılaştırması
------------------------------------------

Bir "ReadOnlyCollection", bir "ImmutableCollection"ı oluşturduktan sonra düzenleyememeniz bakımından "ImmutableCollection"dan farklıdır - her zaman "n" öğeleri içerir ve bunlar değiştirilemez veya yeniden düzenlenemez. Öte yandan bir 'ReadOnlyCollection' doğrudan düzenlenemez, ancak kaynak koleksiyon kullanılarak öğeler yine de eklenebilir/kaldırılabilir/yeniden sıralanabilir.


## ReadOnlyCollection Oluşturma
Yapıcıyı Kullanma
-----------
Bir "ReadOnlyCollection", var olan bir "IList" nesnesini yapıcıya geçirerek oluşturulur:

    var groceryList = new List<string> { "Apple", "Banana" };
    var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);


LINQ kullanma
-----------
Ek olarak, LINQ, IList nesneleri için bir 'AsReadOnly()' uzantı yöntemi sağlar:

    var readOnlyVersion = groceryList.AsReadOnly();

Not
----
Tipik olarak, kaynak koleksiyonunu özel olarak korumak ve `ReadOnlyCollection`a genel erişime izin vermek istersiniz. Bir satır içi listeden bir 'ReadOnlyCollection' oluşturabilseniz de, koleksiyonu oluşturduktan sonra değiştiremezsiniz.

    var readOnlyGroceryList = new List<string> {"Apple", "Banana"}.AsReadOnly();
    // Great, but you will not be able to update the grocery list because 
    //  you do not have a reference to the source list anymore!

Kendinizi bunu yaparken bulursanız, "ImmutableCollection" gibi başka bir veri yapısı kullanmayı düşünebilirsiniz.

## ReadOnlyCollection Güncelleme
Bir 'ReadOnlyCollection' doğrudan düzenlenemez. Bunun yerine kaynak koleksiyon güncellenir ve 'ReadOnlyCollection' bu değişiklikleri yansıtır. Bu, ReadOnlyCollection'ın temel özelliğidir.

    var groceryList = new List<string> { "Apple", "Banana" };

    var readOnlyGroceryList = new ReadOnlyCollection<string>(groceryList);

    var itemCount = readOnlyGroceryList.Count;  // There are currently 2 items

    //readOnlyGroceryList.Add("Candy");         // Compiler Error - Items cannot be added to a ReadOnlyCollection object
    groceryList.Add("Vitamins");                // ..but they can be added to the original collection

    itemCount = readOnlyGroceryList.Count;      // Now there are 3 items
    var lastItem = readOnlyGroceryList.Last();  // The last item on the read only list is now "Vitamins"

[Demoyu Görüntüle][1]


[1]: https://dotnetfiddle.net/C8qQrS

## Uyarı: ReadOnlyCollection'daki öğeler doğal olarak salt okunur değildir
Kaynak koleksiyon değişmez olmayan bir türdeyse, bir 'ReadOnlyCollection' aracılığıyla erişilen öğeler değiştirilebilir.

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


[Demoyu Görüntüle][1]


[1]: https://dotnetfiddle.net/fXE66F

