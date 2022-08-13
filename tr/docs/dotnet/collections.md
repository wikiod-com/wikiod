---
title: "Koleksiyonlar"
slug: "koleksiyonlar"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

Birkaç çeşit koleksiyon vardır:

- "Dizi"
- "Liste"
- "Sıra"
- 'Sıralı Liste'
- "Yığın"
- [Sözlük][1]


[1]: https://www.wikiod.com/tr/dotnet/sozlukler

## Koleksiyon başlatıcıları kullanma
Bazı koleksiyon türleri, bildirim zamanında başlatılabilir. Örneğin, aşağıdaki ifade bazı tamsayılarla "sayıları" oluşturur ve başlatır:

    List<int> numbers = new List<int>(){10, 9, 8, 7, 7, 6, 5, 10, 4, 3, 2, 1};

Dahili olarak, C# derleyicisi aslında bu başlatmayı Add yöntemine yapılan bir dizi çağrıya dönüştürür. Sonuç olarak, bu söz dizimini yalnızca 'Ekle' yöntemini gerçekten destekleyen koleksiyonlar için kullanabilirsiniz.

>`Stack<T>` ve `Queue<T>` sınıfları bunu desteklemez.

Anahtar/değer çiftlerini alan "Dictionary<TKey, TValue>" sınıfı gibi karmaşık koleksiyonlar için, başlatıcı listesinde her bir anahtar/değer çiftini anonim bir tür olarak belirtebilirsiniz.

    Dictionary<int, string> employee = new Dictionary<int, string>()
         {{44, "John"}, {45, "Bob"}, {47, "James"}, {48, "Franklin"}};
Her çiftteki ilk öğe anahtar, ikincisi ise değerdir.

## Özel Türlerle Başlatılmış Liste Oluşturma
    public class Model
    {
        public string Name { get; set; }
        public bool? Selected { get; set; }
    }

Burada, iki özelliğe sahip, yapıcısı olmayan bir Sınıfımız var: "Ad" ve null yapılabilir bir boole özelliği "Selected". Bir `List<Model>` başlatmak istiyorsak, bunu yürütmenin birkaç farklı yolu vardır.

    var SelectedEmployees = new List<Model>
     {
          new Model() {Name = "Item1", Selected = true},
          new Model() {Name = "Item2", Selected = false},
          new Model() {Name = "Item3", Selected = false},
          new Model() {Name = "Item4"}
     };

Burada, "Model" sınıfımızın birkaç "yeni" örneğini oluşturuyoruz ve bunları verilerle başlatıyoruz. Bir kurucu eklesek ne olur?

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

Bu, Listemizi *biraz* farklı bir şekilde başlatmamızı sağlar.

    var SelectedEmployees = new List<Model>
    {
        new Model("Mark", true),
        new Model("Alexis"),
        new Model("")
    };

Özelliklerden birinin bir sınıfın kendisi olduğu bir Sınıfa ne dersiniz?

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

Örneği biraz basitleştirmek için yapıcıyı "Model" sınıfında geri aldığımıza dikkat edin.

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

'Liste<GenişletilmişModel>' ile 'Koleksiyon<GenişletilmişModel>', 'GenişletilmişModel[]', 'nesne[]' ve hatta sadece '[]' ile değiştirebileceğimizi unutmayın.

## Sıra
.Net'te [FIFO (ilk giren ilk çıkar)][2] konseptini kullanan bir [`Kuyruk`][1] içindeki değerleri yönetmek için kullanılan bir koleksiyon vardır. Kuyrukların temelleri, kuyruğa eleman eklemek için kullanılan [`Enqueue(T item)`][3] ve ilk elemanı alıp kaldırmak için kullanılan [`Dequeue()`][4] yöntemidir. kuyruktan. Genel sürüm, bir dizi dizisi için aşağıdaki kod gibi kullanılabilir.

İlk olarak, ad alanını ekleyin:

    using System.Collections.Generic;

ve kullanın:

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

Nesnelerle çalışan türün genel olmayan bir sürümü vardır.

Ad alanı:

    using System.Collections;

Genel olmayan sıra için bir kod örneği ekleyin:

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

Ayrıca [Peek()][5] adlı bir yöntem de vardır ve bu yöntem, sıranın başındaki nesneyi, öğeleri kaldırmadan döndürür.

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
    
Çıktı (kaldırmadan):

    10
    20
    30
    40
    50

[1]: https://msdn.microsoft.com/library/system.collections.queue(v=vs.110).aspx
[2]: https://en.wikipedia.org/wiki/FIFO_(computing_and_electronics)
[3]: https://msdn.microsoft.com/library/t249c2y7(v=vs.110).aspx
[4]: https://msdn.microsoft.com/library/1c8bzx97(v=vs.110).aspx
[5]: https://msdn.microsoft.com/library/system.collections.queue.peek(v=vs.110).aspx

## Yığın
.Net'te, [LIFO (son giren ilk çıkar)][2] konseptini kullanan bir [`Stack`][1] içindeki değerleri yönetmek için kullanılan bir koleksiyon vardır. Yığınların temelleri, yığına öğe eklemek için kullanılan [`Push(T öğesi)`][3] yöntemi ve son öğenin eklenmesi ve kaldırılması için kullanılan [`Pop()`][4] yöntemidir. yığından. Genel sürüm, bir dizi dizisi için aşağıdaki kod gibi kullanılabilir.

İlk olarak, ad alanını ekleyin:

    using System.Collections.Generic;

ve kullanın:

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

Nesnelerle çalışan türün genel olmayan bir sürümü vardır.

Ad alanı:

    using System.Collections;

Ve genel olmayan yığının bir kod örneği:

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

Ayrıca [Peek()][5] adında, eklenen son öğeyi 'Yığın'dan çıkarmadan döndüren bir yöntem de vardır.

    Stack<int> stack = new Stack<int>();
    stack.Push(10);
    stack.Push(20);
    
    var lastValueAdded = stack.Peek(); // 20

Yığındaki öğeler üzerinde yineleme yapmak mümkündür ve yığının sırasına (LIFO) uyacaktır.

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
    
Çıktı (kaldırmadan):

    50
    40
    30
    20
    10
    


[1]: https://msdn.microsoft.com/library/system.collections.stack(v=vs.110).aspx
[2]: https://en.wikipedia.org/wiki/Stack_(abstract_data_type)
[3]: https://msdn.microsoft.com/library/system.collections.stack.push(v=vs.110).aspx
[4]: https://msdn.microsoft.com/library/system.collections.stack.pop(v=vs.110).aspx
[5]: https://msdn.microsoft.com/library/system.collections.stack.peek(v=vs.110).aspx

