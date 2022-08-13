---
title: "Koleksiyon Başlatıcılar"
slug: "koleksiyon-baslatclar"
draft: false
images: []
weight: 9759
type: docs
toc: true
---

Bu sözdizimsel şeker kullanılarak bir nesnenin başlatılması için tek gereksinim, türün "System.Collections.IEnumerable" ve "Add" yöntemini uygulamasıdır. Buna koleksiyon başlatıcı desek de, nesnenin bir koleksiyon olması *değildir.

## Koleksiyon başlatıcılar
Değerlerle bir koleksiyon türünü başlatın:

    var stringList = new List<string>
    {
        "foo",
        "bar",
    };

Koleksiyon başlatıcılar, 'Add()' çağrıları için sözdizimsel şekerdir. Yukarıdaki kod şuna eşdeğerdir:

    var temp = new List<string>();
    temp.Add("foo");
    temp.Add("bar");
    var stringList = temp;

Yarış koşullarından kaçınmak için başlatma işleminin geçici bir değişken kullanılarak atomik olarak yapıldığını unutmayın.

"Add()" yönteminde birden çok parametre sunan türler için, virgülle ayrılmış bağımsız değişkenleri küme parantezleri içine alın:

    var numberDictionary = new Dictionary<int, string>
    {
        { 1, "One" },
        { 2, "Two" },
    };

Bu şuna eşdeğerdir:

    var temp = new Dictionary<int, string>();
    temp.Add(1, "One");
    temp.Add(2, "Two");
    var numberDictionarynumberDictionary = temp;


## C# 6 Dizin Başlatıcılar
C# 6 ile başlayarak, dizin oluşturuculara sahip koleksiyonlar, atanacak dizini köşeli parantez içinde, ardından eşittir işareti ve ardından atanacak değer belirtilerek başlatılabilir.

# Sözlük Başlatma

Sözlük kullanan bu sözdizimine bir örnek:

    var dict = new Dictionary<string, int>
    {
        ["key1"] = 1,
        ["key2"] = 50
    };

Bu şuna eşdeğerdir:

    var dict = new Dictionary<string, int>();
    dict["key1"] = 1;
    dict["key2"] = 50

Bunu C# 6'dan önce yapmak için koleksiyon başlatıcı sözdizimi şuydu:

    var dict = new Dictionary<string, int>
    {
        { "key1", 1 },
        { "key2", 50 }
    };
    
Hangisine karşılık gelir:

    var dict = new Dictionary<string, int>();
    dict.Add("key1", 1);
    dict.Add("key2", 50);


Dolayısıyla, yeni sözdizimi, değerleri atamak için "Add()" yöntemini kullanmak yerine başlatılmış nesnenin *dizinleyicisini* kullandığından, işlevsellik açısından önemli bir fark vardır. Bu, yeni sözdiziminin yalnızca herkese açık bir dizinleyici gerektirdiği ve buna sahip herhangi bir nesne için çalıştığı anlamına gelir.

    public class IndexableClass
    {
        public int this[int index]
        {
            set 
            { 
                Console.WriteLine("{0} was assigned to index {1}", value, index);
            }
        }
    }

    var foo = new IndexableClass
    {
        [0] = 10,
        [1] = 20
    }

Bu çıktı:

> `10, 0 dizinine atandı'<br/>
> '20, dizin 1'e atandı'



## Özel sınıflarda koleksiyon başlatıcılar
Bir sınıf desteği toplama başlatıcıları yapmak için, "IEnumerable" arabirimini uygulaması ve en az bir "Add" yöntemine sahip olması gerekir. C# 6'dan beri, "IEnumerable" uygulayan herhangi bir koleksiyon, uzantı yöntemleri kullanılarak özel "Add" yöntemleriyle genişletilebilir.

    class Program
    {
        static void Main()
        {
            var col = new MyCollection {
                "foo",
                { "bar", 3 },
                "baz",
                123.45d,
            };
        }
    }
    
    class MyCollection : IEnumerable
    {
        private IList list = new ArrayList();

        public void Add(string item)
        {
            list.Add(item)
        }
    
        public void Add(string item, int count)
        {
            for(int i=0;i< count;i++) {
                list.Add(item);
            }
        }
    
        public IEnumerator GetEnumerator()
        {
            return list.GetEnumerator();
        }
    }
    
    static class MyCollectionExtensions
    {
        public static void Add(this MyCollection @this, double value) => 
            @this.Add(value.ToString());
    }



## Nesne başlatıcı içinde koleksiyon başlatıcıyı kullanma
    public class Tag
    {
        public IList<string> Synonyms { get; set; }
    }

"Synonyms", koleksiyon tipi bir özelliktir. "Etiket" nesnesi, nesne başlatıcı sözdizimi kullanılarak oluşturulduğunda, "Eş anlamlılar" da koleksiyon başlatıcı sözdizimi ile başlatılabilir:

    Tag t = new Tag 
    {
        Synonyms = new List<string> {"c#", "c-sharp"}
    };

Collection özelliği salt okunur olabilir ve yine de koleksiyon başlatıcı sözdizimini destekler. Bu değiştirilmiş örneği düşünün (`Synonyms` özelliği artık özel bir ayarlayıcıya sahip):

    public class Tag
    {
        public Tag()
        {
            Synonyms = new List<string>();
        }
        
        public IList<string> Synonyms { get; private set; }
    }

Yeni bir "Tag" nesnesi şu şekilde oluşturulabilir:

    Tag t = new Tag 
    {
        Synonyms = {"c#", "c-sharp"}
    };

Bu işe yarar, çünkü toplama başlatıcıları 'Add()' çağrıları üzerinde yalnızca sözdizimsel şekerdir. Burada yeni bir liste oluşturulmaz, derleyici sadece çıkmakta olan nesnede 'Add()' için çağrılar üretiyor.

## Parametre Dizili Koleksiyon Başlatıcılar
Normal parametreleri ve parametre dizilerini karıştırabilirsiniz:

    public class LotteryTicket : IEnumerable{
        public int[] LuckyNumbers;
        public string UserName;

        public void Add(string userName, params int[] luckyNumbers){
            UserName = userName;
            Lottery = luckyNumbers;
        }
    }

Bu sözdizimi artık mümkün:

    var Tickets = new List<LotteryTicket>{
        {"Mr Cool"  , 35663, 35732, 12312, 75685},
        {"Bruce"    , 26874, 66677, 24546, 36483, 46768, 24632, 24527},
        {"John Cena", 25446, 83356, 65536, 23783, 24567, 89337}
    }



