---
title: "Nesnelere Linq"
slug: "nesnelere-linq"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

LINQ to Objects, herhangi bir IEnumerable koleksiyonuyla LINQ sorgularının kullanımını ifade eder.

## C#'da LINQ to Objects Kullanmak
**Linq'te basit bir SELECT sorgusu**

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

Yukarıdaki örnekte, LINQ kullanılarak sorgulanacak nesnelerin bir koleksiyonu olarak bir dizi dizi (araba) kullanılır. Bir LINQ sorgusunda, veri kaynağını (arabalar) ve aralık değişkenini (araba) tanıtmak için from yan tümcesi önce gelir. Sorgu yürütüldüğünde, aralık değişkeni, arabalardaki her bir ardışık öğeye referans olarak hizmet edecektir. Derleyici araba türünü çıkarabildiğinden, bunu açıkça belirtmeniz gerekmez.

Yukarıdaki kod derlenip çalıştırıldığında aşağıdaki sonucu verir:
[![buraya resim açıklamasını girin][1]][1]

**WHERE Cümlesiyle SEÇİN**

    var list = from car in cars
               where car.Contains("VW")
               select car;

WHERE yan tümcesi, WHERE yan tümcesini karşılayan bir dizi alt kümesini bulmak ve döndürmek için dize dizisini (arabalar) sorgulamak için kullanılır.

Yukarıdaki kod derlenip çalıştırıldığında aşağıdaki sonucu verir:

[![buraya resim açıklamasını girin][2]][2]


**Sıralı Liste Oluşturma**

    var list = from car in cars
               orderby car ascending 
               select car;

Bazen döndürülen verileri sıralamak yararlıdır. orderby yan tümcesi, öğelerin sıralanan tür için varsayılan karşılaştırıcıya göre sıralanmasına neden olur.

Yukarıdaki kod derlenip çalıştırıldığında aşağıdaki sonucu verir:

[![buraya resim açıklamasını girin][3]][3]


**Özel bir türle çalışma**

Bu örnekte, yazılan bir liste oluşturulur, doldurulur ve ardından sorgulanır.

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

Yukarıdaki kod derlenip çalıştırıldığında aşağıdaki sonucu verir:

[![buraya resim açıklamasını girin][4]][4]


Şimdiye kadar örnekler şaşırtıcı görünmüyor, çünkü kişi temelde aynı şeyi yapmak için diziyi yineleyebilir. Ancak, aşağıdaki birkaç örnekle, LINQ to Objects ile nasıl daha karmaşık sorgular oluşturabileceğinizi ve çok daha az kodla daha fazlasını nasıl elde edeceğinizi görebilirsiniz.

Aşağıdaki örnekte 60000 adetten fazla satılmış arabaları seçip bunları satılan adet sayısına göre sıralayabiliriz:

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

Yukarıdaki kod derlenip çalıştırıldığında aşağıdaki sonucu verir:
[![buraya resim açıklamasını girin][5]][5]


Aşağıdaki örnekte, tek sayıda birim satan arabaları seçip adlarına göre alfabetik olarak sıralayabiliriz:

    var list = from car in cars
               where car.UnitsSold % 2 != 0 
               orderby car.Name ascending 
               select car;

Yukarıdaki kod derlenip çalıştırıldığında aşağıdaki sonucu verir:
[![buraya resim açıklamasını girin][6]][6]


[1]: https://i.stack.imgur.com/lG65Q.png
[2]: https://i.stack.imgur.com/llGXx.png
[3]: https://i.stack.imgur.com/ODH55.png
[4]: https://i.stack.imgur.com/0jUOC.png
[5]: https://i.stack.imgur.com/ZDeTt.png
[6]: https://i.stack.imgur.com/fJnTp.png

## LINQ to Object sorguları nasıl yürütür?
LINQ sorguları hemen yürütülmez. Sorguyu oluştururken, sorguyu gelecekteki yürütme için saklarsınız. Yalnızca sorguyu gerçekten yinelemeyi talep ettiğinizde sorgu yürütülür (örneğin, bir for döngüsünde, ToList, Count, Max, Average, First, vb. çağrılırken)

Bu *ertelenmiş yürütme* olarak kabul edilir. Bu, sorguyu birden çok adımda oluşturmanıza, koşullu ifadelere dayalı olarak potansiyel olarak değiştirmenize ve daha sonra yalnızca sonuca ihtiyaç duyduğunuzda yürütmenize olanak tanır.

Kod verildiğinde:

    var query = from n in numbers 
                where n % 2 != 0
                select n;

Yukarıdaki örnek, sorguyu yalnızca "sorgu" değişkeninde saklar. Sorgunun kendisini yürütmez.

"foreach" ifadesi, sorgu yürütmesini zorlar:

    foreach(var n in query) {
        Console.WriteLine($"Number selected {n}");
    }

Bazı LINQ yöntemleri ayrıca "Count", "First", "Max", "Average" sorgu yürütmesini de tetikler. Tek değerler döndürürler. 'ToList' ve 'ToArray' sonuçları toplar ve bunları sırasıyla bir Liste veya Dizi'ye dönüştürür.

Aynı sorgu üzerinde birden çok LINQ işlevini çağırırsanız, sorgu boyunca birden çok kez yineleme yapmanın mümkün olduğunu unutmayın. Bu, her aramada size farklı sonuçlar verebilir. Yalnızca bir veri kümesiyle çalışmak istiyorsanız, onu bir liste veya diziye kaydettiğinizden emin olun.



