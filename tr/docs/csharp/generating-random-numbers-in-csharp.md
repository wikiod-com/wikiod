---
title: "C# ile Rastgele Sayılar Üretmek"
slug: "c-ile-rastgele-saylar-uretmek"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

## Sözdizimi
- Rastgele()

- Rastgele (int Tohum)

- int Sonraki()

- int Sonraki(int maxValue)

- int Sonraki(int minValue, int maxValue)


## Parametreler
| parametreler | Ayrıntılar |
| ---------- | ------- |
| tohum | Rastgele sayılar üretmek için bir değer. Ayarlanmazsa, varsayılan değer mevcut sistem saatine göre belirlenir.
| minDeğer | Oluşturulan sayılar bu değerden küçük olmayacaktır. Ayarlanmadıysa, varsayılan değer 0'dır.
| maxDeğer | Oluşturulan sayılar bu değerden daha küçük olacaktır. Ayarlanmadıysa, varsayılan değer `Int32.MaxValue`dur.
| dönüş değeri | Rastgele değere sahip bir sayı döndürür.

Sistem tarafından oluşturulan rastgele tohum, her farklı çalıştırmada aynı değildir.

Aynı zamanda üretilen tohumlar aynı olabilir.

## Rastgele bir int oluştur
Bu örnek, 0 ile 2147483647 arasında rastgele değerler üretir.

    Random rnd = new Random();
    int randomNumber = rnd.Next();

## Belirli bir aralıkta rastgele bir int üret
"minValue" ve "maxValue - 1" arasında rastgele bir sayı oluşturun.

    Random rnd = new Random();
    var randomBetween10And20 = rnd.Next(10, 20);

## Aynı rastgele sayı dizisini tekrar tekrar oluşturma
Aynı tohumla "Rastgele" örnekler oluştururken aynı sayılar üretilecektir.

    int seed = 5;
    for (int i = 0; i < 2; i++)
    {
       Console.WriteLine("Random instance " + i);
       Random rnd = new Random(seed);
       for (int j = 0; j < 5; j++)
       {
          Console.Write(rnd.Next());
          Console.Write(" ");
       }
    
       Console.WriteLine();
    }

Çıktı:

    Random instance 0
    726643700 610783965 564707973 1342984399 995276750
    Random instance 1
    726643700 610783965 564707973 1342984399 995276750

## Aynı anda farklı tohumlarla birden fazla rastgele sınıf oluşturun
Aynı anda oluşturulan iki Random sınıfı aynı tohum değerine sahip olacaktır.

`System.Guid.NewGuid().GetHashCode()` kullanarak aynı anda bile farklı bir tohum elde edebilirsiniz.

    Random rnd1 = new Random();
    Random rnd2 = new Random();
    Console.WriteLine("First 5 random number in rnd1");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd1.Next());

    Console.WriteLine("First 5 random number in rnd2");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd2.Next());

    rnd1 = new Random(Guid.NewGuid().GetHashCode());
    rnd2 = new Random(Guid.NewGuid().GetHashCode());
    Console.WriteLine("First 5 random number in rnd1 using Guid");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd1.Next());
    Console.WriteLine("First 5 random number in rnd2 using Guid");
    for (int i = 0; i < 5; i++)
        Console.WriteLine(rnd2.Next());

Farklı tohumlar elde etmenin başka bir yolu, tohum değerlerini almak için başka bir "Random" örneğini kullanmaktır.

    Random rndSeeds = new Random();
    Random rnd1 = new Random(rndSeeds.Next());
    Random rnd2 = new Random(rndSeeds.Next());
Bu aynı zamanda tüm "Random" örneklerinin sonucunu, "rndSeeds" için yalnızca tohum değerini ayarlayarak kontrol etmeyi mümkün kılar. Diğer tüm örnekler, bu tek tohum değerinden deterministik olarak türetilecektir.

## Rastgele bir çift oluştur
0 ile 1.0 arasında rastgele bir sayı oluşturun. (1.0 dahil değil)

    Random rnd = new Random();
    var randomDouble = rnd.NextDouble();



## Rastgele bir karakter oluştur
Belirli bir sayı aralığı için "Next()" aşırı yüklemesini kullanarak "a" ve "z" arasında rastgele bir harf oluşturun, ardından elde edilen "int"i "char"a dönüştürün

    Random rnd = new Random();
    char randomChar = (char)rnd.Next('a','z'); 
    //'a' and 'z' are interpreted as ints for parameters for Next()
    

## Maksimum değerin yüzdesi olan bir sayı üret
Rastgele sayılar için ortak bir ihtiyaç, bir maksimum değerin "%X"i olan bir sayı üretmektir. bu, "NextDouble()" sonucunun yüzde olarak işlenmesiyle yapılabilir:

    var rnd = new Random();
    var maxValue = 5000;
    var percentage = rnd.NextDouble();
    var result = maxValue * percentage; 
    //suppose NextDouble() returns .65, result will hold 65% of 5000: 3250.



