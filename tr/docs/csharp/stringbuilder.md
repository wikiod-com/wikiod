---
title: "StringBuilder"
slug: "stringbuilder"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## StringBuilder nedir ve ne zaman kullanılır
Bir [`StringBuilder`][1], normal bir dizeden farklı olarak değişken olan bir dizi karakteri temsil eder. Çoğu zaman, daha önce oluşturduğumuz dizeleri değiştirmeye ihtiyaç vardır, ancak standart dize nesnesi değiştirilemez. Bu, bir dize her değiştirildiğinde, yeni bir dize nesnesinin oluşturulması, kopyalanması ve ardından yeniden atanması gerektiği anlamına gelir.

    string myString = "Apples";
    mystring += " are my favorite fruit";

Yukarıdaki örnekte, "myString" başlangıçta yalnızca "Elmalar"' değerine sahiptir. Bununla birlikte, "en sevdiğim meyvelerdir"i birleştirdiğimizde, string sınıfının dahili olarak yapması gerekenler şunları içerir:

- `myString` uzunluğuna ve eklediğimiz yeni dizeye eşit yeni bir karakter dizisi oluşturma.
- `myString`in tüm karakterlerini yeni dizimizin başına kopyalamak ve yeni diziyi dizinin sonuna kopyalamak.
- Bellekte yeni bir dize nesnesi oluşturun ve onu `myString`e yeniden atayın.

Tek bir birleştirme için bu nispeten önemsizdir. Ancak, örneğin bir döngüde birçok ekleme işlemi gerçekleştirmek gerekirse ne olur?

    String myString = "";
    for (int i = 0; i < 10000; i++)
        myString += " "; // puts 10,000 spaces into our string

Tekrarlanan kopyalama ve nesne oluşturma nedeniyle, bu, programımızın performansını önemli ölçüde düşürecektir. Bunun yerine bir `StringBuilder` kullanarak bundan kaçınabiliriz.

    StringBuilder myStringBuilder = new StringBuilder();    
    for (int i = 0; i < 10000; i++)
        myStringBuilder.Append(' ');

Şimdi aynı döngü çalıştırıldığında, programın yürütme süresinin performansı ve hızı, normal bir dize kullanmaktan önemli ölçüde daha hızlı olacaktır. 'StringBuilder'ı normal bir dizgeye geri döndürmek için, 'StringBuilder'ın 'ToString()' yöntemini çağırmamız yeterlidir.


----------
Ancak, 'StringBuilder'ın sahip olduğu tek optimizasyon bu değildir. İşlevleri daha da optimize etmek için performansı iyileştirmeye yardımcı olan diğer özelliklerden yararlanabiliriz.

    StringBuilder sb = new StringBuilder(10000); // initializes the capacity to 10000

StringBuilder'ımızın ne kadar uzun olması gerektiğini önceden biliyorsak, boyutunu önceden belirleyebiliriz, bu da dahili olarak sahip olduğu karakter dizisini yeniden boyutlandırmasına gerek duymasını engeller.

    sb.Append('k', 2000);

Eklemek için 'StringBuilder' kullanmak bir dizgeden çok daha hızlı olsa da, birçok kez yalnızca tek bir karakter eklemeniz gerekiyorsa daha da hızlı çalışabilir.

Dizenizi oluşturmayı tamamladıktan sonra, onu temel bir "dize"ye dönüştürmek için "StringBuilder" üzerindeki "ToString()" yöntemini kullanabilirsiniz. Bu genellikle gereklidir, çünkü 'StringBuilder' sınıfı 'string'den miras almaz.

Örneğin, bir "dize" oluşturmak için bir "StringBuilder"ı nasıl kullanabileceğiniz aşağıda açıklanmıştır:

    string RepeatCharacterTimes(char character, int times)
    {
        StringBuilder builder = new StringBuilder("");
        for (int counter = 0; counter < times; counter++)
        {
            //Append one instance of the character to the StringBuilder.
            builder.Append(character);
        }
        //Convert the result to string and return it.
        return builder.ToString();
    }

----------
Sonuç olarak, performans göz önünde bulundurularak bir dizide birçok değişiklik yapılması gerektiğinde, dize yerine `StringBuilder` kullanılmalıdır.


[1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

## Çok sayıda kayıttan dize oluşturmak için StringBuilder'ı kullanın
    public string GetCustomerNamesCsv()
    {
        List<CustomerData> customerDataRecords = GetCustomerData(); // Returns a large number of records, say, 10000+
    
        StringBuilder customerNamesCsv = new StringBuilder();
        foreach (CustomerData record in customerDataRecords)
        {
           customerNamesCsv
               .Append(record.LastName)
               .Append(',')
               .Append(record.FirstName)
               .Append(Environment.NewLine);
        }

        return customerNamesCsv.ToString();
    }


