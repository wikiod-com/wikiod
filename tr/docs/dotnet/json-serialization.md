---
title: "JSON Serileştirme"
slug: "json-serilestirme"
draft: false
images: []
weight: 9725
type: docs
toc: true
---

**JavaScriptSerializer ve Json.NET**

[`JavaScriptSerializer` sınıfı](https://msdn.microsoft.com/en-us/library/system.web.script.serialization.javascriptserializer(v=vs.110).aspx) .NET 3.5'te tanıtıldı ve AJAX etkin uygulamalar için .NET'in eşzamansız iletişim katmanı tarafından dahili olarak kullanılır. Yönetilen kodda JSON ile çalışmak için kullanılabilir.

'JavaScriptSerializer' sınıfının varlığına rağmen, Microsoft, serileştirme ve seri durumdan çıkarma için açık kaynak [Json.NET kitaplığı](http://www.newtonsoft.com/json) kullanılmasını önerir. Json.NET, JSON'u özel sınıflara (özel bir [`JavaScriptConverter` nesnesi](https://msdn.microsoft.com/en-us/library/system.web.script.serialization) eşlemek için daha iyi performans ve daha dostça bir arayüz sunar. 'JavaScriptSerializer' ile aynı şeyi gerçekleştirmek için javascriptconverter(v=vs.110).aspx) gerekir).

## System.Web.Script.Serialization.JavaScriptSerializer kullanarak seri durumdan çıkarma
"JavaScriptSerializer.Deserialize<T>(input)" yöntemi, "JavaScriptSerializer" tarafından yerel olarak desteklenen varsayılan eşlemeleri kullanarak, geçerli bir JSON dizesini belirtilen "<T>" türündeki bir nesneye seri durumdan çıkarmaya çalışır.

<!-- dil: c# -->
    using System.Collections;
    using System.Web.Script.Serialization;

    // ...

    string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

    JavaScriptSerializer JSS = new JavaScriptSerializer(); 
    Dictionary<string, object> parsedObj = JSS.Deserialize<Dictionary<string, object>>(rawJSON);

    string name = parsedObj["Name"].toString();
    ArrayList numbers = (ArrayList)parsedObj["Numbers"]

Not: 'JavaScriptSerializer' nesnesi .NET sürüm 3.5'te tanıtıldı

## Json.NET kullanarak seri durumdan çıkarma
<!-- dil: c# -->
    internal class Sequence{
        public string Name;
        public List<int> Numbers;
    }    

    // ...

    string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

    Sequence sequence = JsonConvert.DeserializeObject<Sequence>(rawJSON);

Daha fazla bilgi için [Json.NET resmi sitesine](http://www.newtonsoft.com/json) bakın.

Not: Json.NET, .NET sürüm 2 ve üstünü destekler.

## Json.NET kullanarak seri hale getirme
    [JsonObject("person")]
    public class Person
    {
        [JsonProperty("name")]
        public string PersonName { get; set; }
        [JsonProperty("age")]
        public int PersonAge { get; set; }
        [JsonIgnore]
        public string Address { get; set; }
    }

    Person person = new Person { PersonName = "Andrius", PersonAge = 99, Address = "Some address" };
    string rawJson = JsonConvert.SerializeObject(person);

    Console.WriteLine(rawJson); // {"name":"Andrius","age":99}
Özelliklerin (ve sınıfların) ortaya çıkan json dizesindeki görünümlerini değiştirmek veya bunları json dizesinden tamamen kaldırmak için niteliklerle nasıl dekore edilebileceğine dikkat edin (JsonIgnore).

Json.NET serileştirme öznitelikleri hakkında daha fazla bilgi [burada][1] bulunabilir.

C#'ta genel tanımlayıcılar, kural olarak *PascalCase*'de yazılır. JSON'da kural, tüm adlar için *camelCase* kullanmaktır. İkisi arasında dönüşüm yapmak için bir sözleşme çözümleyici kullanabilirsiniz.

    using Newtonsoft.Json;
    using Newtonsoft.Json.Serialization;

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        [JsonIgnore]
        public string Address { get; set; }
    }

    public void ToJson() {
        Person person = new Person { Name = "Andrius", Age = 99, Address = "Some address" };
        var resolver = new CamelCasePropertyNamesContractResolver();
        var settings = new JsonSerializerSettings { ContractResolver = resolver };
        string json = JsonConvert.SerializeObject(person, settings);

        Console.WriteLine(json); // {"name":"Andrius","age":99}
    }

[1]: http://www.newtonsoft.com/json/help/html/serializationattributes.htm

## Newtonsoft.Json kullanarak Serileştirme-Deserialization
Diğer yardımcılardan farklı olarak, bu, seri hale getirmek ve seri durumdan çıkarmak için statik sınıf yardımcılarını kullanır, bu nedenle diğerlerinden biraz daha kolaydır.


    using Newtonsoft.Json;

    var rawJSON      = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
    var fibo         = JsonConvert.DeserializeObject<Dictionary<string, object>>(rawJSON);
    var rawJSON2     = JsonConvert.SerializeObject(fibo);



## Dinamik bağlama
Newtonsoft'un Json.NET'i, türü açıkça oluşturmanıza gerek kalmadan json'u dinamik olarak ( ExpandoObject / Dynamic nesneleri kullanarak) bağlamanıza izin verir.

**Seri hale getirme**

    dynamic jsonObject = new ExpandoObject();
    jsonObject.Title   = "Merchent of Venice";
    jsonObject.Author  = "William Shakespeare";
    Console.WriteLine(JsonConvert.SerializeObject(jsonObject));


**Seri hale getirme**

    var rawJson = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
    dynamic parsedJson = JObject.Parse(rawJson);
    Console.WriteLine("Name: " + parsedJson.Name);
    Console.WriteLine("Name: " + parsedJson.Numbers.Length);

rawJson nesnesindeki anahtarların dinamik nesnede üye değişkenlere dönüştürüldüğüne dikkat edin.

Bu, bir uygulamanın farklı JSON formatlarını kabul edebileceği/üretebileceği durumlarda kullanışlıdır. Bununla birlikte, serileştirme/serileştirmenin bir sonucu olarak oluşturulan Json dizesi veya dinamik nesne için fazladan bir doğrulama düzeyi kullanılması önerilir.

## Json.NET'i JsonSerializerSettings ile kullanarak seri hale getirme
Bu serileştirici, varsayılan .net json serileştiricisinin sahip olmadığı, Null değer işleme gibi bazı güzel özelliklere sahiptir, sadece `JsonSerializerSettings` oluşturmanız yeterlidir:

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore});
       return result;
    }

.net'teki bir başka ciddi serileştirici sorunu da kendi kendine referans veren döngüdür. Bir kursa kayıtlı bir öğrenci olması durumunda, örneğinin bir kurs özelliği vardır ve bir kurs, bir referans döngüsü oluşturacak olan 'Liste<Öğrenci>' anlamına gelen bir öğrenci koleksiyonuna sahiptir. Bunu `JsonSerializerSettings` ile halledebilirsiniz:

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
       return result;
    }

Bunun gibi çeşitli serileştirme seçenekleri koyabilirsiniz:

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore, ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
       return result;
    }


