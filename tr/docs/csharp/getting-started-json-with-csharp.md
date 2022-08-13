---
title: "C# ile Json'a Başlarken"
slug: "c-ile-jsona-baslarken"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

Aşağıdaki konu, C# dilini ve Serileştirme ve Seriyi Kaldırma kavramlarını kullanarak Json ile çalışmanın bir yolunu tanıtacaktır.

## Basit Json Örneği
    {
        "id": 89,
        "name": "Aldous Huxley",
        "type": "Author",
        "books":[{
                   "name": "Brave New World",
                   "date": 1932 
                 },
                 {
                   "name": "Eyeless in Gaza",
                   "date": 1936
                 },
                 {
                   "name": "The Genius and the Goddess",
                   "date": 1955 
                 }]  
    }

Json'da yeniyseniz, işte bir [örneklenmiş öğretici][1].


[1]: https://www.w3schools.com/js/js_json_intro.asp "eğitici"

## First Things First: Json ile çalışacak kitaplık
C# kullanarak Json ile çalışmak için Newtonsoft (.net kitaplığı) kullanmak gerekir. Bu kitaplık, programcının nesneleri ve daha fazlasını serileştirmesine ve serisini kaldırmasına izin veren yöntemler sağlar.
[Bir öğretici var][1] Yöntemleri ve kullanımları hakkında ayrıntılı bilgi edinmek istiyorsanız.

Visual Studio kullanıyorsanız, *Tools/Nuget Package Manager/Manage Package to Solution/* seçeneğine gidin ve arama çubuğuna "Newtonsoft" yazın ve paketi yükleyin.
NuGet'iniz yoksa, bu [ayrıntılı eğitim][2] size yardımcı olabilir.


[1]: http://www.newtonsoft.com/json
[2]: https://developer.xamarin.com/guides/cross-platform/xamarin-studio/nuget_walkthrough/

## C# Uygulaması
Bazı kodları okumadan önce, json kullanarak uygulamaları programlamaya yardımcı olacak ana kavramları anlamak önemlidir.

> **Serileştirme**: Bir nesneyi, uygulamalar aracılığıyla gönderilebilecek bir bayt akışına dönüştürme işlemi. Aşağıdaki kod seri hale getirilebilir ve önceki json'a dönüştürülebilir.


> **Seri durumdan çıkarma**: Bir json/bayt akışını bir nesneye dönüştürme işlemi. Serileştirme işleminin tam tersi. Önceki json, aşağıdaki örneklerde gösterildiği gibi bir C# nesnesine seri hale getirilebilir.

Bunu çözmek için, daha önce açıklanan süreçleri kullanmak için json yapısını sınıflara dönüştürmek önemlidir. Visual Studio kullanıyorsanız, yalnızca *"Edit/Paste Special/Paste JSON as Classes"* öğesini seçip json yapısını yapıştırarak bir json'u otomatik olarak bir sınıfa dönüştürebilirsiniz.

    using Newtonsoft.Json;

      class Author
    {
        [JsonProperty("id")] // Set the variable below to represent the json attribute 
        public int id;       //"id"
        [JsonProperty("name")]
        public string name;
        [JsonProperty("type")]
        public string type;
        [JsonProperty("books")]
        public Book[] books;

        public Author(int id, string name, string type, Book[] books) {
            this.id = id;
            this.name = name;
            this.type= type;
            this.books = books;
        }
    }

     class Book
    {
       [JsonProperty("name")]
       public string name;
       [JsonProperty("date")]
       public DateTime date;
    }

## Serileştirme
 
     static void Main(string[] args)
        {
            Book[] books = new Book[3];
            Author author = new Author(89,"Aldous Huxley","Author",books);
            string objectDeserialized = JsonConvert.SerializeObject(author); 
            //Converting author into json
        }

".SerializeObject" yöntemi bir *type nesnesi* parametresi olarak alır, böylece içine herhangi bir şey koyabilirsiniz.

## Seri durumdan çıkarma
Bir json'u herhangi bir yerden, bir dosyadan veya hatta bir sunucudan alabilirsiniz, böylece aşağıdaki koda dahil edilmez.

    static void Main(string[] args)
    {
        string jsonExample; // Has the previous json
        Author author = JsonConvert.DeserializeObject<Author>(jsonExample);
    }

".DeserializeObject" yöntemi, '*jsonExample*' öğesini seri durumdan çıkarır ve bir "*Author*" nesnesine dönüştürür. Bu nedenle, sınıf tanımında json değişkenlerini ayarlamak önemlidir, bu nedenle yöntem onu ​​doldurmak için ona erişir.

## Serileştirme ve Serileştirmeden Çıkarma Ortak Yardımcı Programlar işlevi
Bu örnek, tüm tür nesne serileştirme ve seri durumdan çıkarma için ortak işlev için kullanılır.

<pre>
System.Runtime.Serialization.Formatters.Binary kullanarak;
System.Xml.Serialization kullanarak;

ad alanı Çerçevesi
{
genel statik sınıf IGUtilities
{
        public static string Serialization<T>(this T obj)
        {
        string data = JsonConvert.SerializeObject(obj);
        return data;
        }

        public static T Deserialization<T>(this string JsonData)
        {
        T copy = JsonConvert.DeserializeObject<T>(JsonData);
        return copy;
        }

         public static T Clone<T>(this T obj)
        {
            string data = JsonConvert.SerializeObject(obj);
            T copy = JsonConvert.DeserializeObject<T>(data);
            return copy;
        }
}
}
</pre>



