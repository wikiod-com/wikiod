---
title: "çoklu eşleme"
slug: "coklu-esleme"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

## Sözdizimi
- `public static IEnumerable<TReturn> Query<TFirst, TSecond, TReturn>(
            this IDbConnection cnn, string sql, Func<TFirst, TSecond, TReturn> map, object param = null, IDbTransaction transaction = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)` 
- `public static IEnumerable<TReturn> Sorgu<TFirst, TSecond, TThird, TFourth, TFifth, TSixth, TSeventh, TReturn>(bu IDbConnection cnn, string sql, Func<TFirst, TSecond, TThird, TFourth, TSixth,TFifth, TReturn> map, nesne param = null, IDbTransaction işlem = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)`
- `public static IEnumerable<TReturn> Query<TReturn>(bu IDbConnection cnn, string sql, Type[] türleri, Func<object[], TReturn> haritası, nesne param = null, IDbTransaction işlemi = null, bool buffered = true, string splitOn = "Id", int? commandTimeout = null, CommandType? commandType = null)
        `

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| cnn | Zaten açık olması gereken veritabanı bağlantınız. |
| sql | Yürütülecek komut.|
| türleri | Kayıt kümesindeki tür dizisi. |
| harita | Dönüş sonucunun oluşturulmasını işleyen `Func<>`. |
| param | Parametrelerin çıkarılacağı nesne. |
| işlem | Varsa, bu sorgunun bir parçası olduğu işlem. |
| arabelleğe alınmış | Sorgunun sonuçlarını okurken arabelleğe alınıp alınmayacağı. Bu, varsayılan değeri doğru olan isteğe bağlı bir parametredir. Arabelleğe alınan doğru olduğunda, sonuçlar bir 'Liste<T>' içinde arabelleğe alınır ve ardından birden çok numaralandırma için güvenli olan bir 'IEnumerable<T>' olarak döndürülür. Arabelleğe alınan yanlış olduğunda, bellekte tek bir satırı işlemenize izin veren okumayı bitirene kadar sql bağlantısı açık tutulur. Birden çok numaralandırma, veritabanına ek bağlantılar oluşturur. Arabelleğe alınan false, döndürülen kayıtların yalnızca çok küçük parçalarını korursanız bellek kullanımını azaltmak için oldukça verimli olsa da, sonucu hevesle gerçekleştirmeye kıyasla [büyük bir performans ek yüküne](http://stackoverflow.com/a/30493725/37055) sahiptir. Ayarlamak. Son olarak, çok sayıda eşzamanlı arabelleğe alınmamış sql bağlantınız varsa, bağlantılar kullanılabilir hale gelene kadar isteklerin engellenmesine neden olan bağlantı havuzu açlığını göz önünde bulundurmanız gerekir. |
| splitOn | İkinci nesneyi ayırmamız ve okumamız gereken Alan (varsayılan: id). Bir kayıtta 1'den fazla tür bulunduğunda, bu virgülle ayrılmış bir liste olabilir. |
| komutZaman aşımı | Komut yürütme zaman aşımından önceki saniye sayısı. |
| komutTürü | Depolanmış bir işlem mi yoksa toplu iş mi? |

## Basit çoklu tablo eşleme
Bir Person sınıfını doldurması gereken kalan atlılarla ilgili bir sorgumuz olduğunu varsayalım.

| İsim | Doğdu | ikamet |
|-----------------|------|------------------------ --|
| Daniel Dennett | 1942 | Amerika Birleşik Devletleri |
| Sam Harris | 1967 | Amerika Birleşik Devletleri |
| Richard Dawkins | 1941 | Birleşik Krallık |

    public class Person
    {
        public string Name { get; set; }
        public int Born { get; set; }
        public Country Residience { get; set; }
    }

    public class Country
    {
        public string Residence { get; set; }
    }

Döndürülen örneği oluşturmak için kullanılabilecek bir "Func<>" alan bir aşırı yük "Sorgu<>" kullanarak, kişi sınıfını ve Residence özelliğini Country örneğiyle doldurabiliriz. `Func<>`, son genel argüman her zaman dönüş tipi olmak üzere 7 giriş tipi alabilir.

    var sql = @"SELECT 'Daniel Dennett' AS Name, 1942 AS Born, 'United States of America' AS Residence
    UNION ALL SELECT 'Sam Harris' AS Name, 1967 AS Born, 'United States of America' AS Residence
    UNION ALL SELECT 'Richard Dawkins' AS Name, 1941 AS Born, 'United Kingdom' AS Residence";

    var result = connection.Query<Person, Country, Person>(sql, (person, country) => {
            if(country == null)
            {
                country = new Country { Residence = "" };
            }
            person.Residience = country;
            return person;
        }, 
        splitOn: "Residence");

> Doldurulacak bir sonraki sınıf türünün 1. sütunu olan `splitOn: "Residence"` argümanının kullanımına dikkat edin (bu durumda `Country`). Dapper otomatik olarak ayrılmak için *Id* adlı bir sütun arayacaktır, ancak bir sütun bulamazsa ve `splitOn` sağlanmazsa, yardımcı bir mesajla birlikte bir `System.ArgumentException` atılır. Bu nedenle, isteğe bağlı olmasına rağmen, genellikle bir "splitOn" değeri sağlamanız gerekir.


## Bire çok eşleme
Bire çok ilişkisini içeren daha karmaşık bir örneğe bakalım. Sorgumuz şimdi yinelenen veriler içeren birden çok satır içerecek ve bunu ele almamız gerekecek. Bunu bir kapanışta bir arama ile yapıyoruz.

Sorgu, örnek sınıflarda olduğu gibi biraz değişir.

| Kimlik | İsim | Doğdu | Ülke Kimliği | ÜlkeAdı | Kitap Kimliği | KitapAdı |
|----|-----------------|------|-----------|------- -------------------|--------|--------------------- ---------------------------------|
| 1 | Daniel Dennett | 1942 | 1 | Amerika Birleşik Devletleri | 1 | beyin fırtınaları |
| 1 | Daniel Dennett | 1942 | 1 | Amerika Birleşik Devletleri | 2 | Dirsek Odası |
| 2 | Sam Harris | 1967 | 1 | Amerika Birleşik Devletleri | 3 | Ahlaki Peyzaj |
| 2 | Sam Harris | 1967 | 1 | Amerika Birleşik Devletleri | 4 | Uyanış: Dinsiz Maneviyat Rehberi |
| 3 | Richard Dawkins | 1941 | 2 | Birleşik Krallık | 5 | Gerçekliğin Büyüsü: Neyin Gerçekten Doğru Olduğunu Nasıl Biliyoruz |
| 3 | Richard Dawkins | 1941 | 2 | Birleşik Krallık | 6 | Merak İştahı: Bir Bilim Adamının Yaratılışı |


    public class Person
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public int Born { get; set; }
        public Country Residience { get; set; }
        public ICollection<Book> Books { get; set; }
    }

    public class Country
    {
        public int CountryId { get; set; }
        public string CountryName { get; set; }
    }

    public class Book
    {
        public int BookId { get; set; }
        public string BookName { get; set; }
    }

`remainingHorsemen` sözlüğü, kişi nesnelerinin tamamen gerçekleştirilmiş örnekleriyle doldurulacaktır. Sorgu sonucunun her satırı için lambda bağımsız değişkenlerinde tanımlanan türlerin örneklerinin eşlenmiş değerleri iletilir ve bununla nasıl başa çıkacağınız size kalmış.
              
                var sql = @"SELECT 1 AS Id, 'Daniel Dennett' AS Name, 1942 AS Born, 1 AS CountryId, 'United States of America' AS CountryName, 1 AS BookId, 'Brainstorms' AS BookName
    UNION ALL SELECT 1 AS Id, 'Daniel Dennett' AS Name, 1942 AS Born, 1 AS CountryId, 'United States of America' AS CountryName, 2 AS BookId, 'Elbow Room' AS BookName
    UNION ALL SELECT 2 AS Id, 'Sam Harris' AS Name, 1967 AS Born, 1 AS CountryId,  'United States of America' AS CountryName, 3 AS BookId, 'The Moral Landscape' AS BookName
    UNION ALL SELECT 2 AS Id, 'Sam Harris' AS Name, 1967 AS Born, 1 AS CountryId,  'United States of America' AS CountryName, 4 AS BookId, 'Waking Up: A Guide to Spirituality Without Religion' AS BookName
    UNION ALL SELECT 3 AS Id, 'Richard Dawkins' AS Name, 1941 AS Born, 2 AS CountryId,  'United Kingdom' AS CountryName, 5 AS BookId, 'The Magic of Reality: How We Know What`s Really True' AS BookName
    UNION ALL SELECT 3 AS Id, 'Richard Dawkins' AS Name, 1941 AS Born, 2 AS CountryId,  'United Kingdom' AS CountryName, 6 AS BookId, 'An Appetite for Wonder: The Making of a Scientist' AS BookName";

    var remainingHorsemen = new Dictionary<int, Person>();
    connection.Query<Person, Country, Book, Person>(sql, (person, country, book) => {
        //person
        Person personEntity;
        //trip
        if (!remainingHorsemen.TryGetValue(person.Id, out personEntity))
        {
            remainingHorsemen.Add(person.Id, personEntity = person);
        }
    
        //country
        if(personEntity.Residience == null)
        {
            if (country == null)
            {
                country = new Country { CountryName = "" };
            }
            personEntity.Residience = country;
        }                    
    
        //books
        if(personEntity.Books == null)
        {
            personEntity.Books = new List<Book>();
        }
    
        if (book != null)
        {
            if (!personEntity.Books.Any(x => x.BookId == book.BookId))
            {
                personEntity.Books.Add(book);
            }
        }
    
        return personEntity;
    }, 
    splitOn: "CountryId,BookId");

> 'splitOn' argümanının, sonraki türün ilk sütunlarının virgülle ayrılmış bir listesi olduğuna dikkat edin.


## Özel Eşlemeler
Sorgu sütun adları sınıflarınızla eşleşmiyorsa, türler için eşlemeler ayarlayabilirsiniz. Bu örnek, özel bir eşlemenin yanı sıra 'System.Data.Linq.Mapping.ColumnAttribute' kullanılarak eşlemeyi gösterir.

> Eşlemelerin tür başına yalnızca bir kez ayarlanması gerekir, bu nedenle bunları uygulama başlangıcında veya yalnızca bir kez başlatılacakları başka bir yerde ayarlayın.

Bire çok örnekle aynı sorguyu tekrar varsayarsak ve sınıflar aşağıdaki gibi daha iyi adlara doğru yeniden düzenlenir:


    public class Person
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public int Born { get; set; }
        public Country Residience { get; set; }
        public ICollection<Book> Books { get; set; }
    }

    public class Country
    {
        [System.Data.Linq.Mapping.Column(Name = "CountryId")]
        public int Id { get; set; }

        [System.Data.Linq.Mapping.Column(Name = "CountryName")]
        public string Name { get; set; }
    }

    public class Book
    {
        public int Id { get; set; }

        public string Name { get; set; }
    }

>"Kitap"ın "ColumnAttribute"a nasıl güvenmediğine dikkat edin, ancak "if" ifadesini korumamız gerekir

Şimdi bu eşleme kodunu uygulamanızda yalnızca bir kez yürütüleceği bir yere yerleştirin:

    Dapper.SqlMapper.SetTypeMap(
        typeof(Country),
        new CustomPropertyTypeMap(
            typeof(Country),
            (type, columnName) =>
                type.GetProperties().FirstOrDefault(prop =>
                    prop.GetCustomAttributes(false)
                        .OfType<System.Data.Linq.Mapping.ColumnAttribute>()
                        .Any(attr => attr.Name == columnName)))
    );


    var bookMap = new CustomPropertyTypeMap(
        typeof(Book),
        (type, columnName) =>
        {
            if(columnName == "BookId")
            {
                return type.GetProperty("Id");
            }

            if (columnName == "BookName")
            {
                return type.GetProperty("Name");
            }

            throw new InvalidOperationException($"No matching mapping for {columnName}");
        }        
    );
    Dapper.SqlMapper.SetTypeMap(typeof(Book), bookMap);

Ardından sorgu, önceki `Sorgu<>` örneklerinden herhangi biri kullanılarak yürütülür.

Eşlemeleri eklemenin daha basit bir yolu [bu yanıt][1] içinde gösterilmiştir.


[1]: http://stackoverflow.com/a/12615036/2613363

## 7'den fazla türü eşleme
Bazen eşlediğiniz türlerin sayısı, yapıyı yapan İşlev<> tarafından sağlanan 7'yi aşıyor.

Genel tür argüman girdileriyle `Sorgu<>` kullanmak yerine, eşlenecek türleri bir dizi olarak ve ardından eşleme işlevini sağlayacağız. İlk manuel ayar ve değerlerin dökümü dışında, fonksiyonun geri kalanı değişmez.

              
                var sql = @"SELECT 1 AS Id, 'Daniel Dennett' AS Name, 1942 AS Born, 1 AS CountryId, 'United States of America' AS CountryName, 1 AS BookId, 'Brainstorms' AS BookName
    UNION ALL SELECT 1 AS Id, 'Daniel Dennett' AS Name, 1942 AS Born, 1 AS CountryId, 'United States of America' AS CountryName, 2 AS BookId, 'Elbow Room' AS BookName
    UNION ALL SELECT 2 AS Id, 'Sam Harris' AS Name, 1967 AS Born, 1 AS CountryId,  'United States of America' AS CountryName, 3 AS BookId, 'The Moral Landscape' AS BookName
    UNION ALL SELECT 2 AS Id, 'Sam Harris' AS Name, 1967 AS Born, 1 AS CountryId,  'United States of America' AS CountryName, 4 AS BookId, 'Waking Up: A Guide to Spirituality Without Religion' AS BookName
    UNION ALL SELECT 3 AS Id, 'Richard Dawkins' AS Name, 1941 AS Born, 2 AS CountryId,  'United Kingdom' AS CountryName, 5 AS BookId, 'The Magic of Reality: How We Know What`s Really True' AS BookName
    UNION ALL SELECT 3 AS Id, 'Richard Dawkins' AS Name, 1941 AS Born, 2 AS CountryId,  'United Kingdom' AS CountryName, 6 AS BookId, 'An Appetite for Wonder: The Making of a Scientist' AS BookName";

    var remainingHorsemen = new Dictionary<int, Person>();
    connection.Query<Person>(sql,
        new[]
        {
            typeof(Person),
            typeof(Country),
            typeof(Book)
        }
        , obj => {
    
            Person person = obj[0] as Person;
            Country country = obj[1] as Country;
            Book book = obj[2] as Book;
    
            //person
            Person personEntity;
            //trip
            if (!remainingHorsemen.TryGetValue(person.Id, out personEntity))
            {
                remainingHorsemen.Add(person.Id, personEntity = person);
            }
    
            //country
            if(personEntity.Residience == null)
            {
                if (country == null)
                {
                    country = new Country { CountryName = "" };
                }
                personEntity.Residience = country;
            }                    
    
            //books
            if(personEntity.Books == null)
            {
                personEntity.Books = new List<Book>();
            }
    
            if (book != null)
            {
                if (!personEntity.Books.Any(x => x.BookId == book.BookId))
                {
                    personEntity.Books.Add(book);
                }
            }
    
            return personEntity;
    },
    splitOn: "CountryId,BookId");




