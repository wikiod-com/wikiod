---
title: "Parametre Sözdizimi Referansı"
slug: "parametre-sozdizimi-referans"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| `bu cnn` | Temel veritabanı bağlantısı - "bu", bir uzantı yöntemini belirtir; bağlantının açık olması gerekmez - açık değilse otomatik olarak açılır ve kapanır.
| `<T>` / `Tür` | (isteğe bağlı) Döndürülecek nesnenin türü; genel olmayan / "Tür" olmayan API kullanılırsa, sorgudan döndürülen sütun adı başına adlandırılan bir özelliği simüle ederek satır başına bir "dinamik" nesne döndürülür (bu "dinamik" nesne ayrıca "Kimlik<dize, nesne >`).
| `sql` | Çalıştırılacak SQL
| "param" | (isteğe bağlı) Dahil edilecek parametreler.
| "işlem" | (isteğe bağlı) Komutla ilişkilendirilecek veritabanı işlemi
| "arabelleğe alınmış" | (isteğe bağlı) Canlı okuyucu üzerinde açık bir "IEnumerable" öğesinin gösterilmesine karşı verilerin bir listede önceden tüketilip tüketilmeyeceği (varsayılan)
| "komutZaman aşımı" | (isteğe bağlı) Komutta kullanılacak zaman aşımı; belirtilmemişse, `SqlMapper.Settings.CommandTimeout` varsayılır (belirtilmişse)
| "komutTürü" | Gerçekleştirilen komutun türü; varsayılan olarak "Komut Metni"

Parametreleri ifade etmek için sözdizimi RDBMS arasında değişir. Yukarıdaki tüm örnekler SQL Server sözdizimini kullanır, yani `@foo`; ancak, "?foo" ve ":foo" da düzgün çalışmalıdır.

## Değer Satır İçi
Bazen bir parametrenin uygunluğu (bakım ve ifade açısından), onu bir parametre olarak ele almak için performans maliyetinden daha ağır basabilir. Örneğin, sayfa boyutu bir yapılandırma ayarıyla sabitlendiğinde. Veya bir durum değeri bir "enum" değeriyle eşleştirilir. Düşünmek:

    var orders = connection.Query<Order>(@"
    select top (@count) * -- these brackets are an oddity of SQL Server
    from Orders
    where CustomerId = @customerId
    and Status = @open", new { customerId, count = PageSize, open = OrderStatus.Open });

Buradaki tek *gerçek* parametre "customerId"dir - diğer ikisi aslında değişmeyecek olan sözde parametrelerdir. Bunları sabit olarak algılarsa, RDBMS genellikle daha iyi bir iş çıkarabilir. Dapper'ın bunun için özel bir sözdizimi vardır - `@name` yerine `{=name}` - bu *yalnızca* sayısal türler için geçerlidir. (Bu, SQL enjeksiyonundan kaynaklanan herhangi bir saldırı yüzeyini en aza indirir). Bir örnek aşağıdaki gibidir:

    var orders = connection.Query<Order>(@"
    select top {=count} *
    from Orders
    where CustomerId = @customerId
    and Status = {=open}", new { customerId, count = PageSize, open = OrderStatus.Open });

Dapper, SQL'i yayınlamadan önce değerleri değişmez değerlerle değiştirir, böylece RDBMS aslında şöyle bir şey görür:

    select top 10 *
    from Orders
    where CustomerId = @customerId
    and Status = 3

Bu, özellikle RDBMS sistemlerinin yalnızca daha iyi kararlar almasına değil, aynı zamanda gerçek parametrelerin önlediği sorgu planlarını açmasına izin verirken kullanışlıdır. Örneğin, bir sütun yüklemi bir parametreye karşıysa, o sütunlarda belirli değerlere sahip filtrelenmiş bir dizin kullanılamaz. Bunun nedeni, *sonraki* sorgunun belirtilen değerlerden birinden farklı bir parametreye sahip olabilmesidir.

Değişmez değerlerle, sorgu iyileştirici, değerin gelecekteki sorgularda değişemeyeceğini bildiği için filtrelenmiş dizinleri kullanabilir.

## Temel Parametreli SQL
Dapper, tamamen parametreli SQL yoluyla en iyi uygulamayı takip etmeyi kolaylaştırır.

![Bobby Tabloları](https://imgs.xkcd.com/comics/exploits_of_a_mom.png)

Parametreler önemlidir, bu nedenle zarif, doğru olanı yapmayı kolaylaştırır. Sadece parametrelerinizi RDBMS'niz için normal şekilde ifade edin (genellikle `@foo`, `?foo` veya `:foo`) ve dapper'a * `foo`* adlı bir üyesi olan bir nesne verin. Bunu yapmanın en yaygın yolu, anonim bir türdür:

    int id = 123;
    string name = "abc";
    connection.Execute("insert [KeyLookup](Id, Name) values(@id, @name)",
        new { id, name });

Ve bu kadar. Dapper gerekli parametreleri ekleyecektir ve her şey çalışmalıdır.

Nesne Modelinizi Kullanma
---

Mevcut nesne modelinizi parametre olarak da kullanabilirsiniz:

    KeyLookup lookup = ... // some existing instance
    connection.Execute("insert [KeyLookup](Id, Name) values(@Id, @Name)", lookup);

Dapper, nesnenin hangi üyelerinin ekleneceğini belirlemek için komut metnini kullanır - genellikle "Description", "IsActive", "CreationDate" gibi gereksiz şeyler eklemez çünkü verdiğimiz komut açıkça onları içermez - bunu yapabileceği durumlar olsa da, örneğin komutunuz şunları içeriyorsa:

    // TODO - removed for now; include the @Description in the insert

Yukarıdakilerin sadece bir yorum olduğunu anlamaya çalışmaz.

Saklı Prosedürler
---

Saklı yordamların parametreleri tam olarak aynı şekilde çalışır, ancak bu zarif, neyin dahil edilmesi gerektiğini/ edilmemesi gerektiğini belirlemeye çalışamaz - mevcut her şey bir parametre olarak değerlendirilir. Bu nedenle anonim türler genellikle tercih edilir:

    connection.Execute("KeyLookupInsert", new { id, name },
        commandType: CommandType.StoredProcedure);



## Liste Genişletmeleri
Veritabanı sorgularında yaygın bir senaryo, buradaki listenin çalışma zamanında oluşturulduğu "IN (...)"dir. Çoğu RDBMS bunun için iyi bir metafordan yoksundur - ve bunun için evrensel bir *çapraz RDBMS* çözümü yoktur. Bunun yerine, zarif bir otomatik komut genişletmesi sağlar. Tek gereken, sağlanan "IEnumerable" parametre değeridir. `@foo` içeren bir komut, `(@foo0,@foo1,@foo2,@foo3)` şeklinde genişletilir (4 öğelik bir dizi için). Bunun en yaygın kullanımı "IN" olacaktır:

    int[] orderIds = ...
    var orders = connection.Query<Order>(@"
    select *
    from Orders
    where Id in @orderIds", new { orderIds });

Bu daha sonra otomatik olarak çok satırlı getirme için uygun SQL'i vermek üzere genişler:

    select *
    from Orders
    where Id in (@orderIds0, @orderIds1, @orderIds2, @orderIds3)

`@orderIds0` vb. parametreleri diziden alınan değerler olarak eklenir.
Orijinal olarak geçerli SQL olmadığı gerçeğinin, bu özelliğin yanlışlıkla kullanılmamasını sağlamak için kasıtlı olduğunu unutmayın. Bu özellik ayrıca SQL Server'daki `OPTIMIZE FOR` / `UNKNOWN` sorgu ipucuyla da doğru şekilde çalışır; Eğer kullanırsan:

    option (optimize for
        (@orderIds unknown))

bunu doğru bir şekilde genişletecek:

    option (optimize for
        (@orderIds0 unknown, @orderIds1 unknown, @orderIds2 unknown, @orderIds3 unknown))

## Birden Çok Girdi Kümesine Karşı İşlem Gerçekleştirme
Bazen aynı şeyi defalarca yapmak istersiniz. Dapper, *en dıştaki* parametre (genellikle tek bir anonim tür veya bir etki alanı modeli örneğidir) gerçekten bir "IEnumerable" dizisi olarak sağlanmışsa, bunu "Execute" yönteminde destekler. Örneğin:

    Order[] orders = ...
    // update the totals
    connection.Execute("update Orders set Total=@Total where Id=@Id", orders);

Burada zarif, verilerimiz üzerinde basit bir döngü yapıyor, aslında bizim yaptığımızla aynı:

    Order[] orders = ...
    // update the totals
    foreach(Order order in orders) {
        connection.Execute("update Orders set Total=@Total where Id=@Id", order);
    }

Bu kullanım, tüm "Birden Çok Etkin Sonuç Kümesine" açıkça yapılandırılmış bir bağlantıda "async" API ile birleştirildiğinde *özellikle* ilginç hale gelir - bu kullanımda, zarif işlemler otomatik olarak *boru hattı* yapar, böylece ödeme yapmazsınız satır başına gecikme maliyeti. Bu biraz daha karmaşık bir kullanım gerektirir,

    await connection.ExecuteAsync(
        new CommandDefinition(
            "update Orders set Total=@Total where Id=@Id", 
             orders, flags: CommandFlags.Pipelined))

Ancak, tablo değerli parametreleri de araştırmak isteyebileceğinizi unutmayın.

## Sözde Konumsal Parametreler (adlandırılmış parametreleri desteklemeyen sağlayıcılar için)
Bazı ADO.NET sağlayıcıları (en önemlisi: OleDB) *adlandırılmış* parametreleri desteklemez; parametreler bunun yerine yalnızca *konum* ile, `?` yer tutucusu ile belirtilir. Dapper bunlar için hangi üyeyi kullanacağını bilemez, bu nedenle zarif alternatif bir sözdizimine izin verir, `?foo?`; bu, diğer SQL türevlerinde '@foo' veya ':foo' ile aynı olacaktır, ancak bu şık, sorguyu yürütmeden önce parametre belirtecini tamamen '?' ile **değiştirir**.

Bu, liste genişletme gibi diğer özelliklerle birlikte çalışır, dolayısıyla aşağıdakiler geçerlidir:

    string region = "North";
    int[] users = ...
    var docs = conn.Query<Document>(@"
         select * from Documents
         where Region = ?region?
         and OwnerId in ?users?", new { region, users }).AsList();

'.region' ve '.users' üyeleri buna göre kullanılır ve verilen SQL (örneğin, 3 kullanıcı ile):

         select * from Documents
         where Region = ?
         and OwnerId in (?,?,?)

Ancak, bu özelliği kullanırken aynı parametrenin birden çok kez kullanılmasına **dapper** izin vermediğine dikkat edin; bu, aynı parametre değerini (büyük olabilir) birden çok kez eklemek zorunda kalmamak içindir. Aynı değere birden çok kez başvurmanız gerekiyorsa, bir değişken bildirmeyi düşünün, örneğin:

    declare @id int = ?id?; // now we can use @id multiple times in the SQL

Değişkenler mevcut değilse, parametrelerde yinelenen üye adları kullanabilirsiniz - bu, değerin birden çok kez gönderildiğini de açıkça gösterecektir:

    int id = 42;
    connection.Execute("... where ParentId = $id0$ ... SomethingElse = $id1$ ...",
          new { id0 = id, id1 = id });

