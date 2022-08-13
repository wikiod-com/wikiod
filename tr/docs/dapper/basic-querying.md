---
title: "Temel Sorgulama"
slug: "temel-sorgulama"
draft: false
images: []
weight: 9748
type: docs
toc: true
---

## Sözdizimi
- genel statik IEnumerable&lt;T&gt; Sorgu&lt;T&gt;(bu IDbConnection cnn, string sql, nesne param = null, SqlTransaction işlem = null, bool arabelleğe alınmış = true)
- genel statik IEnumerable&lt;dinamik&gt; Sorgu (bu IDbConnection cnn, string sql, nesne param = null, SqlTransaction işlemi = null, bool arabelleğe alınmış = true)

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| cnn | Zaten açık olması gereken veritabanı bağlantınız. |
| sql | Yürütülecek komut. |
| param | Parametrelerin çıkarılacağı nesne. |
| işlem | Varsa, bu sorgunun bir parçası olduğu işlem. |
| arabelleğe alınmış | Sorgunun sonuçlarını okurken arabelleğe alınıp alınmayacağı. Bu, varsayılan değeri doğru olan isteğe bağlı bir parametredir. Arabelleğe alınan doğru olduğunda, sonuçlar bir 'Liste<T>' içinde arabelleğe alınır ve ardından birden çok numaralandırma için güvenli olan bir 'IEnumerable<T>' olarak döndürülür. Arabelleğe alınan yanlış olduğunda, bellekte tek bir satırı işlemenize izin veren okumayı bitirene kadar sql bağlantısı açık tutulur. Birden çok numaralandırma, veritabanına ek bağlantılar oluşturur. Arabelleğe alınan false, döndürülen kayıtların yalnızca çok küçük parçalarını korursanız bellek kullanımını azaltmak için oldukça verimli olsa da, sonucu hevesle gerçekleştirmeye kıyasla [büyük bir performans ek yüküne](http://stackoverflow.com/a/30493725/37055) sahiptir. Ayarlamak. Son olarak, çok sayıda eşzamanlı arabelleğe alınmamış sql bağlantınız varsa, bağlantılar kullanılabilir hale gelene kadar isteklerin engellenmesine neden olan bağlantı havuzu açlığını göz önünde bulundurmanız gerekir. |


## Statik bir tür için sorgulama
Derleme zamanında bilinen türler için 'Sorgu<T>' ile genel bir parametre kullanın.

    public class Dog
    {
        public int? Age { get; set; }
        public Guid Id { get; set; }
        public string Name { get; set; }
        public float? Weight { get; set; }
    
        public int IgnoredProperty { get { return 1; } }
    }    
    
    //
    IDBConnection db = /* ... */;

    var @params = new { age = 3 };
    var sql = "SELECT * FROM dbo.Dogs WHERE Age = @age";

    IEnumerable<Dog> dogs = db.Query<Dog>(sql, @params);

## Dinamik türler için sorgulama
Genel türün dışında bırakırsanız dinamik olarak da sorgulayabilirsiniz.
    
    IDBConnection db = /* ... */;
    IEnumerable<dynamic> result = db.Query("SELECT 1 as A, 2 as B");

    var first = result.First();
    int a = (int)first.A; // 1
    int b = (int)first.B; // 2

## Dinamik Parametrelerle Sorgulama
    var color = "Black";
    var age = 4;

    var query = "Select * from Cats where Color = :Color and Age > :Age";
    var dynamicParameters = new DynamicParameters();
    dynamicParameters.Add("Color", color);
    dynamicParameters.Add("Age", age);

    using (var connection = new SqlConnection(/* Your Connection String Here */))
    {
        IEnumerable<dynamic> results = connection.Query(query, dynamicParameters);
    }

