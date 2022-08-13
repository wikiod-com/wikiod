---
title: "ADO.NET"
slug: "adonet"
draft: false
images: []
weight: 9877
type: docs
toc: true
---

ADO(ActiveX Data Objects).Net, bileşenleri aracılığıyla SQL Server, Oracle, XML gibi veri kaynaklarına erişim sağlayan Microsoft tarafından sağlanan bir araçtır. .Net ön uç uygulamaları, uygun ayrıcalıklarla ADO.Net aracılığıyla bir veri kaynağına bağlandıklarında verileri alabilir, oluşturabilir ve işleyebilir.

ADO.Net bağlantısız bir mimari sağlar. Bir veritabanıyla etkileşim kurmak güvenli bir yaklaşımdır, çünkü bağlantının tüm oturum boyunca sürdürülmesi gerekmez.

**SQL'leri `Parameters.AddWithValue` ile parametreleştirme hakkında bir not:** `AddWithValue` asla iyi bir başlangıç ​​noktası değildir. Bu yöntem, aktarılanlardan verilerin türünü çıkarmaya dayanır. Bununla, dönüşümün sorgunuzun [bir dizin kullanmasını] engellediği bir duruma düşebilirsiniz(http://stackoverflow.com/q/799584 /87698). "char"/"varchar" ("n" öncesinde olmadan) veya "date" gibi bazı SQL Server veri türlerinin karşılık gelen bir .NET veri türüne sahip olmadığına dikkat edin. Bu gibi durumlarda, bunun yerine doğru veri türüyle "Ekle" kullanılmalıdır](http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-already /).

## En İyi Uygulamalar - Sql İfadelerini Yürütme
    public void SaveNewEmployee(Employee newEmployee)
    {
        // best practice - wrap all database connections in a using block so they are always closed & disposed even in the event of an Exception
        // best practice - retrieve the connection string by name from the app.config or web.config (depending on the application type) (note, this requires an assembly reference to System.configuration)
        using(SqlConnection con = new SqlConnection(System.Configuration.ConfigurationManager.ConnectionStrings["MyConnectionName"].ConnectionString))
        {
            // best practice - use column names in your INSERT statement so you are not dependent on the sql schema column order
            // best practice - always use parameters to avoid sql injection attacks and errors if malformed text is used like including a single quote which is the sql equivalent of escaping or starting a string (varchar/nvarchar)
            // best practice - give your parameters meaningful names just like you do variables in your code
            using(SqlCommand sc = new SqlCommand("INSERT INTO employee (FirstName, LastName, DateOfBirth /*etc*/) VALUES (@firstName, @lastName, @dateOfBirth /*etc*/)", con))
            {
                // best practice - always specify the database data type of the column you are using
                // best practice - check for valid values in your code and/or use a database constraint, if inserting NULL then use System.DbNull.Value
                sc.Parameters.Add(new SqlParameter("@firstName", SqlDbType.VarChar, 200){Value = newEmployee.FirstName ?? (object) System.DBNull.Value});
                sc.Parameters.Add(new SqlParameter("@lastName", SqlDbType.VarChar, 200){Value = newEmployee.LastName ?? (object) System.DBNull.Value});
    
                // best practice - always use the correct types when specifying your parameters, Value is assigned to a DateTime instance and not a string representation of a Date
                sc.Parameters.Add(new SqlParameter("@dateOfBirth", SqlDbType.Date){ Value = newEmployee.DateOfBirth });
    
                // best practice - open your connection as late as possible unless you need to verify that the database connection is valid and wont fail and the proceeding code execution takes a long time (not the case here)
                con.Open();
                sc.ExecuteNonQuery();
            }
    
            // the end of the using block will close and dispose the SqlConnection
            // best practice - end the using block as soon as possible to release the database connection
        }
    }

    // supporting class used as parameter for example
    public class Employee
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public DateTime DateOfBirth { get; set; }
    }



----------

## [ADO.NET][1] ile çalışmak için en iyi uygulama
* Temel kural, minimum süre için bağlantı açmaktır. Prosedür yürütmeniz bittiğinde bağlantıyı açıkça kapatın, bu, bağlantı nesnesini bağlantı havuzuna geri döndürür. Varsayılan bağlantı havuzu maksimum boyutu = 100. Bağlantı havuzu oluşturma, SQL Server'a fiziksel bağlantının performansını geliştirdiğinden.[SQL Server'da Bağlantı Havuzu Oluşturma][2]
* Tüm veritabanı bağlantılarını bir kullanım bloğuna sarın, böylece bir İstisna durumunda bile her zaman kapalı ve atılırlar. İfadeleri kullanma hakkında daha fazla bilgi için [Bildirimi kullanma (C# Referansı)][3] bölümüne bakın.
* Bağlantı dizelerini app.config veya web.config'den ada göre alın (uygulama türüne bağlı olarak)
* Bu, `System.configuration` için bir derleme referansı gerektirir
* Yapılandırma dosyanızı nasıl yapılandıracağınızla ilgili ek bilgi için [Bağlantı Dizileri ve Yapılandırma Dosyaları][4]'na bakın.
* Gelen değerler için her zaman parametreleri kullanın.
* [sql enjeksiyonu][5] saldırılarından kaçının
* Bir dizeden kaçmanın veya bir dize başlatmanın (varchar/nvarchar) sql eşdeğeri olan tek bir alıntı eklemek gibi hatalı biçimlendirilmiş metin kullanılırsa hatalardan kaçının
* Veri tabanı sağlayıcısının sorgu planlarını yeniden kullanmasına izin vermek (tüm veri tabanı sağlayıcıları tarafından desteklenmez), bu da verimliliği artırır
* Parametrelerle çalışırken
* Sql parametrelerinin türü ve boyutu uyumsuzluğu, ekleme/güncelleme/seçme hatasının yaygın bir nedenidir.
* Sql parametrelerinize kodunuzda değişkenler yaptığınız gibi anlamlı isimler verin
* Kullanmakta olduğunuz sütunun veritabanı veri türünü belirtin, bu, beklenmedik sonuçlara yol açabilecek yanlış parametre türlerinin kullanılmamasını sağlar
* Gelen parametrelerinizi komuta geçirmeden önce doğrulayın (dediği gibi, "[çöp içeri, çöp dışarı](https://en.wikipedia.org/wiki/Garbage_in,_garbage_out)"). Yığında gelen değerleri mümkün olduğunca erken doğrulayın
* Parametre değerlerinizi atarken doğru türleri kullanın, örneğin: bir DateTime'ın dize değerini atmayın, bunun yerine parametrenin değerine gerçek bir DateTime örneği atayın
* Dize tipi parametrelerin [boyutunu](https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparameter.size(v=vs.110).aspx) belirtin. Bunun nedeni, parametreler * ve * boyutunda eşleşirse SQL Server'ın yürütme planlarını yeniden kullanabilmesidir. MAKS için -1 kullanın
* [AddWithValue][6] yöntemini kullanmayın, bunun ana nedeni, gerektiğinde parametre türünü veya kesinlik/ölçeği belirtmeyi unutmanın çok kolay olmasıdır. Ek bilgi için bkz. [AdWithValue kullanmayı zaten bırakabilir miyiz?][7]
* Veritabanı bağlantılarını kullanırken
* Bağlantıyı olabildiğince geç açın ve bir an önce kapatın. Bu, herhangi bir harici kaynakla çalışırken genel bir kılavuzdur.
* Veritabanı bağlantı örneklerini asla paylaşmayın (örnek: tek bir ana bilgisayara sahip olmak, 'SqlConnection' türünde paylaşılan bir örnek). Kodunuzun gerektiğinde her zaman yeni bir veritabanı bağlantısı örneği oluşturmasını sağlayın ve ardından çağıran kodun onu elden çıkarmasını ve bittiğinde "atmasını" sağlayın. Bunun nedeni
    * Most database providers have some sort of connection pooling so creating new managed connections is cheap
    * It eliminates any future errors if the code starts working with multiple threads


[1]:https://msdn.microsoft.com/en-us/library/h43ks021(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/8xx3tyca(v=vs.110).aspx
[3]:https://msdn.microsoft.com/en-us/library/yh598w02.aspx
[4]:https://msdn.microsoft.com/en-us/library/ms254494(v=vs.110).aspx
[5]:https://en.wikipedia.org/wiki/SQL_injection
[6]:https://msdn.microsoft.com/en-us/library/system.data.sqlclient.sqlparametercollection.addwithvalue(v=vs.110).aspx
[7]:http://blogs.msmvps.com/jcoehoorn/blog/2014/05/12/can-we-stop-using-addwithvalue-already/

## SQL ifadelerini komut olarak yürütme
    // Uses Windows authentication. Replace the Trusted_Connection parameter with
    // User Id=...;Password=...; to use SQL Server authentication instead. You may
    // want to find the appropriate connection string for your server.
    string connectionString = @"Server=myServer\myInstance;Database=myDataBase;Trusted_Connection=True;"

    string sql = "INSERT INTO myTable (myDateTimeField, myIntField) " +
        "VALUES (@someDateTime, @someInt);";

    // Most ADO.NET objects are disposable and, thus, require the using keyword.
    using (var connection = new SqlConnection(connectionString))
    using (var command = new SqlCommand(sql, connection))
    {
        // Use parameters instead of string concatenation to add user-supplied
        // values to avoid SQL injection and formatting issues. Explicitly supply datatype.

        // System.Data.SqlDbType is an enumeration. See Note1
        command.Parameters.Add("@someDateTime", SqlDbType.DateTime).Value = myDateTimeVariable;
        command.Parameters.Add("@someInt", SqlDbType.Int).Value = myInt32Variable;

        // Execute the SQL statement. Use ExecuteScalar and ExecuteReader instead
        // for query that return results (or see the more specific examples, once
        // those have been added).

        connection.Open();
        command.ExecuteNonQuery();
    }

***Not 1:*** MSFT SQL Server'a özgü varyasyon için lütfen [SqlDbType Enumeration][1]'e bakın.

***Not 2:*** MySQL'e özgü varyasyon için lütfen [MySqlDbType Enumeration][2]'ye bakın.


[1]: https://msdn.microsoft.com/en-us/library/system.data.sqldbtype(v=vs.110).aspx
[2]: https://dev.mysql.com/doc/dev/connector-net/html/T_MySql_Data_MySqlClient_MySqlDbType.htm

## Satıcıya özel sınıfları soyutlamak için ortak arayüzleri kullanma
    var providerName = "System.Data.SqlClient";    //Oracle.ManagedDataAccess.Client, IBM.Data.DB2
    var connectionString = "{your-connection-string}";
    //you will probably get the above two values in the ConnectionStringSettings object from .config file

    var factory = DbProviderFactories.GetFactory(providerName);
    using(var connection = factory.CreateConnection()) {    //IDbConnection
        connection.ConnectionString = connectionString;
        connection.Open();
        
        using(var command = connection.CreateCommand()) {    //IDbCommand
            command.CommandText = "{query}";
            
            using(var reader = command.ExecuteReader()) {    //IDataReader
                while(reader.Read()) {
                    ...
                }
            }
        }
    }

