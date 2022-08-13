---
title: "Veritabanlarına Erişim"
slug: "veritabanlarna-erisim"
draft: false
images: []
weight: 9946
type: docs
toc: true
---

## Bağlantı Dizeleri
Bağlantı Dizesi, belirli bir veri kaynağı hakkındaki bilgileri ve kimlik bilgilerini, konumları ve diğer bilgileri depolayarak ona nasıl bağlanılacağını belirten bir dizedir.

    Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=myPassword;

**Bağlantı Dizinizi Kaydetme**
---

Tipik olarak, bir yapılandırma dosyasında (ASP.NET uygulamalarında "app.config" veya "web.config" gibi) bir bağlantı dizesi depolanır. Aşağıda, bu dosyalardan birinde yerel bir bağlantının nasıl görünebileceğine ilişkin bir örnek verilmiştir:

    <connectionStrings> 
       <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=True;"/> 
    </connectionStrings>

    <connectionStrings> 
       <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=SSPI;"/> 
    </connectionStrings>

Bu, uygulamanızın bağlantı dizesine 'WidgetsContext' aracılığıyla programlı olarak erişmesine olanak tanır. Hem 'Integrated Security=SSPI' hem de 'Integrated Security=True' aynı işlevi yerine getirse de; 'Integrated Security=true' ile kullanıldığında bir istisna attığı hem SQLClient hem de OleDB sağlayıcısıyla çalıştığı için 'Integrated Security=SSPI' tercih edilir. OleDb sağlayıcısı.

**Farklı Sağlayıcılar için Farklı Bağlantılar**
----

Her veri sağlayıcının (SQL Server, MySQL, Azure, vb.) tümü, bağlantı dizeleri için kendi sözdizimi biçimine sahiptir ve farklı kullanılabilir özellikler sunar. [ConnectionStrings.com](https://www.connectionstrings.com/), sizinkinin nasıl görünmesi gerektiğinden emin değilseniz inanılmaz derecede faydalı bir kaynaktır.


## Entity Framework Bağlantıları
Entity Framework, temel veritabanlarıyla etkileşim kurmak için kullanılan soyutlama sınıflarını "DbContext" gibi sınıflar biçiminde sunar. Bu bağlamlar genellikle, sorgulanabilecek mevcut koleksiyonları ortaya çıkaran "DbSet<T>" özelliklerinden oluşur:

    public class ExampleContext: DbContext 
    { 
        public virtual DbSet<Widgets> Widgets { get; set; } 
    }

DbContext'in kendisi veritabanlarıyla bağlantı kurmayı yönetecek ve bağlantıların nasıl kurulacağını belirlemek için genellikle bir konfigürasyondan uygun Bağlantı Dizesi verilerini okuyacaktır:

    public class ExampleContext: DbContext 
    { 
        // The parameter being passed in to the base constructor indicates the name of the 
        // connection string
        public ExampleContext() : base("ExampleContextEntities")
        {
        }
    
        public virtual DbSet<Widgets> Widgets { get; set; } 
    }

**Entity Framework Sorgularını Yürütme**
----

Aslında bir Entity Framework sorgusu yürütmek oldukça kolay olabilir ve yalnızca bağlamın bir örneğini oluşturmanızı ve ardından verilerinizi çekmek veya bunlara erişmek için mevcut özellikleri kullanmanızı gerektirir.

    using(var context = new ExampleContext())
    {
          // Retrieve all of the Widgets in your database
          var data = context.Widgets.ToList();
    }

Entity Framework, değişiklikleri veritabanına iletmek için yalnızca "SaveChanges()" yöntemini çağırarak veritabanınızdaki güncelleme girişlerini işlemek için kullanılabilecek kapsamlı bir değişiklik izleme sistemi de sağlar:

    using(var context = new ExampleContext())
    {
          // Grab the widget you wish to update
          var widget = context.Widgets.Find(w => w.Id == id);
          // If it exists, update it
          if(widget != null)
          {
               // Update your widget and save your changes
               widget.Updated = DateTime.UtcNow;
               context.SaveChanges();
          }
    }

## ADO.NET Bağlantıları
ADO.NET Bağlantıları, bir C# uygulamasından bir veritabanına bağlanmanın en basit yollarından biridir. Bir sağlayıcının kullanımına ve sorgu yapmak için veritabanınıza işaret eden bir bağlantı dizesine güvenirler.

**Ortak Veri Sağlayıcı Sınıfları**
----

Aşağıdakilerin çoğu, veritabanlarını ve ilgili ad alanlarını sorgulamak için yaygın olarak kullanılan sınıflardır:

- `System.Data.SqlClient`ten `SqlConnection`,`SqlCommand`,`SqlDataReader`
- "System.Data.OleDb"den "OleDbConnection", "OleDbCommand", "OleDbDataReader"
- `MySqlConnection`, `MySqlCommand`, `MySqlDbDataReader` [`MySql.Data`](http://dev.mysql.com/downloads/file/?id=13427)

Bunların tümü, C# aracılığıyla verilere erişmek için yaygın olarak kullanılır ve veri merkezli uygulamalar oluştururken yaygın olarak karşılaşılır. Aynı 'FooConnection', 'FooCommand', 'FooDataReader' sınıflarını uygulayan belirtilmeyen diğer birçok sınıfın da aynı şekilde davranması beklenebilir.

**ADO.NET Bağlantıları için Ortak Erişim Modeli**
----

Bir ADO.NET bağlantısı üzerinden verilerinize erişirken kullanılabilecek yaygın bir kalıp aşağıdaki gibi görünebilir:

    // This scopes the connection (your specific class may vary)
    using(var connection = new SqlConnection("{your-connection-string}")
    {
        // Build your query
        var query = "SELECT * FROM YourTable WHERE Property = @property");
        // Scope your command to execute
        using(var command = new SqlCommand(query, connection))
        {
             // Open your connection
             connection.Open();

             // Add your parameters here if necessary

             // Execute your query as a reader (again scoped with a using statement)
             using(var reader = command.ExecuteReader())
             {
                   // Iterate through your results here
             }
        }
    }

Veya sadece basit bir güncelleme yapıyorsanız ve bir okuyucuya ihtiyacınız yoksa, aynı temel konsept geçerli olacaktır:

    using(var connection = new SqlConnection("{your-connection-string}"))
    {
         var query = "UPDATE YourTable SET Property = Value WHERE Foo = @foo";
         using(var command = new SqlCommand(query,connection))
         {
              connection.Open();
              
              // Add parameters here
              
              // Perform your update
              command.ExecuteNonQuery();
         }
    }


Hatta bir dizi ortak arayüze karşı program yapabilir ve sağlayıcıya özel sınıflar hakkında endişelenmenize gerek kalmaz. ADO.NET tarafından sağlanan temel arabirimler şunlardır:

- IDbConnection - veritabanı bağlantılarını yönetmek için
- IDbCommand - SQL komutlarını çalıştırmak için
- IDbTransaction - işlemleri yönetmek için
- IDataReader - bir komut tarafından döndürülen verileri okumak için
- IDataAdapter - veri kümelerine ve veri kümelerinden veri kanalize etmek için


    var connectionString = "{your-connection-string}";
    var providerName = "{System.Data.SqlClient}"; //for Oracle use "Oracle.ManagedDataAccess.Client"
    //most likely you will get the above two from ConnectionStringSettings object

    var factory = DbProviderFactories.GetFactory(providerName);

    using(var connection = new factory.CreateConnection()) {
        connection.ConnectionString = connectionString;
        connection.Open();

        using(var command = new connection.CreateCommand()) {
            command.CommandText = "{sql-query}";    //this needs to be tailored for each database system
    
            using(var reader = command.ExecuteReader()) {
                while(reader.Read()) {
                    ...
                }
            }
        }
    }

