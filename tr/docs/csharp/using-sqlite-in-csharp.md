---
title: "SQLite'ı C#'ta Kullanmak"
slug: "sqlite-cta-kullanmak"
draft: false
images: []
weight: 9945
type: docs
toc: true
---

## C#'ta SQLite kullanarak basit CRUD oluşturma
Öncelikle uygulamamıza SQLite desteği eklememiz gerekiyor. bunu yapmanın iki yolu var

- Sisteminize uygun DLL dosyasını <a href="https://sqlite.org/download.html">SQLite indirme sayfasından</a> indirin ve ardından projeye manuel olarak ekleyin
- NuGet aracılığıyla SQLite bağımlılığı ekleyin

Bunu ikinci şekilde yapacağız

Önce Nuget menüsünü açın

[![buraya resim açıklamasını girin][1]][1]


ve **System.Data.SQLite** dosyasını arayın, seçin ve **Yükle**'ye basın

[![buraya resim açıklamasını girin][2]][2]

Kurulum ayrıca [Paket Yöneticisi Konsolu] [3] ile de yapılabilir.

    PM> Install-Package System.Data.SQLite

Veya yalnızca temel özellikler için

    PM> Install-Package System.Data.SQLite.Core 

İndirmek için bu kadar, böylece doğrudan kodlamaya geçebiliriz.

Öncelikle bu tablo ile basit bir SQLite veritabanı oluşturun ve projeye dosya olarak ekleyin

    CREATE TABLE User(
      Id INTEGER PRIMARY KEY AUTOINCREMENT,
      FirstName TEXT NOT NULL,
      LastName TEXT NOT NULL
    );

Ayrıca dosyanın **Çıktı Dizinine Kopyala** özelliğini ihtiyaçlarınıza göre **Daha yeniyse kopyala**'nın **Her zaman kopyala** olarak ayarlamayı unutmayın.

[![buraya resim açıklamasını girin][4]][4]

Veritabanımız için temel varlık olacak Kullanıcı adında bir sınıf oluşturun

    private class User
    {
        public string FirstName { get; set; }
        public string Lastname { get; set; }
    }

Sorgu yürütme için iki yöntem yazacağız, ilki veritabanına ekleme, güncelleme veya çıkarma için

    private int ExecuteWrite(string query, Dictionary<string, object> args)
    {
        int numberOfRowsAffected;

        //setup the connection to the database
        using (var con = new SQLiteConnection("Data Source=test.db"))
        {
            con.Open();
            
            //open a new command
            using (var cmd = new SQLiteCommand(query, con))
            {
                //set the arguments given in the query
                foreach (var pair in args)
                {
                    cmd.Parameters.AddWithValue(pair.Key, pair.Value);
                }

                //execute the query and get the number of row affected
                numberOfRowsAffected = cmd.ExecuteNonQuery();
            }

            return numberOfRowsAffected;
        }
    }

ve ikincisi veritabanından okumak için

    private DataTable Execute(string query)
    {
        if (string.IsNullOrEmpty(query.Trim()))
            return null;

        using (var con = new SQLiteConnection("Data Source=test.db"))
        {
            con.Open();
            using (var cmd = new SQLiteCommand(query, con))
            {
                foreach (KeyValuePair<string, object> entry in args)
                {
                    cmd.Parameters.AddWithValue(entry.Key, entry.Value);
                }

                var da = new SQLiteDataAdapter(cmd);

                var dt = new DataTable();
                da.Fill(dt);

                da.Dispose();
                return dt;
            }
        }
    }


Şimdi **CRUD** yöntemlerimize geçelim

kullanıcı ekleme

    private int AddUser(User user)
    {
        const string query = "INSERT INTO User(FirstName, LastName) VALUES(@firstName, @lastName)";

        //here we are setting the parameter values that will be actually 
        //replaced in the query in Execute method
        var args = new Dictionary<string, object>
        {
            {"@firstName", user.FirstName},
            {"@lastName", user.Lastname}
        };

        return ExecuteWrite(query, args);
    }


Kullanıcı düzenleme

    private int EditUser(User user)
    {
        const string query = "UPDATE User SET FirstName = @firstName, LastName = @lastName WHERE Id = @id";

        //here we are setting the parameter values that will be actually 
        //replaced in the query in Execute method
        var args = new Dictionary<string, object>
        {
            {"@id", user.Id},
            {"@firstName", user.FirstName},
            {"@lastName", user.Lastname}
        };

        return ExecuteWrite(query, args);
    }
    
kullanıcı siliniyor

    private int DeleteUser(User user)
    {
        const string query = "Delete from User WHERE Id = @id";

        //here we are setting the parameter values that will be actually 
        //replaced in the query in Execute method
        var args = new Dictionary<string, object>
        {
            {"@id", user.Id}
        };

        return ExecuteWrite(query, args);
    }

'Id' ile kullanıcı alınıyor

    private User GetUserById(int id)
    {
        var query = "SELECT * FROM User WHERE Id = @id";

        var args = new Dictionary<string, object>
        {
            {"@id", id}
        };

        DataTable dt = ExecuteRead(query, args);

        if (dt == null || dt.Rows.Count == 0)
        {
            return null;
        }

        var user = new User
        {
            Id = Convert.ToInt32(dt.Rows[0]["Id"]),
            FirstName = Convert.ToString(dt.Rows[0]["FirstName"]),
            Lastname = Convert.ToString(dt.Rows[0]["LastName"])
        };

        return user;
    }


[1]: http://i.stack.imgur.com/owqid.png
[2]: http://i.stack.imgur.com/4N4MH.png
[3]: https://docs.nuget.org/ndocs/tools/package-manager-console
[4]: http://i.stack.imgur.com/baf9b.png

## Sorgu Yürütme

    using (SQLiteConnection conn = new SQLiteConnection(@"Data Source=data.db;Pooling=true;FailIfMissing=false"))
    {
        conn.Open();
        using (SQLiteCommand cmd = new SQLiteCommand(conn))
        {
           cmd.CommandText = "query";
           using (SqlDataReader dr = cmd.ExecuteReader())
           {
               while(dr.Read())
               {
                   //do stuff
               }
           }
        }
    }
    
*Not*: 'FailIfMissing'i true olarak ayarlamak, eksikse 'data.db' dosyasını oluşturur. Ancak dosya boş olacaktır. Bu nedenle, gerekli tabloların yeniden oluşturulması gerekir.

