---
title: "Dapper.NET'i kullanmaya başlama"
slug: "dapperneti-kullanmaya-baslama"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Nuget'ten Dapper'ı yükleyin
Ya Visual Studio GUI'de arama yapın:

Araçlar > NuGet Paket Yöneticisi > Çözüm için Paketleri Yönet... (Visual Studio 2015)

[![Dapper seçiliyken Visual Studio paket yöneticisi arayüzünün ekran görüntüsü][1]][1]

Veya en son kararlı sürümü yüklemek için bu komutu bir Nuget Power Shell örneğinde çalıştırın.

    Install-Package Dapper

Veya belirli bir sürüm için

    Install-Package Dapper -Version 1.42.0

[1]: http://i.stack.imgur.com/sWn6V.png

## Dapper'ı C#'ta Kullanmak
    using System.Data;
    using System.Linq;
    using Dapper;
    
    class Program
    {
        static void Main()
        {
            using (IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true"))
            {
                db.Open();
                var result = db.Query<string>("SELECT 'Hello World'").Single();
                Console.WriteLine(result);
            }
        }
    }

Bağlantıyı bir [`Using` bloğuna](https://www.wikiod.com/tr/docs/c%23/38/using-statement/157/cleaner-dispose-syntax) sarmak bağlantıyı kapatır

## Dapper'ı LINQPad'de Kullanma
[LINQPad](http://www.linqpad.net/) veritabanı sorgularını test etmek için mükemmeldir ve [NuGet entegrasyonunu](http://www.linqpad.net/Purchase.aspx#NuGet) içerir. Dapper'ı LINQPad'de kullanmak için **F4**'e basarak Sorgu Özellikleri'ni açın ve ardından **NuGet Ekle**'yi seçin. **dapper dot net**'i arayın ve **Sorguya Ekle**'yi seçin. Ayrıca **Ad alanları ekle**'ye tıklayıp Uzantı Yöntemlerini LINQPad sorgunuza dahil etmek için Dapper'ı vurgulamak isteyeceksiniz.

Dapper etkinleştirildiğinde, Dil açılır menüsünü **C# Programı** olarak değiştirebilir, sorgu sonuçlarını C# sınıflarıyla eşleyebilir ve sonuçları incelemek için .Dump() yöntemini kullanabilirsiniz:

geçersiz Ana()
	{
(IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true")){ kullanılarak
db.Open();
var skaler = db.Query<string>("SELECT GETDATE()").SingleOrDefault();
skalar.Dump("Bu bir dizi skaler sonucudur:");
			
var sonuçlar = db.Query<myobject>(@"
SEÇ * DAN (
DEĞERLER (1,'bir'),
(2,'iki'),
(3,'üç')
) AS mytable(id,name)");
sonuçlar.Dump("Bu, bir sınıfa eşlenmiş bir tablodur:");
		}
	}
	
// Diğer metotları ve sınıfları burada tanımlayın
sınıf nesnem {
public int id { get; Ayarlamak; }
genel dize adı { get; Ayarlamak; }
	}

Programı yürütürken sonuçlar şöyle görünür:

[![LINQPad ekran görüntüsü][1]][1]


[1]: http://i.stack.imgur.com/swXB1.png

