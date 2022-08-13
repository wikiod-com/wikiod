---
title: "Komutları Yürütme"
slug: "komutlar-yurutme"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

## Saklı Yordamlar
## Basit kullanım ##

Dapper, depolanan işlemleri tamamen destekler:

    var user = conn.Query<User>("spGetUser", new { Id = 1 }, 
                                commandType: CommandType.StoredProcedure)
               .SingleOrDefault();

## Giriş, Çıkış ve Dönüş parametreleri ##
Daha süslü bir şey istiyorsanız, şunları yapabilirsiniz:

    var p = new DynamicParameters();
    p.Add("@a", 11);
    p.Add("@b", 
          dbType: DbType.Int32, 
          direction: ParameterDirection.Output);
    p.Add("@c", 
          dbType: DbType.Int32, 
          direction: ParameterDirection.ReturnValue);
    
    conn.Execute("spMagicProc", p, 
                 commandType: CommandType.StoredProcedure); 
    
    var b = p.Get<int>("@b");
    var c = p.Get<int>("@c"); 

## Tablo Değerli Parametreler ##
Tablo Değerli Parametresini kabul eden bir saklı yordamınız varsa, SQL Server'daki tablo türüyle aynı yapıya sahip bir DataTable geçirmeniz gerekir.
İşte bir tablo türü için bir tanım ve onu kullanan prosedür:

    CREATE TYPE [dbo].[myUDTT] AS TABLE([i1] [int] NOT NULL);
    GO
    CREATE PROCEDURE myProc(@data dbo.myUDTT readonly) AS
    SELECT i1 FROM @data;
    GO
    /*
    -- optionally grant permissions as needed, depending on the user you execute this with.
    -- Especially the GRANT EXECUTE ON TYPE is often overlooked and can cause problems if omitted.
    GRANT EXECUTE ON TYPE::[dbo].[myUDTT] TO [user];
    GRANT EXECUTE ON dbo.myProc TO [user];
    GO
    */
Bu prosedürü c# içinden çağırmak için aşağıdakileri yapmanız gerekir:

    // Build a DataTable with one int column
    DataTable data = new DataTable();
    data.Columns.Add("i1", typeof(int));
    // Add two rows
    data.Rows.Add(1);
    data.Rows.Add(2);

    var q = conn.Query("myProc", new {data}, commandType: CommandType.StoredProcedure);


## Sonuç döndürmeyen bir komut yürütün
    IDBConnection db = /* ... */  
    var id = /* ... */

    db.Execute(@"update dbo.Dogs set Name = 'Beowoof' where Id = @id",
       new { id });

