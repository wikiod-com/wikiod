---
title: "Çoklu Sonuçlar"
slug: "coklu-sonuclar"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

## Sözdizimi
- genel statik SqlMapper.GridReader QueryMultiple(bu IDbConnection cnn, string sql, nesne param = null, IDbTransaction işlem = null, int? commandTimeout = null, CommandType? commandType = null)
- genel statik SqlMapper.GridReader QueryMultiple(bu IDbConnection cnn, CommandDefinition komutu)

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| cnn | Veritabanı bağlantınız zaten açık olmalıdır |
| sql | İşlenecek sql dizesi, birden çok sorgu içeriyor |
| param | Parametrelerin çıkarılacağı nesne |
| SqlMapper.GridReader | Bir Dapper sorgusundan birden çok sonuç kümesini okumak için arabirimler sağlar |


## Temel Çoklu Sonuç Örneği
Tek bir sorguda birden çok ızgarayı getirmek için 'QueryMultiple' yöntemi kullanılır. Bu, daha sonra, döndürülen `GridReader'a karşı art arda çağrılar yoluyla her bir ızgarayı *sırasıyla* almanızı sağlar.

    var sql = @"select * from Customers where CustomerId = @id
                select * from Orders where CustomerId = @id
                select * from Returns where CustomerId = @id";
    
    using (var multi = connection.QueryMultiple(sql, new {id=selectedId}))
    {
       var customer = multi.Read<Customer>().Single();
       var orders = multi.Read<Order>().ToList();
       var returns = multi.Read<Return>().ToList();
    } 

