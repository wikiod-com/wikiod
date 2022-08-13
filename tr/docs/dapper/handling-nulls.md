---
title: "Null İşlemleri"
slug: "null-islemleri"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## null vs DBNull
ADO.NET'te "null"u doğru bir şekilde işlemek sürekli bir karışıklık kaynağıdır. Dapper'daki kilit nokta şudur ki *yapmak zorunda değilsiniz*; hepsiyle kendi içinde ilgilenir.

- "null" olan parametre değerleri doğru şekilde "DBNull.Value" olarak gönderilir
- "boş" olan okunan değerler "boş" olarak sunulur veya (bilinen bir türle eşleme durumunda) basitçe yok sayılır (türe dayalı varsayılanlarını bırakarak)

Sadece çalışır:

    string name = null;
    int id = 123;
    connection.Execute("update Customer set Name=@name where Id=@id",
        new {id, name});

