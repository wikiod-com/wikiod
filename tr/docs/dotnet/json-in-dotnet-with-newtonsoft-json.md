---
title: "Newtonsoft.Json ile .NET'te JSON"
slug: "newtonsoftjson-ile-nette-json"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

NuGet paketi "Newtonsoft.Json", .NET'te JSON biçimli metin ve nesneleri kullanmak ve işlemek için fiili standart haline geldi. Hızlı ve kullanımı kolay, sağlam bir araçtır.

## JSON metninden bir nesnenin serisini kaldırma
    var json = "{\"Name\":\"Joe Smith\",\"Age\":21}";
    var person = JsonConvert.DeserializeObject<Person>(json);
    
Bu, Adı "Joe Smith" ve Yaş 21 olan bir "Kişi" nesnesi verir.
    

## Nesneyi JSON'a seri hale getirin
    using Newtonsoft.Json;

    var obj = new Person
    {
        Name = "Joe Smith",
        Age = 21
    };
    var serializedJson = JsonConvert.SerializeObject(obj);

Bu, şu JSON ile sonuçlanır: `{"Ad":"Joe Smith","Yaş":21}`

