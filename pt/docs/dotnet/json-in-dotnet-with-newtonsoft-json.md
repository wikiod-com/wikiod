---
title: "JSON em .NET com Newtonsoft.Json"
slug: "json-em-net-com-newtonsoftjson"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

O pacote NuGet `Newtonsoft.Json` tornou-se o padrão de fato para usar e manipular texto e objetos formatados em JSON em .NET. É uma ferramenta robusta, rápida e fácil de usar.

## Desserializar um objeto do texto JSON
    var json = "{\"Name\":\"Joe Smith\",\"Age\":21}";
    var person = JsonConvert.DeserializeObject<Person>(json);
    
Isso produz um objeto `Person` com o nome "Joe Smith" e Age 21.
    

## Serialize o objeto em JSON
    using Newtonsoft.Json;

    var obj = new Person
    {
        Name = "Joe Smith",
        Age = 21
    };
    var serializedJson = JsonConvert.SerializeObject(obj);

Isso resulta neste JSON: `{"Name":"Joe Smith","Age":21}`

