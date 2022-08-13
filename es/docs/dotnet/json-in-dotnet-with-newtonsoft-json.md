---
title: "JSON en .NET con Newtonsoft.Json"
slug: "json-en-net-con-newtonsoftjson"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

El paquete NuGet `Newtonsoft.Json` se ha convertido en el estándar de facto para usar y manipular texto y objetos con formato JSON en .NET. Es una herramienta robusta, rápida y fácil de usar.

## Deserializar un objeto de texto JSON
    var json = "{\"Name\":\"Joe Smith\",\"Age\":21}";
    var person = JsonConvert.DeserializeObject<Person>(json);
    
Esto produce un objeto `Persona` con el nombre "Joe Smith" y edad 21.
    

## Serializar objeto en JSON
    using Newtonsoft.Json;

    var obj = new Person
    {
        Name = "Joe Smith",
        Age = 21
    };
    var serializedJson = JsonConvert.SerializeObject(obj);

Esto da como resultado este JSON: `{"Nombre":"Joe Smith","Edad":21}`

