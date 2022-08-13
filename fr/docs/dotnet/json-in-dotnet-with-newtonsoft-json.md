---
title: "JSON dans .NET avec Newtonsoft.Json"
slug: "json-dans-net-avec-newtonsoftjson"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

Le package NuGet `Newtonsoft.Json` est devenu la norme de facto pour l'utilisation et la manipulation de texte et d'objets au format JSON dans .NET. C'est un outil robuste, rapide et facile à utiliser.

## Désérialiser un objet à partir du texte JSON
    var json = "{\"Name\":\"Joe Smith\",\"Age\":21}";
    var person = JsonConvert.DeserializeObject<Person>(json);
    
Cela donne un objet `Person` avec le nom "Joe Smith" et Age 21.
    

## Sérialiser l'objet en JSON
    using Newtonsoft.Json;

    var obj = new Person
    {
        Name = "Joe Smith",
        Age = 21
    };
    var serializedJson = JsonConvert.SerializeObject(obj);

Il en résulte ce JSON : `{"Name":"Joe Smith","Age":21}`

