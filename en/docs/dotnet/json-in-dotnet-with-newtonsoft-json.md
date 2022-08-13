---
title: "JSON in .NET with Newtonsoft.Json"
slug: "json-in-net-with-newtonsoftjson"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

The NuGet package `Newtonsoft.Json` has become the defacto standard for using and manipulating JSON formatted text and objects in .NET. It is a robust tool that is fast, and easy to use.

## Deserialize an object from JSON text
    var json = "{\"Name\":\"Joe Smith\",\"Age\":21}";
    var person = JsonConvert.DeserializeObject<Person>(json);
    
This yields a `Person` object with Name "Joe Smith" and Age 21.
    

## Serialize object into JSON
    using Newtonsoft.Json;

    var obj = new Person
    {
        Name = "Joe Smith",
        Age = 21
    };
    var serializedJson = JsonConvert.SerializeObject(obj);

This results in this JSON: `{"Name":"Joe Smith","Age":21}`

