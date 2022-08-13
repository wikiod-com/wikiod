---
title: "Sérialisation JSON"
slug: "serialisation-json"
draft: false
images: []
weight: 9725
type: docs
toc: true
---

**JavaScriptSerializer contre Json.NET**

La [classe `JavaScriptSerializer`](https://msdn.microsoft.com/en-us/library/system.web.script.serialization.javascriptserializer(v=vs.110).aspx) a été introduite dans .NET 3.5 et est utilisé en interne par la couche de communication asynchrone de .NET pour les applications compatibles AJAX. Il peut être utilisé pour travailler avec JSON dans du code managé.

Malgré l'existence de la classe `JavaScriptSerializer`, Microsoft recommande d'utiliser la [bibliothèque Json.NET] open source (http://www.newtonsoft.com/json) pour la sérialisation et la désérialisation. Json.NET offre de meilleures performances et une interface plus conviviale pour mapper JSON à des classes personnalisées (un objet [`JavaScriptConverter` personnalisé](https://msdn.microsoft.com/en-us/library/system.web.script.serialization. javascriptconverter(v=vs.110).aspx) serait nécessaire pour accomplir la même chose avec `JavaScriptSerializer`).

## Désérialisation à l'aide de System.Web.Script.Serialization.JavaScriptSerializer
La méthode `JavaScriptSerializer.Deserialize<T>(input)` tente de désérialiser une chaîne de JSON valide dans un objet du type spécifié `<T>`, en utilisant les mappages par défaut pris en charge de manière native par `JavaScriptSerializer`.

<!-- langage : c# -->
    using System.Collections;
    using System.Web.Script.Serialization;

    // ...

    string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

    JavaScriptSerializer JSS = new JavaScriptSerializer(); 
    Dictionary<string, object> parsedObj = JSS.Deserialize<Dictionary<string, object>>(rawJSON);

    string name = parsedObj["Name"].toString();
    ArrayList numbers = (ArrayList)parsedObj["Numbers"]

Remarque : L'objet `JavaScriptSerializer` a été introduit dans la version 3.5 de .NET

## Désérialisation à l'aide de Json.NET
<!-- langage : c# -->
    internal class Sequence{
        public string Name;
        public List<int> Numbers;
    }    

    // ...

    string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

    Sequence sequence = JsonConvert.DeserializeObject<Sequence>(rawJSON);

Pour plus d'informations, consultez le [site officiel de Json.NET](http://www.newtonsoft.com/json).

Remarque : Json.NET prend en charge .NET version 2 et supérieure.

## Sérialisation à l'aide de Json.NET
    [JsonObject("person")]
    public class Person
    {
        [JsonProperty("name")]
        public string PersonName { get; set; }
        [JsonProperty("age")]
        public int PersonAge { get; set; }
        [JsonIgnore]
        public string Address { get; set; }
    }

    Person person = new Person { PersonName = "Andrius", PersonAge = 99, Address = "Some address" };
    string rawJson = JsonConvert.SerializeObject(person);

    Console.WriteLine(rawJson); // {"name":"Andrius","age":99}
Remarquez comment les propriétés (et les classes) peuvent être décorées avec des attributs pour modifier leur apparence dans la chaîne json résultante ou pour les supprimer complètement de la chaîne json (JsonIgnore).

Vous trouverez plus d'informations sur les attributs de sérialisation Json.NET [ici][1].

En C#, les identifiants publics sont écrits en *PascalCase* par convention. Dans JSON, la convention consiste à utiliser *camelCase* pour tous les noms. Vous pouvez utiliser un résolveur de contrat pour convertir entre les deux.

    using Newtonsoft.Json;
    using Newtonsoft.Json.Serialization;

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        [JsonIgnore]
        public string Address { get; set; }
    }

    public void ToJson() {
        Person person = new Person { Name = "Andrius", Age = 99, Address = "Some address" };
        var resolver = new CamelCasePropertyNamesContractResolver();
        var settings = new JsonSerializerSettings { ContractResolver = resolver };
        string json = JsonConvert.SerializeObject(person, settings);

        Console.WriteLine(json); // {"name":"Andrius","age":99}
    }

[1] : http://www.newtonsoft.com/json/help/html/serializationattributes.htm

## Sérialisation-Désérialisation à l'aide de Newtonsoft.Json
Contrairement aux autres assistants, celui-ci utilise des assistants de classe statiques pour sérialiser et désérialiser, il est donc un peu plus facile à utiliser que les autres.


    using Newtonsoft.Json;

    var rawJSON      = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
    var fibo         = JsonConvert.DeserializeObject<Dictionary<string, object>>(rawJSON);
    var rawJSON2     = JsonConvert.SerializeObject(fibo);



## Liaison dynamique
Json.NET de Newtonsoft vous permet de lier dynamiquement json (à l'aide d'objets ExpandoObject / Dynamic) sans avoir besoin de créer le type explicitement.

**Sérialisation**

    dynamic jsonObject = new ExpandoObject();
    jsonObject.Title   = "Merchent of Venice";
    jsonObject.Author  = "William Shakespeare";
    Console.WriteLine(JsonConvert.SerializeObject(jsonObject));


**Désérialisation**

    var rawJson = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
    dynamic parsedJson = JObject.Parse(rawJson);
    Console.WriteLine("Name: " + parsedJson.Name);
    Console.WriteLine("Name: " + parsedJson.Numbers.Length);

Notez que les clés de l'objet rawJson ont été transformées en variables membres dans l'objet dynamique.

Ceci est utile dans les cas où une application peut accepter/produire différents formats de JSON. Il est cependant suggéré d'utiliser un niveau de validation supplémentaire pour la chaîne Json ou pour l'objet dynamique généré à la suite de la sérialisation/désérialisation.

## Sérialisation à l'aide de Json.NET avec JsonSerializerSettings
Ce sérialiseur a quelques fonctionnalités intéressantes que le sérialiseur .net json par défaut n'a pas, comme la gestion des valeurs Null, il vous suffit de créer le `JsonSerializerSettings` :

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore});
       return result;
    }

Un autre problème sérieux de sérialiseur dans .net est la boucle d'auto-référence. Dans le cas d'un étudiant inscrit à un cours, son instance a une propriété course et un cours a une collection d'étudiants qui signifie une `List<Student>` qui créera une boucle de référence. Vous pouvez gérer cela avec `JsonSerializerSettings` :

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
       return result;
    }

Vous pouvez mettre diverses options de sérialisation comme ceci :

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore, ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
       return result;
    }


