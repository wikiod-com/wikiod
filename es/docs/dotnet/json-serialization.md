---
title: "Serialización JSON"
slug: "serializacion-json"
draft: false
images: []
weight: 9725
type: docs
toc: true
---

**JavaScriptSerializer frente a Json.NET**

La [clase `JavaScriptSerializer`](https://msdn.microsoft.com/en-us/library/system.web.script.serialization.javascriptserializer(v=vs.110).aspx) se introdujo en .NET 3.5 y es utilizado internamente por la capa de comunicación asíncrona de .NET para aplicaciones habilitadas para AJAX. Se puede usar para trabajar con JSON en código administrado.

A pesar de la existencia de la clase `JavaScriptSerializer`, Microsoft recomienda usar la [biblioteca Json.NET] de código abierto (http://www.newtonsoft.com/json) para la serialización y deserialización. Json.NET ofrece un mejor rendimiento y una interfaz más amigable para asignar JSON a clases personalizadas (un [objeto `JavaScriptConverter` personalizado] (https://msdn.microsoft.com/en-us/library/system.web.script.serialization. javascriptconverter(v=vs.110).aspx) sería necesario para lograr lo mismo con `JavaScriptSerializer`).

## Deserialización usando System.Web.Script.Serialization.JavaScriptSerializer
El método `JavaScriptSerializer.Deserialize<T>(input)` intenta deserializar una cadena de JSON válido en un objeto del tipo especificado `<T>`, utilizando las asignaciones predeterminadas admitidas de forma nativa por `JavaScriptSerializer`.

<!-- idioma: c# -->
    using System.Collections;
    using System.Web.Script.Serialization;

    // ...

    string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

    JavaScriptSerializer JSS = new JavaScriptSerializer(); 
    Dictionary<string, object> parsedObj = JSS.Deserialize<Dictionary<string, object>>(rawJSON);

    string name = parsedObj["Name"].toString();
    ArrayList numbers = (ArrayList)parsedObj["Numbers"]

Nota: El objeto `JavaScriptSerializer` se introdujo en la versión 3.5 de .NET

## Deserialización usando Json.NET
<!-- idioma: c# -->
    internal class Sequence{
        public string Name;
        public List<int> Numbers;
    }    

    // ...

    string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

    Sequence sequence = JsonConvert.DeserializeObject<Sequence>(rawJSON);

Para obtener más información, consulte el [sitio oficial de Json.NET] (http://www.newtonsoft.com/json).

Nota: Json.NET es compatible con .NET versión 2 y superior.

## Serialización usando Json.NET
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
Observe cómo las propiedades (y las clases) se pueden decorar con atributos para cambiar su apariencia en la cadena json resultante o para eliminarlos de la cadena json (JsonIgnore).

Puede encontrar más información sobre los atributos de serialización de Json.NET [aquí][1].

En C#, los identificadores públicos se escriben en *PascalCase* por convención. En JSON, la convención es usar *camelCase* para todos los nombres. Puede usar una resolución de contrato para convertir entre los dos.

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

[1]: http://www.newtonsoft.com/json/help/html/serializationattributes.htm

## Serialización-Deserialización usando Newtonsoft.Json
A diferencia de los otros ayudantes, este usa ayudantes de clase estática para serializar y deserializar, por lo tanto, es un poco más fácil de usar que los otros.


    using Newtonsoft.Json;

    var rawJSON      = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
    var fibo         = JsonConvert.DeserializeObject<Dictionary<string, object>>(rawJSON);
    var rawJSON2     = JsonConvert.SerializeObject(fibo);



## Enlace dinámico
Json.NET de Newtonsoft le permite enlazar json dinámicamente (usando objetos ExpandoObject/Dynamic) sin necesidad de crear el tipo explícitamente.

**Publicación por entregas**

    dynamic jsonObject = new ExpandoObject();
    jsonObject.Title   = "Merchent of Venice";
    jsonObject.Author  = "William Shakespeare";
    Console.WriteLine(JsonConvert.SerializeObject(jsonObject));


**Desserialización**

    var rawJson = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
    dynamic parsedJson = JObject.Parse(rawJson);
    Console.WriteLine("Name: " + parsedJson.Name);
    Console.WriteLine("Name: " + parsedJson.Numbers.Length);

Observe que las claves en el objeto rawJson se han convertido en variables miembro en el objeto dinámico.

Esto es útil en los casos en que una aplicación puede aceptar/producir varios formatos de JSON. Sin embargo, se sugiere utilizar un nivel adicional de validación para la cadena Json o para el objeto dinámico generado como resultado de la serialización/deserialización.

## Serialización usando Json.NET con JsonSerializerSettings
Este serializador tiene algunas características interesantes que el serializador .net json predeterminado no tiene, como el manejo de valores nulos, solo necesita crear `JsonSerializerSettings`:

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore});
       return result;
    }

Otro problema grave del serializador en .net es el bucle de autorreferencia. En el caso de un estudiante que está inscrito en un curso, su instancia tiene una propiedad de curso y un curso tiene una colección de estudiantes que significa una `Lista<Estudiante>` que creará un bucle de referencia. Puede manejar esto con `JsonSerializerSettings`:

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
       return result;
    }

Puede poner varias opciones de serialización como esta:

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore, ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
       return result;
    }


