---
title: "Serialização JSON"
slug: "serializacao-json"
draft: false
images: []
weight: 9725
type: docs
toc: true
---

**JavaScriptSerializer vs Json.NET**

A [classe `JavaScriptSerializer`](https://msdn.microsoft.com/en-us/library/system.web.script.serialization.javascriptserializer(v=vs.110).aspx) foi introduzida no .NET 3.5 e é usado internamente pela camada de comunicação assíncrona do .NET para aplicativos habilitados para AJAX. Ele pode ser usado para trabalhar com JSON em código gerenciado.

Apesar da existência da classe `JavaScriptSerializer`, a Microsoft recomenda o uso da [biblioteca Json.NET] (http://www.newtonsoft.com/json) de código aberto para serialização e desserialização. O Json.NET oferece melhor desempenho e uma interface mais amigável para mapear JSON para classes personalizadas (um [objeto `JavaScriptConverter` personalizado](https://msdn.microsoft.com/en-us/library/system.web.script.serialization. javascriptconverter(v=vs.110).aspx) seria necessário para realizar o mesmo com `JavaScriptSerializer`).

## Desserialização usando System.Web.Script.Serialization.JavaScriptSerializer
O método `JavaScriptSerializer.Deserialize<T>(input)` tenta desserializar uma string de JSON válido em um objeto do tipo especificado `<T>`, usando os mapeamentos padrão suportados nativamente pelo `JavaScriptSerializer`.

<!-- idioma: c# -->
    using System.Collections;
    using System.Web.Script.Serialization;

    // ...

    string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

    JavaScriptSerializer JSS = new JavaScriptSerializer(); 
    Dictionary<string, object> parsedObj = JSS.Deserialize<Dictionary<string, object>>(rawJSON);

    string name = parsedObj["Name"].toString();
    ArrayList numbers = (ArrayList)parsedObj["Numbers"]

Nota: O objeto `JavaScriptSerializer` foi introduzido no .NET versão 3.5

## Desserialização usando Json.NET
<!-- idioma: c# -->
    internal class Sequence{
        public string Name;
        public List<int> Numbers;
    }    

    // ...

    string rawJSON = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";

    Sequence sequence = JsonConvert.DeserializeObject<Sequence>(rawJSON);

Para obter mais informações, consulte o [site oficial do Json.NET](http://www.newtonsoft.com/json).

Nota: Json.NET suporta .NET versão 2 e superior.

## Serialização usando Json.NET
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
Observe como as propriedades (e classes) podem ser decoradas com atributos para alterar sua aparência na string json resultante ou para removê-las da string json (JsonIgnore).

Mais informações sobre atributos de serialização Json.NET podem ser encontradas [aqui][1].

Em C#, os identificadores públicos são escritos em *PascalCase* por convenção. Em JSON, a convenção é usar *camelCase* para todos os nomes. Você pode usar um resolvedor de contrato para converter entre os dois.

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

## Serialização-desserialização usando Newtonsoft.Json
Ao contrário dos outros auxiliares, este usa auxiliares de classe estática para serializar e desserializar, portanto, é um pouco mais fácil de usar do que os outros.


    using Newtonsoft.Json;

    var rawJSON      = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
    var fibo         = JsonConvert.DeserializeObject<Dictionary<string, object>>(rawJSON);
    var rawJSON2     = JsonConvert.SerializeObject(fibo);



## Vinculação dinâmica
O Json.NET da Newtonsoft permite vincular json dinamicamente (usando objetos ExpandoObject/Dynamic) sem a necessidade de criar o tipo explicitamente.

**Serialização**

    dynamic jsonObject = new ExpandoObject();
    jsonObject.Title   = "Merchent of Venice";
    jsonObject.Author  = "William Shakespeare";
    Console.WriteLine(JsonConvert.SerializeObject(jsonObject));


**Desserialização**

    var rawJson = "{\"Name\":\"Fibonacci Sequence\",\"Numbers\":[0, 1, 1, 2, 3, 5, 8, 13]}";
    dynamic parsedJson = JObject.Parse(rawJson);
    Console.WriteLine("Name: " + parsedJson.Name);
    Console.WriteLine("Name: " + parsedJson.Numbers.Length);

Observe que as chaves no objeto rawJson foram transformadas em variáveis ​​de membro no objeto dinâmico.

Isso é útil nos casos em que um aplicativo pode aceitar/produzir formatos variados de JSON. No entanto, é sugerido usar um nível extra de validação para a string Json ou para o objeto dinâmico gerado como resultado da serialização/desserialização.

## Serialização usando Json.NET com JsonSerializerSettings
Este serializador tem alguns recursos interessantes que o serializador .net json padrão não possui, como manipulação de valor nulo, você só precisa criar o `JsonSerializerSettings`:

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore});
       return result;
    }

Outro problema sério do serializador no .net é o loop de auto-referência. No caso de um aluno matriculado em um curso, sua instância possui uma propriedade course e um curso possui uma coleção de alunos que significa uma `List<Student>` que criará um loop de referência. Você pode lidar com isso com `JsonSerializerSettings`:

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
       return result;
    }

Você pode colocar várias opções de serializações como esta:

    public static string Serialize(T obj)
    {
       string result = JsonConvert.SerializeObject(obj, new JsonSerializerSettings { NullValueHandling = NullValueHandling.Ignore, ReferenceLoopHandling = ReferenceLoopHandling.Ignore});
       return result;
    }


