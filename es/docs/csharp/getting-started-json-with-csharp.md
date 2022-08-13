---
title: "Primeros pasos Json con C#"
slug: "primeros-pasos-json-con-c"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

El siguiente tema presentará una forma de trabajar con Json utilizando el lenguaje C# y los conceptos de serialización y deserialización.

## Ejemplo simple de Json
    {
        "id": 89,
        "name": "Aldous Huxley",
        "type": "Author",
        "books":[{
                   "name": "Brave New World",
                   "date": 1932 
                 },
                 {
                   "name": "Eyeless in Gaza",
                   "date": 1936
                 },
                 {
                   "name": "The Genius and the Goddess",
                   "date": 1955 
                 }]  
    }

Si eres nuevo en Json, aquí tienes un [tutorial ejemplificado][1].


[1]: https://www.w3schools.com/js/js_json_intro.asp "tutorial"

## Lo primero es lo primero: biblioteca para trabajar con Json
Para trabajar con Json usando C#, es necesario usar Newtonsoft (biblioteca .net). Esta biblioteca proporciona métodos que permiten al programador serializar y deserializar objetos y más.
[Hay un tutorial][1] si desea conocer detalles sobre sus métodos y usos.

Si usa Visual Studio, vaya a *Tools/Nuget Package Manager/Manage Package to Solution/* y escriba "Newtonsoft" en la barra de búsqueda e instale el paquete.
Si no tiene NuGet, este [tutorial detallado][2] podría ayudarlo.


[1]: http://www.newtonsoft.com/json
[2]: https://developer.xamarin.com/guides/cross-platform/xamarin-studio/nuget_walkthrough/

## Implementación de C#
Antes de leer algo de código, es importante entender los conceptos principales que ayudarán a programar aplicaciones usando json.

> **Serialización**: Proceso de convertir un objeto en un flujo de bytes que se pueden enviar a través de aplicaciones. El siguiente código se puede serializar y convertir en el json anterior.


> **Deserialización**: Proceso de convertir un json/flujo de bytes en un objeto. Es exactamente el proceso opuesto de serialización. El json anterior se puede deserializar en un objeto C# como se demuestra en los ejemplos a continuación.

Para resolver esto, es importante convertir la estructura json en clases para usar los procesos ya descritos. Si usa Visual Studio, puede convertir un json en una clase automáticamente con solo seleccionar *"Editar/Pegar especial/Pegar JSON como clases"* y pegar la estructura json.

    using Newtonsoft.Json;

      class Author
    {
        [JsonProperty("id")] // Set the variable below to represent the json attribute 
        public int id;       //"id"
        [JsonProperty("name")]
        public string name;
        [JsonProperty("type")]
        public string type;
        [JsonProperty("books")]
        public Book[] books;

        public Author(int id, string name, string type, Book[] books) {
            this.id = id;
            this.name = name;
            this.type= type;
            this.books = books;
        }
    }

     class Book
    {
       [JsonProperty("name")]
       public string name;
       [JsonProperty("date")]
       public DateTime date;
    }

## Serialización
 
     static void Main(string[] args)
        {
            Book[] books = new Book[3];
            Author author = new Author(89,"Aldous Huxley","Author",books);
            string objectDeserialized = JsonConvert.SerializeObject(author); 
            //Converting author into json
        }

El método ".SerializeObject" recibe como parámetro un *tipo de objeto*, por lo que puedes poner cualquier cosa en él.

## Deserialización
Puede recibir un json desde cualquier lugar, un archivo o incluso un servidor, por lo que no se incluye en el siguiente código.

    static void Main(string[] args)
    {
        string jsonExample; // Has the previous json
        Author author = JsonConvert.DeserializeObject<Author>(jsonExample);
    }

El método ".DeserializeObject" deserializa '*jsonExample*' en un objeto "*Author*". Por eso es importante establecer las variables json en la definición de clases, para que el método acceda a ellas para completarlas.

## Función de utilidades comunes de serialización y deserialización
Esta muestra solía tener una función común para todo tipo de serialización y deserialización de objetos.

<antes>
usando System.Runtime.Serialization.Formatters.Binary;
usando System.Xml.Serialización;

Marco de espacio de nombres
{
clase estática pública IGUtilities
{
        public static string Serialization<T>(this T obj)
        {
        string data = JsonConvert.SerializeObject(obj);
        return data;
        }

        public static T Deserialization<T>(this string JsonData)
        {
        T copy = JsonConvert.DeserializeObject<T>(JsonData);
        return copy;
        }

         public static T Clone<T>(this T obj)
        {
            string data = JsonConvert.SerializeObject(obj);
            T copy = JsonConvert.DeserializeObject<T>(data);
            return copy;
        }
}
}
</pre>



