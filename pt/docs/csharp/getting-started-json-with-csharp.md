---
title: "Introdução Json com C#"
slug: "introducao-json-com-c"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

O tópico a seguir apresentará uma maneira de trabalhar com Json usando a linguagem C# e os conceitos de serialização e desserialização.

## Exemplo simples de Json
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

Se você é novo em Json, aqui está um [tutorial exemplificado][1].


[1]: https://www.w3schools.com/js/js_json_intro.asp "tutorial"

## Primeiras coisas primeiro: Biblioteca para trabalhar com Json
Para trabalhar com Json usando C#, é preciso usar Newtonsoft (biblioteca .net). Esta biblioteca fornece métodos que permitem ao programador serializar e desserializar objetos e muito mais.
[Existe um tutorial][1] se você quiser saber detalhes sobre seus métodos e usos.

Se você usa o Visual Studio, vá para *Tools/Nuget Package Manager/Manage Package to Solution/* e digite "Newtonsoft" na barra de pesquisa e instale o pacote.
Se você não tiver o NuGet, este [tutorial detalhado][2] pode ajudá-lo.


[1]: http://www.newtonsoft.com/json
[2]: https://developer.xamarin.com/guides/cross-platform/xamarin-studio/nuget_walkthrough/

## Implementação C#
Antes de ler algum código, é importante entender os principais conceitos que ajudarão a programar aplicações usando json.

> **Serialização**: Processo de conversão de um objeto em um fluxo de bytes que pode ser enviado por meio de aplicativos. O código a seguir pode ser serializado e convertido no json anterior.


> **Desserialização**: Processo de conversão de um json/stream de bytes em um objeto. É exatamente o processo oposto de serialização. O json anterior pode ser desserializado em um objeto C#, conforme demonstrado nos exemplos abaixo.

Para resolver isso, é importante transformar a estrutura json em classes para utilizar os processos já descritos. Se você usar o Visual Studio, poderá transformar um json em uma classe automaticamente apenas selecionando *"Edit/Paste Special/Paste JSON as Classes"* e colando a estrutura do json.

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

## Serialização
 
     static void Main(string[] args)
        {
            Book[] books = new Book[3];
            Author author = new Author(89,"Aldous Huxley","Author",books);
            string objectDeserialized = JsonConvert.SerializeObject(author); 
            //Converting author into json
        }

O método ".SerializeObject" recebe como parâmetro um *type object*, então você pode colocar qualquer coisa nele.

## Desserialização
Você pode receber um json de qualquer lugar, um arquivo ou até mesmo um servidor para que não seja incluído no código a seguir.

    static void Main(string[] args)
    {
        string jsonExample; // Has the previous json
        Author author = JsonConvert.DeserializeObject<Author>(jsonExample);
    }

O método ".DeserializeObject" desserializa '*jsonExample*' em um objeto "*Author*". Por isso é importante definir as variáveis ​​json na definição das classes, para que o método acesse-as para preenchê-las.

## Função de utilitários comuns de serialização e deserialização
Este exemplo usado para função comum para todos os tipos de serialização e desserialização de objetos.

<pré>
usando System.Runtime.Serialization.Formatters.Binary;
usando System.Xml.Serialization;

Estrutura de namespace
{
classe estática pública IGUtilities
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



