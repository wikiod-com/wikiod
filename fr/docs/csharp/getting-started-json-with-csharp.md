---
title: "Premiers pas avec Json avec C#"
slug: "premiers-pas-avec-json-avec-c"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

La rubrique suivante présentera une manière de travailler avec Json en utilisant le langage C# et les concepts de sérialisation et de désérialisation.

## Exemple Json simple
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

Si vous débutez avec Json, voici un [tutoriel illustré][1].


[1] : https://www.w3schools.com/js/js_json_intro.asp "tutoriel"

## Tout d'abord : bibliothèque pour travailler avec Json
Pour travailler avec Json en utilisant C #, il est nécessaire d'utiliser Newtonsoft (bibliothèque .net). Cette bibliothèque fournit des méthodes qui permettent au programmeur de sérialiser et de désérialiser des objets et plus encore.
[Il y a un tutoriel][1] si vous voulez connaître les détails de ses méthodes et usages.

Si vous utilisez Visual Studio, accédez à *Tools/Nuget Package Manager/Manage Package to Solution/* et tapez "Newtonsoft" dans la barre de recherche et installez le package.
Si vous n'avez pas NuGet, ce [tutoriel détaillé][2] pourrait vous aider.


[1] : http://www.newtonsoft.com/json
[2] : https://developer.xamarin.com/guides/cross-platform/xamarin-studio/nuget_walkthrough/

## Implémentation C#
Avant de lire du code, il est important de comprendre les principaux concepts qui aideront à programmer des applications en json.

> **Sérialisation** : processus de conversion d'un objet en un flux d'octets pouvant être envoyé via des applications. Le code suivant peut être sérialisé et converti dans le json précédent.


> **Désérialisation** : Processus de conversion d'un json/flux d'octets en un objet. C'est exactement le processus inverse de la sérialisation. Le json précédent peut être désérialisé en un objet C#, comme illustré dans les exemples ci-dessous.

Pour résoudre ce problème, il est important de transformer la structure json en classes afin d'utiliser les processus déjà décrits. Si vous utilisez Visual Studio, vous pouvez transformer automatiquement un json en une classe en sélectionnant *"Edit/Paste Special/Paste JSON as Classes"* et en collant la structure json.

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

## Sérialisation
 
     static void Main(string[] args)
        {
            Book[] books = new Book[3];
            Author author = new Author(89,"Aldous Huxley","Author",books);
            string objectDeserialized = JsonConvert.SerializeObject(author); 
            //Converting author into json
        }

La méthode ".SerializeObject" reçoit en paramètre un *type object*, vous pouvez donc y mettre n'importe quoi.

## Désérialisation
Vous pouvez recevoir un json de n'importe où, d'un fichier ou même d'un serveur, il n'est donc pas inclus dans le code suivant.

    static void Main(string[] args)
    {
        string jsonExample; // Has the previous json
        Author author = JsonConvert.DeserializeObject<Author>(jsonExample);
    }

La méthode ".DeserializeObject" désérialise '*jsonExample*' en un objet "*Author*". C'est pourquoi il est important de définir les variables json dans la définition des classes, afin que la méthode y accède afin de la remplir.

## Fonction des utilitaires communs de sérialisation et de désérialisation
Cet exemple utilisait une fonction commune pour tous les types de sérialisation et de désérialisation d'objets.

<pré>
en utilisant System.Runtime.Serialization.Formatters.Binary ;
en utilisant System.Xml.Serialization ;

Cadre d'espace de noms
{
classe statique publique IGUtilities
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



