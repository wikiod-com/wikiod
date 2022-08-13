---
title: "Client HTTP"
slug: "client-http"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

Les RFC HTTP/1.1 actuellement pertinentes sont :

* [7230 : Syntaxe et routage des messages](https://tools.ietf.org/html/rfc7230)
* [7231 : Sémantique et contenu](https://tools.ietf.org/html/rfc7231)
* [7232 : Requêtes conditionnelles](https://tools.ietf.org/html/rfc7232)
* [7233 : Demandes de plage](https://tools.ietf.org/html/rfc7233)
* [7234 : Mise en cache](https://tools.ietf.org/html/rfc7234)
* [7235 : Authentification](https://tools.ietf.org/html/rfc7235)
* [7239 : Extension HTTP transférée](https://tools.ietf.org/html/rfc7239)
* [7240 : Préférer l'en-tête pour HTTP](https://tools.ietf.org/html/rfc7240)

Il existe également les RFC informatifs suivants :

* [7236 : Enregistrements du schéma d'authentification](https://tools.ietf.org/html/rfc7236)
* [7237 : Enregistrements de méthode](https://tools.ietf.org/html/rfc7237)

Et le RFC expérimental :

* [7238 : Code d'état 308 du protocole de transfert hypertexte (redirection permanente)](https://tools.ietf.org/html/rfc7238)

Protocoles associés :

* [4918 : Extensions HTTP pour la création et la gestion des versions distribuées sur le Web (WebDAV)
](https://tools.ietf.org/html/rfc4918)
* [4791 : Extensions de calendrier pour WebDAV (CalDAV)
](https://tools.ietf.org/html/rfc4791)


## Lecture de la réponse GET sous forme de chaîne à l'aide de System.Net.HttpClient
`HttpClient` est disponible via [NuGet : Microsoft HTTP Client Libraries](https://www.nuget.org/packages/Microsoft.Net.Http/).

    string requestUri = "http://www.example.com";
    string responseData;
    
    using (var client = new HttpClient())
    {
        using(var response = client.GetAsync(requestUri).Result)
        {
           response.EnsureSuccessStatusCode();
           responseData = response.Content.ReadAsStringAsync().Result;
        }
    }

## Téléchargeur HTTP de base utilisant System.Net.Http.HttpClient
    using System;
    using System.IO;
    using System.Linq;
    using System.Net.Http;
    using System.Threading.Tasks;
    
    class HttpGet
    {
        private static async Task DownloadAsync(string fromUrl, string toFile)
        {
            using (var fileStream = File.OpenWrite(toFile))
            {
                using (var httpClient = new HttpClient())
                {
                    Console.WriteLine("Connecting...");
                    using (var networkStream = await httpClient.GetStreamAsync(fromUrl))
                    {
                        Console.WriteLine("Downloading...");
                        await networkStream.CopyToAsync(fileStream);
                        await fileStream.FlushAsync();
                    }
                }
            }
        }
    
        static void Main(string[] args)
        {
            try
            {
                Run(args).Wait();
            }
            catch (Exception ex)
            {
                if (ex is AggregateException)
                    ex = ((AggregateException)ex).Flatten().InnerExceptions.First();
    
                Console.WriteLine("--- Error: " + 
                    (ex.InnerException?.Message ?? ex.Message));
            }
        }
        static async Task Run(string[] args)
        {
            if (args.Length < 2)
            {
                Console.WriteLine("Basic HTTP downloader");
                Console.WriteLine();
                Console.WriteLine("Usage: httpget <url>[<:port>] <file>");
                return;
            }
    
            await DownloadAsync(fromUrl: args[0], toFile: args[1]);
    
            Console.WriteLine("Done!");
        }
    }



## Lecture de la réponse GET sous forme de chaîne à l'aide de System.Net.HttpWebRequest
    string requestUri = "http://www.example.com";
    string responseData;
    
    HttpWebRequest request = (HttpWebRequest)WebRequest.Create(parameters.Uri);
    WebResponse response = request.GetResponse();
    
    using (StreamReader responseReader = new StreamReader(response.GetResponseStream()))
    {
        responseData = responseReader.ReadToEnd();
    }

## Lecture de la réponse GET sous forme de chaîne à l'aide de System.Net.WebClient
    string requestUri = "http://www.example.com";
    string responseData;

    using (var client = new WebClient())
    {    
        responseData = client.DownloadString(requestUri);
    }

## Envoi d'une requête POST avec une charge utile de chaîne à l'aide de System.Net.HttpWebRequest
    string requestUri = "http://www.example.com";
    string requestBodyString = "Request body string.";
    string contentType = "text/plain";
    string requestMethod = "POST";
    
    HttpWebRequest request = (HttpWebRequest)WebRequest.Create(requestUri)
    {
      Method = requestMethod,
      ContentType = contentType,
    };

    byte[] bytes = Encoding.UTF8.GetBytes(requestBodyString);
    Stream stream = request.GetRequestStream();
    stream.Write(bytes, 0, bytes.Length);
    stream.Close();

    HttpWebResponse response = (HttpWebResponse)request.GetResponse();

## Envoi d'une requête POST avec une charge utile de chaîne à l'aide de System.Net.WebClient
    string requestUri = "http://www.example.com";
    string requestBodyString = "Request body string.";
    string contentType = "text/plain";
    string requestMethod = "POST";
        
    byte[] responseBody;    
    byte[] requestBodyBytes = Encoding.UTF8.GetBytes(requestBodyString);
    
    using (var client = new WebClient())
    {
        client.Headers[HttpRequestHeader.ContentType] = contentType;
        responseBody = client.UploadData(requestUri, requestMethod, requestBodyBytes);
    }

## Envoi d'une requête POST avec une charge utile de chaîne à l'aide de System.Net.HttpClient
`HttpClient` est disponible via [NuGet : Microsoft HTTP Client Libraries](https://www.nuget.org/packages/Microsoft.Net.Http/).

    string requestUri = "http://www.example.com";
    string requestBodyString = "Request body string.";
    string contentType = "text/plain";
    string requestMethod = "POST";

    var request = new HttpRequestMessage
    {
        RequestUri = requestUri,
        Method = requestMethod,
    };

    byte[] requestBodyBytes = Encoding.UTF8.GetBytes(requestBodyString);
    request.Content = new ByteArrayContent(requestBodyBytes);

    request.Content.Headers.ContentType = new MediaTypeHeaderValue(contentType);
    
    HttpResponseMessage result = client.SendAsync(request).Result;
    result.EnsureSuccessStatusCode();

