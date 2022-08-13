---
title: "clientes HTTP"
slug: "clientes-http"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

Los RFC de HTTP/1.1 actualmente relevantes son:

* [7230: sintaxis y enrutamiento de mensajes] (https://tools.ietf.org/html/rfc7230)
* [7231: Semántica y Contenido](https://tools.ietf.org/html/rfc7231)
* [7232: Solicitudes condicionales](https://tools.ietf.org/html/rfc7232)
* [7233: Solicitudes de rango](https://tools.ietf.org/html/rfc7233)
* [7234: almacenamiento en caché] (https://tools.ietf.org/html/rfc7234)
* [7235: Autenticación](https://tools.ietf.org/html/rfc7235)
* [7239: Extensión HTTP reenviada](https://tools.ietf.org/html/rfc7239)
* [7240: Preferir encabezado para HTTP](https://tools.ietf.org/html/rfc7240)

También hay los siguientes RFC informativos:

* [7236: Registros del esquema de autenticación] (https://tools.ietf.org/html/rfc7236)
* [7237: Registros de métodos](https://tools.ietf.org/html/rfc7237)

Y el RFC experimental:

* [7238: el código de estado del protocolo de transferencia de hipertexto 308 (redireccionamiento permanente)] (https://tools.ietf.org/html/rfc7238)

Protocolos relacionados:

* [4918: Extensiones HTTP para creación y control de versiones distribuidas en la Web (WebDAV)
](https://tools.ietf.org/html/rfc4918)
* [4791: Extensiones de calendario para WebDAV (CalDAV)
](https://tools.ietf.org/html/rfc4791)


## Leyendo la respuesta GET como una cadena usando System.Net.HttpClient
`HttpClient` está disponible a través de [NuGet: Microsoft HTTP Client Libraries](https://www.nuget.org/packages/Microsoft.Net.Http/).

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

## Descargador HTTP básico usando System.Net.Http.HttpClient
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



## Leyendo la respuesta GET como una cadena usando System.Net.HttpWebRequest
    string requestUri = "http://www.example.com";
    string responseData;
    
    HttpWebRequest request = (HttpWebRequest)WebRequest.Create(parameters.Uri);
    WebResponse response = request.GetResponse();
    
    using (StreamReader responseReader = new StreamReader(response.GetResponseStream()))
    {
        responseData = responseReader.ReadToEnd();
    }

## Leyendo la respuesta GET como una cadena usando System.Net.WebClient
    string requestUri = "http://www.example.com";
    string responseData;

    using (var client = new WebClient())
    {    
        responseData = client.DownloadString(requestUri);
    }

## Envío de una solicitud POST con una cadena de carga utilizando System.Net.HttpWebRequest
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

## Envío de una solicitud POST con una carga útil de cadena mediante System.Net.WebClient
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

## Envío de una solicitud POST con una cadena de carga utilizando System.Net.HttpClient
`HttpClient` está disponible a través de [NuGet: Microsoft HTTP Client Libraries](https://www.nuget.org/packages/Microsoft.Net.Http/).

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

