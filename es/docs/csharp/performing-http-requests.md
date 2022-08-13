---
title: "Realización de solicitudes HTTP"
slug: "realizacion-de-solicitudes-http"
draft: false
images: []
weight: 9918
type: docs
toc: true
---

## Crear y enviar una solicitud HTTP POST
    using System.Net;
    using System.IO;

    ...

    string requestUrl = "https://www.example.com/submit.html";
    HttpWebRequest request = HttpWebRequest.CreateHttp(requestUrl);
    request.Method = "POST";

    // Optionally, set properties of the HttpWebRequest, such as:
    request.AutomaticDecompression = DecompressionMethods.Deflate | DecompressionMethods.GZip;
    request.ContentType = "application/x-www-form-urlencoded";
    // Could also set other HTTP headers such as Request.UserAgent, Request.Referer,
    // Request.Accept, or other headers via the Request.Headers collection.

    // Set the POST request body data. In this example, the POST data is in 
    // application/x-www-form-urlencoded format.
    string postData = "myparam1=myvalue1&myparam2=myvalue2";
    using (var writer = new StreamWriter(request.GetRequestStream()))
    {
        writer.Write(postData);
    }

    // Submit the request, and get the response body from the remote server.
    string responseFromRemoteServer;
    using (HttpWebResponse response = (HttpWebResponse)request.GetResponse())
    {
        using (StreamReader reader = new StreamReader(response.GetResponseStream()))
        {
            responseFromRemoteServer = reader.ReadToEnd();
        }
    }


## Crear y enviar una solicitud HTTP GET
    using System.Net;
    using System.IO;

    ...

    string requestUrl = "https://www.example.com/page.html";
    HttpWebRequest request = HttpWebRequest.CreateHttp(requestUrl);

    // Optionally, set properties of the HttpWebRequest, such as:
    request.AutomaticDecompression = DecompressionMethods.GZip | DecompressionMethods.Deflate;
    request.Timeout = 2 * 60 * 1000; // 2 minutes, in milliseconds
  
    // Submit the request, and get the response body.
    string responseBodyFromRemoteServer;
    using (HttpWebResponse response = (HttpWebResponse)request.GetResponse())
    {
        using (StreamReader reader = new StreamReader(response.GetResponseStream()))
        {
            responseBodyFromRemoteServer = reader.ReadToEnd();
        }
    }


## Manejo de errores de códigos de respuesta HTTP específicos (como 404 Not Found)
    using System.Net;

    ...

    string serverResponse;
    try 
    {
        // Call a method that performs an HTTP request (per the above examples).
        serverResponse = PerformHttpRequest();
    }
    catch (WebException ex)
    {
        if (ex.Status == WebExceptionStatus.ProtocolError)
        {
            HttpWebResponse response = ex.Response as HttpWebResponse;
            if (response != null)
            {
                if ((int)response.StatusCode == 404) // Not Found
                {
                    // Handle the 404 Not Found error 
                    // ...
                }
                else
                { 
                    // Could handle other response.StatusCode values here.
                    // ...
                }
            }
        }
        else
        {
            // Could handle other error conditions here, such as WebExceptionStatus.ConnectFailure.
            // ...
        }
    }

## Envío de solicitud HTTP POST asíncrona con cuerpo JSON
    public static async Task PostAsync(this Uri uri, object value)
    {
        var content = new ObjectContext(value.GetType(), value, new JsonMediaTypeFormatter());

        using (var client = new HttpClient())
        {
            return await client.PostAsync(uri, content);
        }
    }

    . . .

    var uri = new Uri("https://www.wikiod.com/es/docs/c%23/1971/performing-http-requests");
    await uri.PostAsync(new { foo = 123.45, bar = "Richard Feynman" });

## Recuperar HTML para página web (simple)
    string contents = "";
    string url = "http://msdn.microsoft.com";
    
    using (System.Net.WebClient client = new System.Net.WebClient())
    {
        contents = client.DownloadString(url);
    }
    
    Console.WriteLine(contents);



## Envío de solicitud HTTP GET asíncrona y lectura de solicitud JSON
    public static async Task<TResult> GetAnsync<TResult>(this Uri uri)
    {
        using (var client = new HttpClient())
        {
            var message = await client.GetAsync(uri);

            if (!message.IsSuccessStatusCode)
                throw new Exception();

            return message.ReadAsAsync<TResult>();
        }
    }

    . . .

    public class Result
    {
        public double foo { get; set; }

        public string bar { get; set; }
    }

    var uri = new Uri("https://www.wikiod.com/es/docs/c%23/1971/performing-http-requests");
    var result = await uri.GetAsync<Result>();

