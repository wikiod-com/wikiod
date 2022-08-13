---
title: "HTTP istemcileri"
slug: "http-istemcileri"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

Şu anda ilgili HTTP/1.1 RFC'ler şunlardır:

* [7230: İleti Sözdizimi ve Yönlendirme](https://tools.ietf.org/html/rfc7230)
* [7231: Anlambilim ve İçerik](https://tools.ietf.org/html/rfc7231)
* [7232: Koşullu İstekler](https://tools.ietf.org/html/rfc7232)
* [7233: Aralık İstekleri](https://tools.ietf.org/html/rfc7233)
* [7234: Önbelleğe Alma](https://tools.ietf.org/html/rfc7234)
* [7235: Kimlik Doğrulama](https://tools.ietf.org/html/rfc7235)
* [7239: Yönlendirilen HTTP Uzantısı](https://tools.ietf.org/html/rfc7239)
* [7240: HTTP için Başlığı Tercih Et](https://tools.ietf.org/html/rfc7240)

Ayrıca aşağıdaki bilgilendirici RFC'ler de vardır:

* [7236: Kimlik Doğrulama Düzeni Kayıtları](https://tools.ietf.org/html/rfc7236)
* [7237: Yöntem Kayıtları](https://tools.ietf.org/html/rfc7237)

Ve deneysel RFC:

* [7238: Köprü Metni Aktarım Protokolü Durum Kodu 308 (Kalıcı Yönlendirme)](https://tools.ietf.org/html/rfc7238)

İlgili protokoller:

* [4918: Web Dağıtılmış Yazma ve Sürüm Oluşturma (WebDAV) için HTTP Uzantıları
](https://tools.ietf.org/html/rfc4918)
* [4791: WebDAV için Takvim Uzantıları (CalDAV)
](https://tools.ietf.org/html/rfc4791)


## System.Net.HttpClient kullanarak GET yanıtını dize olarak okuma
"HttpClient", [NuGet: Microsoft HTTP İstemci Kitaplıkları](https://www.nuget.org/packages/Microsoft.Net.Http/) aracılığıyla kullanılabilir.

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

## System.Net.Http.HttpClient kullanan temel HTTP indiricisi
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



## System.Net.HttpWebRequest kullanarak GET yanıtını dize olarak okuma
    string requestUri = "http://www.example.com";
    string responseData;
    
    HttpWebRequest request = (HttpWebRequest)WebRequest.Create(parameters.Uri);
    WebResponse response = request.GetResponse();
    
    using (StreamReader responseReader = new StreamReader(response.GetResponseStream()))
    {
        responseData = responseReader.ReadToEnd();
    }

## System.Net.WebClient kullanarak GET yanıtını dize olarak okuma
    string requestUri = "http://www.example.com";
    string responseData;

    using (var client = new WebClient())
    {    
        responseData = client.DownloadString(requestUri);
    }

## System.Net.HttpWebRequest kullanarak dize yüküyle bir POST isteği gönderme
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

## System.Net.WebClient kullanarak bir dize yüküyle bir POST isteği gönderme
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

## System.Net.HttpClient kullanarak bir dize yüküyle bir POST isteği gönderme
"HttpClient", [NuGet: Microsoft HTTP İstemci Kitaplıkları](https://www.nuget.org/packages/Microsoft.Net.Http/) aracılığıyla kullanılabilir.

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

