---
title: "Görev Paralel Kitaplığı"
slug: "gorev-paralel-kitaplg"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Paralel.Herbiri İçin
Belirli bir web sitesi url'si dizisine ping atmak için Parallel.ForEach döngüsünü kullanan bir örnek.

    static void Main()
    {
        string [] urls = 
        {
            "www.stackoverflow.com", 
            "www.google.net", 
            "www.facebook.com", 
            "www.twitter.com"
        };
        
        System.Threading.Tasks.Parallel.ForEach(urls, url =>
        {
            var ping = new System.Net.NetworkInformation.Ping();
    
            var result = ping.Send(url);
    
            if (result.Status == System.Net.NetworkInformation.IPStatus.Success)
            {
                Console.WriteLine(string.Format("{0} is online", url));
            }
        });
    }

## Paralel.For
Belirli bir web sitesi url'si dizisine ping atmak için Parallel.For döngüsünü kullanan bir örnek.

    static void Main()
    {
        string [] urls = 
        {
            "www.stackoverflow.com", 
            "www.google.net", 
            "www.facebook.com", 
            "www.twitter.com"
        };
    
        System.Threading.Tasks.Parallel.For(0, urls.Length, i =>
        {
            var ping = new System.Net.NetworkInformation.Ping();
    
            var result = ping.Send(urls[i]);
    
            if (result.Status == System.Net.NetworkInformation.IPStatus.Success)
            {
                Console.WriteLine(string.Format("{0} is online", urls[i]));
            }
        });
    }

## Paralel. Çağır
Yöntemleri veya eylemleri paralel olarak çağırma (Paralel bölge)

    static void Main()
    {
        string [] urls = 
        {
            "www.stackoverflow.com", 
            "www.google.net", 
            "www.facebook.com", 
            "www.twitter.com"
        };
        
        System.Threading.Tasks.Parallel.Invoke(
            () => PingUrl(urls[0]),
            () => PingUrl(urls[1]),
            () => PingUrl(urls[2]),
            () => PingUrl(urls[3])
        );
    }
    
    void PingUrl(string url)
    {
        var ping = new System.Net.NetworkInformation.Ping();
        
        var result = ping.Send(url);
        
        if (result.Status == System.Net.NetworkInformation.IPStatus.Success)
        {
            Console.WriteLine(string.Format("{0} is online", url));
        }
    }

## CancellationTokenSource kullanarak iptal edilebilir bir yoklama Görevi


## PingUrl'nin zaman uyumsuz sürümü
        static void Main(string[] args)
        {
            string url = "www.stackoverflow.com";
            var pingTask = PingUrlAsync(url);
            Console.WriteLine($"Waiting for response from {url}");
            Task.WaitAll(pingTask);            
            Console.WriteLine(pingTask.Result);
        }

        static async Task<string> PingUrlAsync(string url)
        {
            string response = string.Empty;
            var ping = new System.Net.NetworkInformation.Ping();

            var result = await ping.SendPingAsync(url);

            await Task.Delay(5000); //simulate slow internet

            if (result.Status == System.Net.NetworkInformation.IPStatus.Success)
            {
                response = $"{url} is online";
            }

            return response;
        }


## Yinelemeler arasında bekleyen zaman uyumsuz iptal edilebilir bir yoklama Görevi


