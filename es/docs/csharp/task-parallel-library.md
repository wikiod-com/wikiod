---
title: "Biblioteca paralela de tareas"
slug: "biblioteca-paralela-de-tareas"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Paralelo.ParaCada
Un ejemplo que usa el bucle Parallel.ForEach para hacer ping a una matriz determinada de direcciones URL de sitios web.

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

## Paralelo.Para
Un ejemplo que usa el bucle Parallel.For para hacer ping a una matriz determinada de direcciones URL de sitios web.

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

## Invocar.en.paralelo
Invocar métodos o acciones en paralelo (región paralela)

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

## Una tarea de sondeo cancelable usando CancellationTokenSource


## Versión asíncrona de PingUrl
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


## Una tarea de sondeo asíncrona cancelable que espera entre iteraciones


