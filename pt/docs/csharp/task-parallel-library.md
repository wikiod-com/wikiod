---
title: "Biblioteca paralela de tarefas"
slug: "biblioteca-paralela-de-tarefas"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Parallel.ForEach
Um exemplo que usa o loop Parallel.ForEach para executar ping em uma determinada matriz de URLs de sites.

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
Um exemplo que usa o loop Parallel.For para executar ping em uma determinada matriz de URLs de sites.

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

## Paralelo.Invoke
Invocando métodos ou ações em paralelo (região paralela)

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

## Uma tarefa de sondagem cancelável usando CancellationTokenSource


## Versão assíncrona de PingUrl
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


## Uma tarefa de pesquisa assíncrona cancelável que aguarda entre as iterações


