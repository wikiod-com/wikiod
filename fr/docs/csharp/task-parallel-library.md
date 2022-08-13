---
title: "Bibliothèque parallèle de tâches"
slug: "bibliotheque-parallele-de-taches"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Parallèle.ForEach
Un exemple qui utilise la boucle Parallel.ForEach pour envoyer un ping à un tableau donné d'URL de sites Web.

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

## Parallèle.Pour
Un exemple qui utilise la boucle Parallel.For pour envoyer un ping à un tableau donné d'URL de sites Web.

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

## Parallel.Invoke
Invoquer des méthodes ou des actions en parallèle (région parallèle)

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

## Une tâche d'interrogation annulable à l'aide de CancellationTokenSource


## Version asynchrone de PingUrl
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


## Une tâche d'interrogation annulable asynchrone qui attend entre les itérations


