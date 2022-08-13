---
title: "Publishing and Deployment"
slug: "publishing-and-deployment"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Kestrel. Configuring Listening Address
Using Kestrel you can specify port using next approaches:

 1. Defining `ASPNETCORE_URLS` environment variable.
    
    Windows

        SET ASPNETCORE_URLS=https://0.0.0.0:5001
 
    OS X 
       
        export ASPNETCORE_URLS=https://0.0.0.0:5001

 2. Via command line passing `--server.urls` parameter

        dotnet run --server.urls=http://0.0.0.0:5001

 3. Using `UseUrls()` method

        var builder = new WebHostBuilder()
                      .UseKestrel()
                      .UseUrls("http://0.0.0.0:5001")

 4. Defining `server.urls` setting in configuration source.

 Next sample use hosting.json file for example.

    Add `hosting.json` with the following content to you project:

        {
           "server.urls": "http://<ip address>:<port>" 
        }
   Examples of posible values:

 - listen 5000 on any IP4 and IP6 addresses from any interface:
        
        "server.urls": "http://*:5000" 
    or

        "server.urls": "http://::5000;http://0.0.0.0:5000"
 - listen 5000 on every IP4 address:
        
        "server.urls": "http://0.0.0.0:5000"

> One should be carefully and not use `http://*:5000;http://::5000`,
> `http://::5000;http://*:5000`, `http://*:5000;http://0.0.0.0:5000` or
> `http://*:5000;http://0.0.0.0:5000` because it will require to register
> IP6 address :: or IP4 address 0.0.0.0 twice

Add file to `publishOptions` in `project.json`

    "publishOptions": {
    "include": [
        "hosting.json",
        ...
      ]
    }

and in entry point for the application call `.UseConfiguration(config)` when creating WebHostBuilder:

    public static void Main(string[] args)
    {
        var config = new ConfigurationBuilder()
            .SetBasePath(Directory.GetCurrentDirectory())
            .AddJsonFile("hosting.json", optional: true)
            .Build();

        var host = new WebHostBuilder()
            .UseConfiguration(config)
            .UseKestrel()
            .UseContentRoot(Directory.GetCurrentDirectory())
            .UseIISIntegration()
            .UseStartup<Startup>()
            .Build();

        host.Run();
    }

