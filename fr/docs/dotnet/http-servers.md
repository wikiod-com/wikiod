---
title: "Serveurs HTTP"
slug: "serveurs-http"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Serveur de fichiers HTTP de base en lecture seule (HttpListener)
**Remarques:**

Cet exemple doit être exécuté en mode administratif.

Un seul client simultané est pris en charge.

Pour plus de simplicité, les noms de fichiers sont supposés être tous en ASCII (pour la partie _filename_ dans l'en-tête _Content-Disposition_) et les erreurs d'accès aux fichiers ne sont pas gérées.


    using System;
    using System.IO;
    using System.Net;

    class HttpFileServer
    {
        private static HttpListenerResponse response;
        private static HttpListener listener;
        private static string baseFilesystemPath;

        static void Main(string[] args)
        {
            if (!HttpListener.IsSupported)
            {
                Console.WriteLine(
                    "*** HttpListener requires at least Windows XP SP2 or Windows Server 2003.");
                return;
            }

            if(args.Length < 2)
            {
                Console.WriteLine("Basic read-only HTTP file server");
                Console.WriteLine();
                Console.WriteLine("Usage: httpfileserver <base filesystem path> <port>");
                Console.WriteLine("Request format: http://url:port/path/to/file.ext");
                return;
            }

            baseFilesystemPath = Path.GetFullPath(args[0]);
            var port = int.Parse(args[1]);

            listener = new HttpListener();
            listener.Prefixes.Add("http://*:" + port + "/");
            listener.Start();

            Console.WriteLine("--- Server stated, base path is: " + baseFilesystemPath);
            Console.WriteLine("--- Listening, exit with Ctrl-C");
            try
            {
                ServerLoop();
            }
            catch(Exception ex)
            {
                Console.WriteLine(ex);
                if(response != null)
                {
                    SendErrorResponse(500, "Internal server error");
                }
            }
        }

        static void ServerLoop()
        {
            while(true)
            {
                var context = listener.GetContext();

                var request = context.Request;
                response = context.Response;
                var fileName = request.RawUrl.Substring(1);
                Console.WriteLine(
                    "--- Got {0} request for: {1}", 
                    request.HttpMethod, fileName);

                if (request.HttpMethod.ToUpper() != "GET")
                {
                    SendErrorResponse(405, "Method must be GET");
                    continue;
                }

                var fullFilePath = Path.Combine(baseFilesystemPath, fileName);
                if(!File.Exists(fullFilePath))
                {
                    SendErrorResponse(404, "File not found");
                    continue;
                }

                Console.Write("    Sending file...");
                using (var fileStream = File.OpenRead(fullFilePath))
                {
                    response.ContentType = "application/octet-stream";
                    response.ContentLength64 = (new FileInfo(fullFilePath)).Length;
                    response.AddHeader(
                        "Content-Disposition",
                        "Attachment; filename=\"" + Path.GetFileName(fullFilePath) + "\"");
                    fileStream.CopyTo(response.OutputStream);
                }

                response.OutputStream.Close();
                response = null;
                Console.WriteLine(" Ok!");
            }
        }

        static void SendErrorResponse(int statusCode, string statusResponse)
        {
            response.ContentLength64 = 0;
            response.StatusCode = statusCode;
            response.StatusDescription = statusResponse;
            response.OutputStream.Close();
            Console.WriteLine("*** Sent error: {0} {1}", statusCode, statusResponse);
        }
    }

## Serveur de fichiers HTTP de base en lecture seule (ASP.NET Core)
1 - Créez un dossier vide, il contiendra les fichiers créés dans les étapes suivantes.

2 - Créez un fichier nommé `project.json` avec le contenu suivant (ajustez le numéro de port et `rootDirectory` selon le cas) :

    {
      "dependencies": {
        "Microsoft.AspNet.Server.Kestrel": "1.0.0-rc1-final",
        "Microsoft.AspNet.StaticFiles": "1.0.0-rc1-final"
      },
    
      "commands": {
        "web": "Microsoft.AspNet.Server.Kestrel --server.urls http://localhost:60000"
      },
    
      "frameworks": {
        "dnxcore50": { }
      },
    
      "fileServer": {
        "rootDirectory": "c:\\users\\username\\Documents" 
      }
    }

3 - Créez un fichier nommé `Startup.cs` avec le code suivant :

    using System;
    using Microsoft.AspNet.Builder;
    using Microsoft.AspNet.FileProviders;
    using Microsoft.AspNet.Hosting;
    using Microsoft.AspNet.StaticFiles;
    using Microsoft.Extensions.Configuration;

    public class Startup
    {
        public void Configure(IApplicationBuilder app)
        {
            var builder = new ConfigurationBuilder();
            builder.AddJsonFile("project.json");
            var config = builder.Build();
            var rootDirectory = config["fileServer:rootDirectory"];
            Console.WriteLine("File server root directory: " + rootDirectory);

            var fileProvider = new PhysicalFileProvider(rootDirectory);

            var options = new StaticFileOptions();
            options.ServeUnknownFileTypes = true;
            options.FileProvider = fileProvider;
            options.OnPrepareResponse = context =>
            {
                context.Context.Response.ContentType = "application/octet-stream";
                context.Context.Response.Headers.Add(
                    "Content-Disposition",
                    $"Attachment; filename=\"{context.File.Name}\"");
            };
            
            app.UseStaticFiles(options);
        }
    }

4 - Ouvrez une invite de commande, accédez au dossier et exécutez :

    dnvm use 1.0.0-rc1-final -r coreclr -p
    dnu restore

**Noter:**
Ces commandes ne doivent être exécutées qu'une seule fois.
Utilisez `dnvm list` pour vérifier le nombre réel de la dernière version installée du noyau CLR.

5 - Démarrez le serveur avec : `dnx web`. Les fichiers peuvent maintenant être demandés à `http://localhost:60000/path/to/file.ext`.

Pour plus de simplicité, les noms de fichiers sont supposés être tous en ASCII (pour la partie nom de fichier dans l'en-tête Content-Disposition) et les erreurs d'accès aux fichiers ne sont pas gérées.

