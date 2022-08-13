---
title: "Servidores HTTP"
slug: "servidores-http"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Servidor de arquivos HTTP básico somente leitura (HttpListener)
**Notas:**

Este exemplo deve ser executado em modo administrativo.

Apenas um cliente simultâneo é suportado.

Para simplificar, supõe-se que os nomes dos arquivos sejam todos ASCII (para a parte _filename_ no cabeçalho _Content-Disposition_) e os erros de acesso ao arquivo não são tratados.


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

## Servidor de arquivos HTTP básico somente leitura (ASP.NET Core)
1 - Crie uma pasta vazia, ela conterá os arquivos criados nas próximas etapas.

2 - Crie um arquivo chamado `project.json` com o seguinte conteúdo (ajuste o número da porta e `rootDirectory` conforme apropriado):

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

3 - Crie um arquivo chamado `Startup.cs` com o seguinte código:

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

4 - Abra um prompt de comando, navegue até a pasta e execute:

    dnvm use 1.0.0-rc1-final -r coreclr -p
    dnu restore

**Observação:**
Esses comandos precisam ser executados apenas uma vez.
Use `dnvm list` para verificar o número real da última versão instalada do núcleo CLR.

5 - Inicie o servidor com: `dnx web`. Os arquivos agora podem ser solicitados em `http://localhost:60000/path/to/file.ext`.

Para simplificar, supõe-se que os nomes dos arquivos sejam todos ASCII (para a parte do nome do arquivo no cabeçalho Content-Disposition) e os erros de acesso ao arquivo não são tratados.

