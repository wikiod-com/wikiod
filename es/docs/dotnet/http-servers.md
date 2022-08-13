---
title: "servidores HTTP"
slug: "servidores-http"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Servidor de archivos HTTP básico de solo lectura (HttpListener)
**Notas:**

Este ejemplo debe ejecutarse en modo administrativo.

Solo se admite un cliente simultáneo.

Para simplificar, se supone que los nombres de archivo son todos ASCII (para la parte _filename_ en el encabezado _Content-Disposition_) y no se manejan los errores de acceso a archivos.


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

## Servidor de archivos HTTP básico de solo lectura (ASP.NET Core)
1 - Cree una carpeta vacía, contendrá los archivos creados en los siguientes pasos.

2 - Cree un archivo llamado `project.json` con el siguiente contenido (ajuste el número de puerto y `rootDirectory` según corresponda):

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

3 - Cree un archivo llamado `Startup.cs` con el siguiente código:

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

4 - Abra un símbolo del sistema, navegue hasta la carpeta y ejecute:

    dnvm use 1.0.0-rc1-final -r coreclr -p
    dnu restore

**Nota:**
Estos comandos deben ejecutarse solo una vez.
Use `dnvm list` para comprobar el número real de la última versión instalada del núcleo CLR.

5 - Inicie el servidor con: `dnx web`. Los archivos ahora se pueden solicitar en `http://localhost:60000/path/to/file.ext`.

Para simplificar, se supone que los nombres de archivo son todos ASCII (para la parte del nombre de archivo en el encabezado Content-Disposition) y no se manejan los errores de acceso a archivos.

