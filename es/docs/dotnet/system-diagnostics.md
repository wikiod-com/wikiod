---
title: "Diagnostico del sistema"
slug: "diagnostico-del-sistema"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Cronómetro
Este ejemplo muestra cómo se puede usar `Stopwatch` para comparar un bloque de código.

    using System;            
    using System.Diagnostics;
            
    public class Benchmark : IDisposable
    {
        private Stopwatch sw;
    
        public Benchmark()
        {
            sw = Stopwatch.StartNew();
        }
    
        public void Dispose()
        {
            sw.Stop();
            Console.WriteLine(sw.Elapsed);
        }
    }
    
    public class Program
    {
        public static void Main()
        {
            using (var bench = new Benchmark())
            {
                Console.WriteLine("Hello World");
            }
        }
    }

## Ejecutar comandos de shell


## Enviar comando a CMD y recibir salida


Este método permite enviar un `comando` a `Cmd.exe` y devuelve la salida estándar (incluido el error estándar) como una cadena:

    private static string SendCommand(string command)
    {
        var cmdOut = string.Empty;
        
        var startInfo = new ProcessStartInfo("cmd", command)
        {
            WorkingDirectory = @"C:\Windows\System32", // Directory to make the call from
            WindowStyle = ProcessWindowStyle.Hidden,   // Hide the window
            UseShellExecute = false,                   // Do not use the OS shell to start the process
            CreateNoWindow = true,                     // Start the process in a new window 
            RedirectStandardOutput = true,             // This is required to get STDOUT
            RedirectStandardError = true               // This is required to get STDERR
        };

        var p = new Process {StartInfo = startInfo};

        p.Start();

        p.OutputDataReceived += (x, y) => cmdOut += y.Data;
        p.ErrorDataReceived += (x, y) => cmdOut += y.Data;
        p.BeginOutputReadLine();
        p.BeginErrorReadLine();
        p.WaitForExit();
        return cmdOut;
    }

**Uso**

    var servername = "SVR-01.domain.co.za";
    var currentUsers = SendCommand($"/C QUERY USER /SERVER:{servername}")

**Producción**

> string currentUsers = "NOMBRE DE USUARIO NOMBRE DE SESIÓN ID ESTADO TIEMPO DE INACTIVIDAD HORA DE INICIO DE SESIÓN Joe.Bloggs ica-cgp#0 2 Activo 24692+13:29 25/07/2016 07:50 Jim.McFlannegan ica-cgp#1 3 Activo . 25/07 /2016 08:33 Andy.McAnderson ica-cgp#2 4 Activo 25/07/2016 08:54 John.Smith ica-cgp#4 5 Activo 14 25/07/2016 08:57 Bob.Bobbington ica-cgp# 5 6 Activo 24692+13:29 25/07/2016 09:05 Tim.Tom ica-cgp#6 7 Activo 25/07/2016 09:08 Bob.Joges ica-cgp#7 8 Activo 24692+13:29 25/07/2016 09:13"

_____________

En algunas ocasiones, el acceso al servidor en cuestión puede estar restringido a determinados usuarios. Si tiene las credenciales de inicio de sesión de este usuario, es posible enviar consultas con este método:

    private static string SendCommand(string command)
    {
        var cmdOut = string.Empty;
        
        var startInfo = new ProcessStartInfo("cmd", command)
        {
            WorkingDirectory = @"C:\Windows\System32",
            WindowStyle = ProcessWindowStyle.Hidden,    // This does not actually work in conjunction with "runas" - the console window will still appear!
            UseShellExecute = false,
            CreateNoWindow = true,
            RedirectStandardOutput = true, 
            RedirectStandardError = true,

            Verb = "runas",
            Domain = "doman1.co.za",
            UserName = "administrator",
            Password = GetPassword()
        };

        var p = new Process {StartInfo = startInfo};

        p.Start();

        p.OutputDataReceived += (x, y) => cmdOut += y.Data;
        p.ErrorDataReceived += (x, y) => cmdOut += y.Data;
        p.BeginOutputReadLine();
        p.BeginErrorReadLine();
        p.WaitForExit();
        return cmdOut;
    }

Obtener la contraseña:

    static SecureString GetPassword()
    {
        var plainText = "password123";
        var ss = new SecureString();
        foreach (char c in plainText)
        {
            ss.AppendChar(c);
        }

        return ss;
    }

**Notas**

Ambos métodos anteriores devolverán una concatenación de STDOUT y STDERR, ya que `OutputDataReceived` y `ErrorDataReceived` se agregan a la misma variable: `cmdOut`.

