---
title: "Diagnóstico do sistema"
slug: "diagnostico-do-sistema"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Cronômetro
Este exemplo mostra como o `Stopwatch` pode ser usado para comparar um bloco de código.

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

## Executar comandos do shell


## Envia o comando para o CMD e recebe a saída


Este método permite que um `command` seja enviado para `Cmd.exe` e retorna a saída padrão (incluindo o erro padrão) como uma string:

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

**Resultado**

> string currentUsers = "USERNAME SESSIONNAME ID ESTADO IDLE TIME LOGON TIME Joe.Bloggs ica-cgp#0 2 Ativo 24692+13:29 25/07/2016 07:50 Jim.McFlannegan ica-cgp#1 3 Ativo . 25/07 /2016 08:33 Andy.McAnderson ica-cgp#2 4 Ativo . 25/07/2016 08:54 John.Smith ica-cgp#4 5 Ativo 14 25/07/2016 08:57 Bob.Bobbington ica-cgp# 5 6 Ativo 24692+13:29 25/07/2016 09:05 Tim.Tom ica-cgp#6 7 Ativo . 25/07/2016 09:08 Bob.Joges ica-cgp#7 8 Ativo 24692+13:29 25/07/2016 09:13"

_____________

Em algumas ocasiões, o acesso ao servidor em questão pode ser restrito a determinados usuários. Se você tiver as credenciais de login para este usuário, é possível enviar consultas com este método:

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

Obtendo a senha:

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

Ambos os métodos acima retornarão uma concatenação de STDOUT e STDERR, pois `OutputDataReceived` e `ErrorDataReceived` são ambos anexados à mesma variável - `cmdOut`.

