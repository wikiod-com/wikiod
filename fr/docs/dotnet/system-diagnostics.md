---
title: "Diagnostique du systeme"
slug: "diagnostique-du-systeme"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Chronomètre
Cet exemple montre comment `Stopwatch` peut être utilisé pour comparer un bloc de code.

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

## Exécuter des commandes shell


## Envoyer la commande à CMD et recevoir la sortie


Cette méthode permet d'envoyer une "commande" à "Cmd.exe" et renvoie la sortie standard (y compris l'erreur standard) sous forme de chaîne :

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

**Usage**

    var servername = "SVR-01.domain.co.za";
    var currentUsers = SendCommand($"/C QUERY USER /SERVER:{servername}")

**Production**

> string currentUsers = "NOM D'UTILISATEUR ID DE SESSION ÉTAT TEMPS D'INACTIVITÉ TEMPS DE CONNEXION Joe.Bloggs ica-cgp#0 2 Actif 24692+13:29 25/07/2016 07:50 Jim.McFlannegan ica-cgp#1 3 Actif . 25/07 /2016 08:33 Andy.McAnderson ica-cgp#2 4 Actif 25/07/2016 08:54 John.Smith ica-cgp#4 5 Actif 14 25/07/2016 08:57 Bob.Bobbington ica-cgp# 5 6 Actif 24692+13:29 25/07/2016 09:05 Tim.Tom ica-cgp#6 7 Actif 25/07/2016 09:08 Bob.Joges ica-cgp#7 8 Actif 24692+13:29 25/07/2016 09:13"

_____________

À certaines occasions, l'accès au serveur en question peut être limité à certains utilisateurs. Si vous disposez des identifiants de connexion pour cet utilisateur, il est alors possible d'envoyer des requêtes avec cette méthode :

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

Obtention du mot de passe :

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

**Remarques**

Les deux méthodes ci-dessus renverront une concaténation de STDOUT et STDERR, car `OutputDataReceived` et `ErrorDataReceived` sont tous deux ajoutés à la même variable - `cmdOut`.

