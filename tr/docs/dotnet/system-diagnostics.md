---
title: "System.Diagnostics"
slug: "systemdiagnostics"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Kronometre
Bu örnek, bir kod bloğunu kıyaslamak için "Kronometre"nin nasıl kullanılabileceğini gösterir.

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

## Kabuk komutlarını çalıştır


## Komutu CMD'ye Gönder ve Çıktı Al


Bu yöntem, bir "komut"un "Cmd.exe"ye gönderilmesine izin verir ve standart çıktıyı (standart hata dahil) bir dize olarak döndürür:

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

**Kullanım**

    var servername = "SVR-01.domain.co.za";
    var currentUsers = SendCommand($"/C QUERY USER /SERVER:{servername}")

**Çıktı**

> string currentUsers = "KULLANICI ADI OTURUM ADI KİMLİK DURUM BOŞLUK ZAMANI OTURUM ZAMANI Joe.Bloggs ica-cgp#0 2 Aktif 24692+13:29 25/07/2016 07:50 Jim.McFlannegan ica-cgp#1 3 Aktif . 25/07 /2016 08:33 Andy.McAnderson ica-cgp#2 4 Aktif .25/07/2016 08:54 John.Smith ica-cgp#4 5 Aktif 14 25/07/2016 08:57 Bob.Bobbington ica-cgp# 5 6 Aktif 24692+13:29 25/07/2016 09:05 Tim.Tom ica-cgp#6 7 Aktif .25/07/2016 09:08 Bob.Joges ica-cgp#7 8 Aktif 24692+13:29 25/07/2016 09:13"

______________

Bazı durumlarda, söz konusu sunucuya erişim belirli kullanıcılarla sınırlandırılabilir. Bu kullanıcı için oturum açma kimlik bilgileriniz varsa, bu yöntemle sorgu göndermek mümkündür:

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

Şifreyi almak:

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

**Notlar**

Yukarıdaki yöntemlerin her ikisi de, "OutputDataReceived" ve "ErrorDataReceived" öğelerinin her ikisi de aynı değişkene - "cmdOut" eklendiğinden, STDOUT ve STDERR'nin bir birleşimini döndürür.

