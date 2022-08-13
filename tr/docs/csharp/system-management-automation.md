---
title: "Sistem.Yönetim.Otomasyon"
slug: "sistemyonetimotomasyon"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

> *System.Management.Automation* ad alanı,
> Windows PowerShell.

[System.Management.Automation][1] Microsoft'tan bir uzantı kitaplığıdır ve NuGet paket yöneticisi veya paket yöneticisi konsolu aracılığıyla Visual Studio projelerine eklenebilir.

<saat>

[![nuget-ui][2]][2]

<saat>

    PM> Install-Package System.Management.Automation

[1]: https://www.nuget.org/packages/System.Management.Automation
[2]: http://i.stack.imgur.com/QJlb8.png

## Basit senkronize ardışık düzeni çağır
Geçerli tarih ve saati alın.

    public class Program
    {
        static void Main()
        {
            // create empty pipeline
            PowerShell ps = PowerShell.Create();

            // add command
            ps.AddCommand("Get-Date");

            // run command(s)
            Console.WriteLine("Date: {0}", ps.Invoke().First());

            Console.ReadLine();
        }
    }

[![buraya resim açıklamasını girin][1]][1]


[1]: http://i.stack.imgur.com/x2IIE.png

