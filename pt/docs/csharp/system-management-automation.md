---
title: "System.Management.Automation"
slug: "systemmanagementautomation"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

> O namespace *System.Management.Automation* é o namespace raiz para
> Windows PowerShell.

[System.Management.Automation][1] é uma biblioteca de extensão da Microsoft e pode ser adicionada a projetos do Visual Studio por meio do gerenciador de pacotes NuGet ou do console do gerenciador de pacotes.

<hr>

[![nuget-ui][2]][2]

<hr>

    PM> Install-Package System.Management.Automation

[1]: https://www.nuget.org/packages/System.Management.Automation
[2]: http://i.stack.imgur.com/QJlb8.png

## Invoca pipeline síncrono simples
Obtenha a data e hora atuais.

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

[![digite a descrição da imagem aqui][1]][1]


[1]: http://i.stack.imgur.com/x2IIE.png

