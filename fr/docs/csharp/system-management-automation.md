---
title: "System.Management.Automation"
slug: "systemmanagementautomation"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

> L'espace de noms *System.Management.Automation* est l'espace de noms racine pour
> WindowsPowerShell.

[System.Management.Automation][1] est une bibliothèque d'extensions de Microsoft et elle peut être ajoutée aux projets Visual Studio via le gestionnaire de packages NuGet ou la console du gestionnaire de packages.

<h>

[![nuget-ui][2]][2]

<h>

    PM> Install-Package System.Management.Automation

[1] : https://www.nuget.org/packages/System.Management.Automation
[2] : http://i.stack.imgur.com/QJlb8.png

## Invoquer un pipeline synchrone simple
Obtenez la date et l'heure actuelles.

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

[![entrez la description de l'image ici][1]][1]


[1] : http://i.stack.imgur.com/x2IIE.png

