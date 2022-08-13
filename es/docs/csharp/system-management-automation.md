---
title: "Automatización.de.Gestión.de.Sistemas"
slug: "automatizaciondegestiondesistemas"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

> El espacio de nombres *System.Management.Automation* es el espacio de nombres raíz para
> Windows PowerShell.

[System.Management.Automation][1] es una biblioteca de extensión de Microsoft y se puede agregar a los proyectos de Visual Studio a través del administrador de paquetes NuGet o la consola del administrador de paquetes.

<hr>

[![nuget-ui][2]][2]

<hr>

    PM> Install-Package System.Management.Automation

[1]: https://www.nuget.org/packages/System.Management.Automation
[2]: http://i.stack.imgur.com/QJlb8.png

## Invocar canalización síncrona simple
Obtener la fecha y hora actual.

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

[![ingrese la descripción de la imagen aquí][1]][1]


[1]: http://i.stack.imgur.com/x2IIE.png

