---
title: "System.Management.Automation"
slug: "systemmanagementautomation"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

> The *System.Management.Automation* namespace is the root namespace for
> Windows PowerShell.

[System.Management.Automation][1] is an extension library from Microsoft and it can be added to Visual Studio projects via NuGet package manager or package manager console.

<hr>

[![nuget-ui][2]][2]

<hr>

    PM> Install-Package System.Management.Automation

  [1]: https://www.nuget.org/packages/System.Management.Automation
  [2]: http://i.stack.imgur.com/QJlb8.png

## Invoke simple synchronous pipeline
Get the current date and time.

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

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/x2IIE.png

