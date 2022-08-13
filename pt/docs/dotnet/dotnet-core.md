---
title: ".NET Core"
slug: "net-core"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

O .NET Core é uma plataforma de desenvolvimento de uso geral mantida pela Microsoft e pela comunidade .NET no GitHub. É multiplataforma, compatível com Windows, macOS e Linux, e pode ser usado em cenários de dispositivo, nuvem e incorporado/IoT.

Quando você pensa em .NET Core, o seguinte deve vir à mente (implantação flexível, plataforma cruzada, ferramentas de linha de comando, código aberto).

Outra grande coisa é que, mesmo que seja de código aberto, a Microsoft está apoiando ativamente.


Por si só, o .NET Core inclui um único modelo de aplicativo -- aplicativos de console -- que é útil para ferramentas, serviços locais e jogos baseados em texto. Modelos de aplicativos adicionais foram criados com base no .NET Core para estender sua funcionalidade, como:

* ASP.NET Core
* Plataforma Windows 10 Universal do Windows (UWP)
* Xamarin.Forms

Além disso, o .NET Core implementa a .NET Standard Library e, portanto, oferece suporte a .NET Standard Libraries.

A .NET Standard Library é uma especificação de API que descreve o conjunto consistente de APIs .NET que os desenvolvedores podem esperar em cada implementação .NET. As implementações .NET precisam implementar essa especificação para serem consideradas compatíveis com .NET Standard Library e para oferecer suporte a bibliotecas direcionadas à .NET Standard Library.

## Aplicativo de console básico
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("\nWhat is your name? ");
            var name = Console.ReadLine();
            var date = DateTime.Now;
            Console.WriteLine("\nHello, {0}, on {1:d} at {1:t}", name, date);
            Console.Write("\nPress any key to exit...");
            Console.ReadKey(true);
        }
    }

