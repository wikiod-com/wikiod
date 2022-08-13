---
title: "Núcleo de .NET"
slug: "nucleo-de-net"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

.NET Core es una plataforma de desarrollo de propósito general mantenida por Microsoft y la comunidad .NET en GitHub. Es multiplataforma, compatible con Windows, macOS y Linux, y se puede usar en escenarios de dispositivo, nube e integrado/IoT.

Cuando piensa en .NET Core, debe pensar en lo siguiente (implementación flexible, multiplataforma, herramientas de línea de comandos, código abierto).

Otra gran cosa es que, incluso si es de código abierto, Microsoft lo apoya activamente.


Por sí mismo, .NET Core incluye un único modelo de aplicación, aplicaciones de consola, que es útil para herramientas, servicios locales y juegos basados ​​en texto. Se han creado modelos de aplicaciones adicionales sobre .NET Core para ampliar su funcionalidad, como:

* ASP.NET Núcleo
* Plataforma universal de Windows de Windows 10 (UWP)
* Xamarin.Forms

Además, .NET Core implementa la biblioteca estándar de .NET y, por lo tanto, es compatible con las bibliotecas estándar de .NET.

La biblioteca estándar de .NET es una especificación de API que describe el conjunto coherente de API de .NET que los desarrolladores pueden esperar en cada implementación de .NET. Las implementaciones de .NET deben implementar esta especificación para que se consideren compatibles con la biblioteca estándar de .NET y para admitir bibliotecas destinadas a la biblioteca estándar de .NET.

## Aplicación de consola básica
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

