---
title: ".NET Çekirdeği"
slug: "net-cekirdegi"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

.NET Core, Microsoft ve GitHub'daki .NET topluluğu tarafından sağlanan genel amaçlı bir geliştirme platformudur. Windows, macOS ve Linux'u destekleyen çapraz platformdur ve cihaz, bulut ve gömülü/IoT senaryolarında kullanılabilir.

.NET Core denilince akla şunlar gelmelidir (esnek dağıtım, çapraz platform, komut satırı araçları, açık kaynak).

Bir başka harika şey de, açık kaynak olsa bile Microsoft'un aktif olarak desteklemesidir.


.NET Core, kendi başına, araçlar, yerel hizmetler ve metin tabanlı oyunlar için kullanışlı olan tek bir uygulama modeli olan konsol uygulamaları içerir. İşlevlerini genişletmek için .NET Core üzerine ek uygulama modelleri oluşturulmuştur, örneğin:

* ASP.NET Çekirdeği
* Windows 10 Evrensel Windows Platformu (UWP)
* Xamarin.Formlar

Ayrıca .NET Core, .NET Standard Library'yi uygular ve bu nedenle .NET Standard Libraries'i destekler.

.NET Standard Library, geliştiricilerin her .NET uygulamasında bekleyebilecekleri tutarlı .NET API kümesini açıklayan bir API özelliğidir. .NET uygulamalarının, .NET Standart Kitaplığı uyumlu olarak değerlendirilmesi ve .NET Standart Kitaplığı hedefleyen kitaplıkları desteklemesi için bu özelliği uygulaması gerekir.

## Temel Konsol Uygulaması
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

