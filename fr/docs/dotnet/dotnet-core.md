---
title: ".NET Core"
slug: "net-core"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

.NET Core est une plate-forme de développement à usage général gérée par Microsoft et la communauté .NET sur GitHub. Il est multiplateforme, prend en charge Windows, macOS et Linux, et peut être utilisé dans des scénarios d'appareils, de cloud et intégrés/IoT.

Lorsque vous pensez à .NET Core, les éléments suivants doivent vous venir à l'esprit (déploiement flexible, multiplateforme, outils de ligne de commande, open source).

Une autre grande chose est que même si c'est open source, Microsoft le soutient activement.


En soi, .NET Core inclut un modèle d'application unique - les applications de console - qui est utile pour les outils, les services locaux et les jeux textuels. Des modèles d'application supplémentaires ont été construits sur .NET Core pour étendre ses fonctionnalités, telles que :

* Noyau ASP.NET
* Plate-forme Windows universelle Windows 10 (UWP)
* Xamarin.Forms

En outre, .NET Core implémente la bibliothèque standard .NET et prend donc en charge les bibliothèques standard .NET.

La bibliothèque standard .NET est une spécification d'API qui décrit l'ensemble cohérent d'API .NET auquel les développeurs peuvent s'attendre dans chaque implémentation .NET. Les implémentations .NET doivent implémenter cette spécification pour être considérées comme conformes à la bibliothèque standard .NET et pour prendre en charge les bibliothèques qui ciblent la bibliothèque standard .NET.

## Application console de base
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

