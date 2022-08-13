---
title: "Premiers pas avec le langage C#"
slug: "premiers-pas-avec-le-langage-c"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Création d'une nouvelle application console (Visual Studio)
1. Ouvrez Visual Studio
2. Dans la barre d'outils, allez dans **Fichier** → **Nouveau projet**
3. Sélectionnez le type de projet **Application console**
4. Ouvrez le fichier "Program.cs" dans l'explorateur de solutions
5. Ajoutez le code suivant à `Main()` :


    public class Program
    {
        public static void Main()
        {
            // Prints a message to the console.
            System.Console.WriteLine("Hello, World!");

            System.Console.ReadKey();
        }
    }

6. Dans la barre d'outils, cliquez sur **Debug** -> **Start Debugging** ou appuyez sur **F5** ou **ctrl + F5** (fonctionnement sans débogueur) pour exécuter le programme.


[Démo en direct sur ideone][1]

-------------------------------------------------- ---------------------------------

# Explication

- `class Program` est une déclaration de classe. La classe "Programme" contient les définitions de données et de méthodes utilisées par votre programme. Les classes contiennent généralement plusieurs méthodes. Les méthodes définissent le comportement de la classe. Cependant, la classe `Program` n'a qu'une seule méthode : `Main`.

- `static void Main()` définit la méthode `Main`, qui est le point d'entrée pour tous les programmes C#. La méthode `Main` indique ce que fait la classe lorsqu'elle est exécutée. Une seule méthode `Main` est autorisée par classe.

- La méthode `System.Console.WriteLine("Hello, world!");` imprime une donnée donnée (dans cet exemple, `Hello, world!`) en tant que sortie dans la fenêtre de la console.

- `System.Console.ReadKey()`, garantit que le programme ne se fermera pas immédiatement après l'affichage du message. Pour ce faire, il attend que l'utilisateur appuie sur une touche du clavier. Toute pression sur une touche de la part de l'utilisateur mettra fin au programme. Le programme se termine lorsqu'il a terminé la dernière ligne de code dans la méthode `main()`.

-------------------------------------------------- ---------------------------------

# En utilisant la ligne de commande

Pour compiler via la ligne de commande, utilisez soit `MSBuild` ou `csc.exe` _(le compilateur C#)_, tous deux faisant partie de [Microsoft Build Tools](https://www.visualstudio.com/downloads/download-visual- studio-vs#d-build-tools).

Pour compiler cet exemple, exécutez la commande suivante dans le même répertoire où se trouve "HelloWorld.cs" :

<!-- langue : lang-none -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs

Il est également possible que vous ayez deux méthodes principales dans une même application. Dans ce cas, vous devez indiquer au compilateur quelle méthode principale exécuter en tapant la commande suivante dans la ** console **. (supposons que la classe `ClassA` a également une méthode principale dans le même fichier `HelloWorld.cs` dans HelloWorld espace de noms)

<!-- langue : lang-none -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs /main:HelloWorld.ClassA 

où HelloWorld est un espace de noms


***Remarque** : Il s'agit du chemin d'accès où **.NET framework v4.0** se trouve en général. Modifiez le chemin en fonction de votre version .NET. De plus, le répertoire peut être **framework** au lieu de **framework64** si vous utilisez le .NET Framework 32 bits. À partir de l'invite de commande Windows, vous pouvez répertorier tous les chemins du framework csc.exe en exécutant les commandes suivantes (la première pour les frameworks 32 bits) :*

    dir %WINDIR%\Microsoft.NET\Framework\csc.exe /s/b
    dir %WINDIR%\Microsoft.NET\Framework64\csc.exe /s/b

![Compilation du fichier .cs][2]

Il devrait maintenant y avoir un fichier exécutable nommé "HelloWorld.exe" dans le même répertoire. Pour exécuter le programme à partir de l'invite de commande, saisissez simplement le nom de l'exécutable et appuyez sur <kbd>Entrée</kbd> comme suit :

<!-- langue : lang-none -->
    HelloWorld.exe

Cela produira :

> Bonjour, monde !

![Exécution du fichier exe dans la console][3]

Vous pouvez également double-cliquer sur l'exécutable et lancer une nouvelle fenêtre de console avec le message "**Hello, world!**"

![Lancer l'exécutable et utiliser le double clic][4]

[1] : https://ideone.com/3OhmnG
[2] : http://i.stack.imgur.com/xT8kk.png
[3] : http://i.stack.imgur.com/x0Fek.png
[4] : http://i.stack.imgur.com/qstu1.png

## Création d'un nouveau projet dans Visual Studio (application console) et exécution en mode débogage
1. **Téléchargez et installez [Visual Studio][1]**. Visual Studio peut être téléchargé depuis [VisualStudio.com][2]. L'édition communautaire est suggérée, d'abord parce qu'elle est gratuite, et ensuite parce qu'elle comprend toutes les fonctionnalités générales et peut être étendue davantage.

2. **Ouvrez Visual Studio.**
3. **Bienvenue.** Accédez à **Fichier → **Nouveau** → Projet**.
    [![Microsoft Visual Studio - File Menu][3]][3]

4. Cliquez sur **Modèles** → **Visual C#** → **Application console**

    [![Microsoft Visual Studio - New Project window][4]][4]

5. **Après avoir sélectionné l'application console,** Entrez un nom pour votre projet et un emplacement pour enregistrer et appuyez sur <kbd>OK</kbd>. Ne vous souciez pas du nom de la solution.

6. **Projet créé**. Le projet nouvellement créé ressemblera à :

    [![Microsoft Visual Studio - c# Default Project][5]][5]

    _(Always use descriptive names for projects so that they can easily be distinguished from other projects.  It is recommended not to use spaces in project or class name.)_

7. **Écrivez du code.** Vous pouvez maintenant mettre à jour votre `Program.cs` pour présenter "Hello world!" à l'utilisateur.

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                }
            }
        }

    Add the following two lines to the `public static void Main(string[] args)` object in `Program.cs`: (make sure it's inside the braces)

        Console.WriteLine("Hello world!");
        Console.Read();

    **Why** `Console.Read()`__?__ The first line prints out the text "Hello world!" to the console, and the second line waits for a single character to be entered; in effect, this causes the program to pause execution so that you're able to see the output while debugging.  Without `Console.Read();`, when you start debugging the application it will just print "Hello world!" to the console and then immediately close.  Your code window should now look like the following:

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                    Console.WriteLine("Hello world!");
                    Console.Read();
                }
            }
        }

8. **Déboguez votre programme.** Appuyez sur le bouton Démarrer de la barre d'outils en haut de la fenêtre [![Bouton Démarrer le débogage][6]][6] ou appuyez sur <kbd>F5</kbd> sur votre clavier pour exécuter votre application. Si le bouton n'est pas présent, vous pouvez exécuter le programme depuis le menu du haut : **Debug → Start Debugging**. Le programme compilera puis ouvrira une fenêtre de console. Cela devrait ressembler à la capture d'écran suivante :

[![Console exécutant l'application Hello World][7]][7]

9. **Arrêtez le programme.** Pour fermer le programme, appuyez simplement sur n'importe quelle touche de votre clavier. Le `Console.Read()` que nous avons ajouté était dans le même but. Une autre façon de fermer le programme est d'aller dans le menu où se trouvait le bouton <kbd>Démarrer</kbd> et de cliquer sur le bouton <kbd>Arrêter</kbd>.

     


[1] : https://www.visualstudio.com/products/vs-2015-product-editions
[2] : http://www.visualstudio.com
[3] : http://i.stack.imgur.com/fpvTX.png
[4] : http://i.stack.imgur.com/kKGls.png
[5] : http://i.stack.imgur.com/WVkeF.png
[6] : https://i.stack.imgur.com/odDu6.png
[7] : http://i.stack.imgur.com/ZD5MF.png

## Création d'un nouveau programme en Mono
Installez d'abord [Mono][1] en suivant les instructions d'installation de la plate-forme de votre choix, comme décrit dans leur [section d'installation][2].

Mono est disponible pour Mac OS X, Windows et Linux.

Une fois l'installation terminée, créez un fichier texte, nommez-le "HelloWorld.cs" et copiez-y le contenu suivant :

    public class Program
    {
        public static void Main()
        {
            System.Console.WriteLine("Hello, world!");
            System.Console.WriteLine("Press any key to exit..");
            System.Console.Read();
        }
    }


Si vous utilisez Windows, exécutez l'invite de commande Mono incluse dans l'installation de Mono et assurez-vous que les variables d'environnement nécessaires sont définies. Si sur Mac ou Linux, ouvrez un nouveau terminal.

Pour compiler le fichier nouvellement créé, exécutez la commande suivante dans le répertoire contenant `HelloWorld.cs` :

<!-- langue : lang-none -->
    mcs -out:HelloWorld.exe HelloWorld.cs
 

Le `HelloWorld.exe` résultant peut alors être exécuté avec :
 
<!-- langue : lang-none -->
    mono HelloWorld.exe
 
qui produira la sortie :
 
 
<!-- langue : lang-none -->
    Hello, world!   
    Press any key to exit..

 
[1] : http://www.mono-project.com/
[2] : http://www.mono-project.com/docs/getting-started/install/

## Création d'un nouveau programme à l'aide de .NET Core
Installez d'abord le [**.NET Core SDK**][1] en suivant les instructions d'installation de la plate-forme de votre choix :

- [Windows][2]
- [OSX][3]
- [Linux][4]
- [Docker][5]

Une fois l'installation terminée, ouvrez une invite de commande ou une fenêtre de terminal.

1. Créez un nouveau répertoire avec `mkdir hello_world` et accédez au répertoire nouvellement créé avec `cd hello_world`.

2. Créez une nouvelle application console avec `dotnet new console`.
Cela produira deux fichiers :

    - **hello_world.csproj**

          <Project Sdk="Microsoft.NET.Sdk">

            <PropertyGroup>
              <OutputType>Exe</OutputType>
              <TargetFramework>netcoreapp1.1</TargetFramework>
            </PropertyGroup>

          </Project>
          
    - **Program.cs**

          using System;
        
          namespace hello_world
          {
              class Program
              {
                  static void Main(string[] args)
                  {
                      Console.WriteLine("Hello World!");
                  }
              }
          }

3. Restaurez les packages nécessaires avec `dotnet restore`.

4. *Facultatif* Générez l'application avec `dotnet build` pour Debug ou `dotnet build -c Release` pour Release. `dotnet run` exécutera également le compilateur et générera des erreurs de construction, le cas échéant.

5. Exécutez l'application avec `dotnet run` pour Debug ou `dotnet run .\bin\Release\netcoreapp1.1\hello_world.dll` pour Release.

-------------------------------------------------- ---------------------------------

Sortie d'invite de commande
---------------------
[![entrez la description de l'image ici][6]][6]


[1] : https://docs.microsoft.com/en-us/dotnet/articles/core/
[2] : https://www.microsoft.com/net/core#windows
[3] : https://www.microsoft.com/net/core#macos
[4] : https://www.microsoft.com/net/core#linuxubuntu
[5] : https://www.microsoft.com/net/core#dockercmd
[6] : https://i.stack.imgur.com/arqCl.png


## Création d'une nouvelle requête à l'aide de LinqPad
LinqPad est un excellent outil qui vous permet d'apprendre et de tester les fonctionnalités des langages .Net (C#, F# et VB.Net.)

1. Installez [LinqPad][1]
2. Créez une nouvelle requête (<kbd>Ctrl</kbd> + <kbd>N</kbd>)
[![entrez la description de l'image ici][2]][2]
3. Sous langage, sélectionnez "Instructions C#"
[![entrez la description de l'image ici][3]][3]
4. Tapez le code suivant et appuyez sur Exécuter (<kbd>F5</kbd>)

        string hw = "Hello World";

        hw.Dump(); //or Console.WriteLine(hw);
[![entrez la description de l'image ici][4]][4]

5. Vous devriez voir "Hello World" imprimé dans l'écran des résultats.
[![entrez la description de l'image ici][5]][5]
6. Maintenant que vous avez créé votre premier programme .Net, allez voir les exemples inclus dans LinqPad via le navigateur "Samples". Il existe de nombreux exemples intéressants qui vous montreront de nombreuses fonctionnalités différentes des langages .Net.
[![entrez la description de l'image ici][6]][6]

**Remarques:**
1. Si vous cliquez sur "IL", vous pouvez inspecter le code IL généré par votre code .net. C'est un excellent outil d'apprentissage.
[![entrez la description de l'image ici][7]][7]
2. Lorsque vous utilisez `LINQ to SQL` ou `Linq to Entities`, vous pouvez inspecter le SQL généré, ce qui est un autre excellent moyen d'en savoir plus sur LINQ.


[1] : http://www.linqpad.net/
[2] : http://i.stack.imgur.com/D0tSi.png
[3] : http://i.stack.imgur.com/kC5Ur.jpg
[4] : http://i.stack.imgur.com/LO4kD.jpg
[5] : http://i.stack.imgur.com/GzsrS.jpg
[6] : http://i.stack.imgur.com/yucuf.jpg
[7] : http://i.stack.imgur.com/XPumO.jpg

## Création d'un nouveau projet à l'aide de Xamarin Studio
1. Téléchargez et installez [Communauté Xamarin Studio][1].
2. Ouvrez Xamarin Studio.
3. Cliquez sur **Fichier** → **Nouveau** → **Solution**.

[![Création d'un nouveau projet dans Xamarin Studio][2]][2]

4. Cliquez sur **.NET** → **Projet console** et choisissez **C#**.
5. Cliquez sur <kbd>Suivant</kbd> pour continuer.

[![Choisir un modèle pour un nouveau projet][3]][3]
 
6. Saisissez le **Nom du projet** et <kbd>Parcourir...</kbd> pour un **Emplacement** à enregistrer, puis cliquez sur <kbd>Créer</kbd>.

[![Nom et emplacement du projet][4]][4]

7. Le projet nouvellement créé ressemblera à :

[![entrez la description de l'image ici][5]][5]

8. Voici le code dans l'éditeur de texte :


    using System;
    
    namespace FirstCsharp
    {
        public class MainClass
        {
            public static void Main(string[] args)
            {
                Console.WriteLine("Hello World!");
                Console.ReadLine();
            }
        }
    }

9. Pour exécuter le code, appuyez sur <kbd>F5</kbd> ou cliquez sur le **bouton de lecture** comme indiqué ci-dessous :

[![Exécuter le code][6]][6]

10. Voici la sortie :

[![sortie][7]][7]


[1] : https://store.xamarin.com/
[2] : http://i.stack.imgur.com/hHjMM.png
[3] : http://i.stack.imgur.com/s58Ju.png
[4] : http://i.stack.imgur.com/lrK8L.png
[5] : http://i.stack.imgur.com/vva82.png
[6] : http://i.stack.imgur.com/6q4ZN.png
[7] : http://i.stack.imgur.com/cqBsK.png

