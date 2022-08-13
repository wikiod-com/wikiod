---
title: "Création d'une application console à l'aide d'un éditeur de texte brut et du compilateur C# (csc.exe)"
slug: "creation-dune-application-console-a-laide-dun-editeur-de-texte-brut-et-du-compilateur-c-cscexe"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Création d'une application console à l'aide d'un éditeur de texte brut et du compilateur C#
Afin d'utiliser un éditeur de texte brut pour créer une application console écrite en C#, vous aurez besoin du compilateur C#. Le compilateur C# (csc.exe) se trouve à l'emplacement suivant :
`%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe`

**N.B.** Selon la version de .NET Framework installée sur votre système, vous devrez peut-être modifier le chemin ci-dessus en conséquence.


----------

<h1>Enregistrer le code</h1>
Le but de cette rubrique n'est pas de vous apprendre <i>comment</i> écrire une application Console, mais de vous apprendre à <i>compiler</i> une [pour produire un seul fichier exécutable], sans rien autre que le compilateur C# et tout éditeur de texte brut (tel que le Bloc-notes).
<br/><br/>

1. Ouvrez la boîte de dialogue Exécuter en utilisant le raccourci clavier <kbd>Touche Windows</kbd> + <kbd>R</kbd>
2. Tapez `notepad`, puis appuyez sur <kbd>Entrée</kbd>
3. Collez l'exemple de code ci-dessous dans le Bloc-notes
4. Enregistrez le fichier sous `ConsoleApp.cs`, en allant dans **Fichier** → **Enregistrer sous...**, puis en saisissant `ConsoleApp.cs` dans le champ de texte 'Nom de fichier', puis en sélectionnant ` Tous les fichiers` comme type de fichier.
5. Cliquez sur "Enregistrer"

<h1>Compilation du code source</h1>
1. Ouvrez la boîte de dialogue Exécuter en utilisant <kbd>Clé Windows</kbd> + <kbd>R</kbd><br/>
2. Saisissez :

    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe /t:exe /out:"C:\Users\yourUserName\Documents\ConsoleApp.exe" "C:\Users\yourUserName\Documents\ConsoleApp.cs"

Maintenant, revenez à l'endroit où vous avez initialement enregistré votre fichier `ConsoleApp.cs`. Vous devriez maintenant voir un fichier exécutable (`ConsoleApp.exe`). Double-cliquez sur `ConsoleApp.exe` pour l'ouvrir.

C'est ça! Votre application console a été compilée. Un fichier exécutable a été créé et vous disposez maintenant d'une application de console fonctionnelle.


    using System;
    
    namespace ConsoleApp
    {
        class Program
        {
            private static string input = String.Empty;
    
            static void Main(string[] args)
            {
                goto DisplayGreeting;
    
                DisplayGreeting:
                {
                    Console.WriteLine("Hello! What is your name?");
    
                    input = Console.ReadLine();
    
                    if (input.Length >= 1)
                    {
                        Console.WriteLine(
                            "Hello, " + 
                            input + 
                            ", enter 'Exit' at any time to exit this app.");
    
                        goto AwaitFurtherInstruction;
                    }
                    else
                    {
                        goto DisplayGreeting;
                    }
                }
    
                AwaitFurtherInstruction:
                {
                    input = Console.ReadLine();
    
                    if(input.ToLower() == "exit")
                    {
                        input = String.Empty;
    
                        Environment.Exit(0);
                    }
                    else
                    {
                        goto AwaitFurtherInstruction;
                    }
                }
            }
        }
    }

