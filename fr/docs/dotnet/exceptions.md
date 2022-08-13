---
title: "Exceptions"
slug: "exceptions"
draft: false
images: []
weight: 9760
type: docs
toc: true
---

Lié:

* [MSDN : Exceptions et gestion des exceptions (Guide de programmation C#)](https://msdn.microsoft.com/en-us/library/ms173160.aspx)
* [MSDN : Gestion et émission d'exceptions](https://msdn.microsoft.com/en-us/library/5b2yeyab.aspx)
* [MSDN : CA1031 : ne pas intercepter les types d'exceptions générales](https://msdn.microsoft.com/en-us/library/ms182137.aspx)
* [MSDN : try-catch (référence C#)](https://msdn.microsoft.com/en-us/library/0yd65esw.aspx)

## Attraper et relancer les exceptions interceptées
Lorsque vous souhaitez intercepter une exception et faire quelque chose, mais que vous ne pouvez pas continuer l'exécution du bloc de code en cours à cause de l'exception, vous pouvez renvoyer l'exception au prochain gestionnaire d'exceptions dans la pile des appels. Il y a de bonnes et de mauvaises façons de le faire.

    private static void AskTheUltimateQuestion()
    {
        try
        {
            var x = 42;
            var y = x / (x - x); // will throw a DivideByZeroException

            // IMPORTANT NOTE: the error in following string format IS intentional
            // and exists to throw an exception to the FormatException catch, below
            Console.WriteLine("The secret to life, the universe, and everything is {1}", y); 
        }
        catch (DivideByZeroException)
        {
            // we do not need a reference to the exception
            Console.WriteLine("Dividing by zero would destroy the universe.");

            // do this to preserve the stack trace:
            throw;
        }
        catch (FormatException ex)
        {
            // only do this if you need to change the type of the Exception to be thrown 
            // and wrap the inner Exception

            // remember that the stack trace of the outer Exception will point to the
            // next line

            // you'll need to examine the InnerException property to get the stack trace
            // to the line that actually started the problem

            throw new InvalidOperationException("Watch your format string indexes.", ex);
        }
        catch (Exception ex)
        {
            Console.WriteLine("Something else horrible happened. The exception: " + ex.Message);

            // do not do this, because the stack trace will be changed to point to
            // this location instead of the location where the exception
            // was originally thrown:
            throw ex; 
        }
    }

    static void Main()
    {
        try
        {
            AskTheUltimateQuestion();
        }
        catch
        {
            // choose this kind of catch if you don't need any information about 
            // the exception that was caught

            // this block "eats" all exceptions instead of rethrowing them
        }
    }

Vous pouvez filtrer par type d'exception et même par propriétés d'exception (nouveau dans C # 6.0, un peu plus disponible dans VB.NET (citation nécessaire)):

[Documentation/C#/nouvelles fonctionnalités][1]


[1] : https://www.wikiod.com/fr/docs/c%23/24/c-sharp-6-0-features/46/exception-filters

## Utiliser un bloc finally
Le bloc `finally { ... }` d'un `try-finally` ou `try-catch-finally` s'exécutera toujours, qu'une exception se soit produite ou non (sauf lorsqu'un `StackOverflowException` a été lancé ou qu'un appel a été lancé été fait à `Environment.FailFast()`).

Il peut être utilisé pour libérer ou nettoyer les ressources acquises dans le bloc `try { ... }` en toute sécurité.

    Console.Write("Please enter a filename: ");
    string filename = Console.ReadLine();

    Stream fileStream = null;

    try
    {
        fileStream = File.Open(filename);
    }
    catch (FileNotFoundException)
    {
        Console.WriteLine("File '{0}' could not be found.", filename);
    }
    finally
    {
        if (fileStream != null)
        {
            fileStream.Dispose();
        }
    }


## Filtres d'exceptions
Depuis C# 6.0, les exceptions peuvent être filtrées à l'aide de l'opérateur "quand".

Ceci est similaire à l'utilisation d'un simple "si" mais ne déroule pas la pile si la condition à l'intérieur du "quand" n'est pas remplie.

__Exemple__
    
    try
    { 
      // ...
    }
    catch (Exception e) when (e.InnerException != null) // Any condition can go in here.
    {
      // ...
    }

Les mêmes informations peuvent être trouvées dans les [Fonctionnalités C# 6.0][1] ici : [Filtres d'exception][2]


[1] : https://www.wikiod.com/fr/docs/c%23/24/c-sharp-6-0-features
[2] : https://www.wikiod.com/fr/docs/c%23/24/c-sharp-6-0-features/46/exception-filters#t=201607211048375185447

## Relancer une exception dans un bloc catch
Dans un bloc `catch`, le mot-clé `throw` peut être utilisé seul, sans spécifier de valeur d'exception, pour *relancer* l'exception qui vient d'être interceptée. Le fait de relancer une exception permet à l'exception d'origine de remonter la chaîne de gestion des exceptions, en préservant sa pile d'appels ou les données associées :
 
    try {...}
    catch (Exception ex) {
      // Note: the ex variable is *not* used
      throw;
    }

Un anti-modèle courant consiste à "jeter ex" à la place, ce qui a pour effet de limiter la vue du prochain gestionnaire d'exceptions sur la trace de la pile :

    try {...}
    catch (Exception ex) {
      // Note: the ex variable is thrown
      //  future stack traces of the exception will not see prior calls
      throw ex;  
    }

En général, l'utilisation de `throw ex` n'est pas souhaitable, car les futurs gestionnaires d'exceptions qui inspectent la trace de la pile ne pourront voir que les appels remontant jusqu'à `throw ex`. En omettant la variable `ex` et en utilisant le mot-clé `throw` seul, l'exception d'origine ["bulle-up"][1].

[1] : http://stackoverflow.com/questions/4065893/about-throw-and-exception-bubbling

## Lancer une exception à partir d'une méthode différente tout en préservant ses informations
Parfois, vous souhaiterez intercepter une exception et la lancer à partir d'un thread ou d'une méthode différente tout en préservant la pile d'exceptions d'origine. Cela peut être fait avec `ExceptionDispatchInfo` :

    using System.Runtime.ExceptionServices;

    void Main()
    {
        ExceptionDispatchInfo capturedException = null;
        try
        {
            throw new Exception();
        }
        catch (Exception ex)
        {
            capturedException = ExceptionDispatchInfo.Capture(ex);
        }
        
        Foo(capturedException);
    }
    
    void Foo(ExceptionDispatchInfo exceptionDispatchInfo)
    {
        // Do stuff
    
        if (capturedException != null)
        {
            // Exception stack trace will show it was thrown from Main() and not from Foo()
            exceptionDispatchInfo.Throw();
        }
    }

## Attraper une exception
Le code peut et doit lever des exceptions dans des circonstances exceptionnelles. Voici quelques exemples :

- Tentative de [lire au-delà de la fin d'un flux][1]
- [Ne pas avoir les autorisations nécessaires][2] pour accéder à un fichier
- Tentative d'exécution d'une opération invalide, telle que [diviser par zéro][3]
- [Un délai d'expiration se produit] [4] lors du téléchargement d'un fichier depuis Internet

L'appelant peut gérer ces exceptions en les "interceptant" et ne doit le faire que lorsque :

- Il peut effectivement résoudre la circonstance exceptionnelle ou se redresser convenablement, ou ;
- Il peut fournir un contexte supplémentaire à l'exception qui serait utile si l'exception doit être relancée (les exceptions relancées sont interceptées par les gestionnaires d'exceptions plus haut dans la pile des appels)

Il convient de noter que choisir *not* pour intercepter une exception est parfaitement valide si l'intention est qu'elle soit gérée à un niveau supérieur.

La capture d'une exception est effectuée en enveloppant le code potentiellement générant dans un bloc `try { ... }` comme suit, et en capturant les exceptions qu'il est capable de gérer dans un bloc `catch (ExceptionType) { ... }` :

    Console.Write("Please enter a filename: ");
    string filename = Console.ReadLine();

    Stream fileStream;

    try
    {
        fileStream = File.Open(filename);
    }
    catch (FileNotFoundException)
    {
        Console.WriteLine("File '{0}' could not be found.", filename);
    }


[1] : https://msdn.microsoft.com/en-us/library/system.io.endofstreamexception(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.unauthorizedaccessexception(v=vs.110).aspx
[3] : https://msdn.microsoft.com/en-us/library/system.dividebyzeroexception(v=vs.110).aspx
[4] : https://msdn.microsoft.com/en-us/library/system.net.webexception.aspx

