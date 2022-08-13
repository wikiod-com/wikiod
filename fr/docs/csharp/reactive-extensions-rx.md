---
title: "Extensions réactives (Rx)"
slug: "extensions-reactives-rx"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Observant l'événement TextChanged sur un TextBox
Un observable est créé à partir de l'événement TextChanged du TextBox. De plus, toute entrée n'est sélectionnée que si elle est différente de la dernière entrée et s'il n'y a pas eu d'entrée dans les 0,5 secondes.
La sortie de cet exemple est envoyée à la console.

    Observable
         .FromEventPattern(textBoxInput, "TextChanged")
         .Select(s => ((TextBox) s.Sender).Text)
         .Throttle(TimeSpan.FromSeconds(0.5))
         .DistinctUntilChanged()
         .Subscribe(text => Console.WriteLine(text));

## Streaming de données depuis la base de données avec Observable
Supposons qu'une méthode renvoie `IEnumerable<T>`, par ex.

    private IEnumerable<T> GetData()
    {
        try 
        {
            // return results from database 
        }
        catch(Exception exception)
        {
            throw;
        }
    }  

Crée un Observable et démarre une méthode de manière asynchrone. `SelectMany` aplatit la collection et l'abonnement est déclenché tous les 200 éléments via `Buffer`.

    int bufferSize = 200;

    Observable
        .Start(() => GetData())
        .SelectMany(s => s)
        .Buffer(bufferSize)
        .ObserveOn(SynchronizationContext.Current)
        .Subscribe(items => 
        {
            Console.WriteLine("Loaded {0} elements", items.Count);
            
            // do something on the UI like incrementing a ProgressBar
        },
        () => Console.WriteLine("Completed loading"));

