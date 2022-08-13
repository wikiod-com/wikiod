---
title: "Extensões reativas (Rx)"
slug: "extensoes-reativas-rx"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Observando o evento TextChanged em um TextBox
Um observável é criado a partir do evento TextChanged do TextBox. Além disso, qualquer entrada só é selecionada se for diferente da última entrada e se não houver entrada em 0,5 segundos.
A saída neste exemplo é enviada para o console.

    Observable
         .FromEventPattern(textBoxInput, "TextChanged")
         .Select(s => ((TextBox) s.Sender).Text)
         .Throttle(TimeSpan.FromSeconds(0.5))
         .DistinctUntilChanged()
         .Subscribe(text => Console.WriteLine(text));

## Transmissão de dados do banco de dados com o Observable
Suponha que tenha um método retornando `IEnumerable<T>`, por exemplo.

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

Cria um Observable e inicia um método de forma assíncrona. O `SelectMany` nivela a coleção e a assinatura é disparada a cada 200 elementos através do `Buffer`.

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

