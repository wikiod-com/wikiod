---
title: "Extensiones Reactivas (Rx)"
slug: "extensiones-reactivas-rx"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Observando el evento TextChanged en un cuadro de texto
Se crea un observable a partir del evento TextChanged del TextBox. Además, cualquier entrada solo se selecciona si es diferente de la última entrada y si no hubo ninguna entrada en 0,5 segundos.
La salida en este ejemplo se envía a la consola.

    Observable
         .FromEventPattern(textBoxInput, "TextChanged")
         .Select(s => ((TextBox) s.Sender).Text)
         .Throttle(TimeSpan.FromSeconds(0.5))
         .DistinctUntilChanged()
         .Subscribe(text => Console.WriteLine(text));

## Transmisión de datos desde la base de datos con Observable
Suponga que tiene un método que devuelve `IEnumerable<T>`, p.e.

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

Crea un Observable e inicia un método de forma asíncrona. `SelectMany` aplana la colección y la suscripción se dispara cada 200 elementos a través de `Buffer`.

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

