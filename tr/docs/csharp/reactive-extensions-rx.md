---
title: "Reaktif Uzantılar (Rx)"
slug: "reaktif-uzantlar-rx"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## TextBox'ta TextChanged olayını gözlemleme
TextBox'ın TextChanged olayından bir gözlemlenebilir oluşturulur. Ayrıca herhangi bir giriş, yalnızca son girişten farklıysa ve 0,5 saniye içinde giriş yapılmadıysa seçilir.
Bu örnekteki çıktı konsola gönderilir.

    Observable
         .FromEventPattern(textBoxInput, "TextChanged")
         .Select(s => ((TextBox) s.Sender).Text)
         .Throttle(TimeSpan.FromSeconds(0.5))
         .DistinctUntilChanged()
         .Subscribe(text => Console.WriteLine(text));

## Gözlemlenebilir ile Veritabanından Veri Akışı
'IEnumerable<T>' döndüren bir metoda sahip olduğunuzu varsayalım, f.e.

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

Bir Gözlenebilir oluşturur ve bir yöntemi eşzamansız olarak başlatır. "SelectMany", koleksiyonu düzleştirir ve abonelik, "Tampon" aracılığıyla her 200 öğede bir tetiklenir.

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

