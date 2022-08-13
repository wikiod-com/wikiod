---
title: "Utilisation de Progress<T> et IProgress<T>"
slug: "utilisation-de-progresst-et-iprogresst"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Rapports de progression simples
`IProgress<T>` peut être utilisé pour signaler la progression d'une procédure à une autre procédure. Cet exemple montre comment vous pouvez créer une méthode de base qui signale sa progression.

    void Main()
    {
        IProgress<int> p = new Progress<int>(progress =>
        {
            Console.WriteLine("Running Step: {0}", progress);
        });
        LongJob(p);
    }
    
    public void LongJob(IProgress<int> progress)
    {
        var max = 10;
        for (int i = 0; i < max; i++)
        {
            progress.Report(i);
        }
    }

Production:

    Running Step: 0
    Running Step: 3
    Running Step: 4
    Running Step: 5
    Running Step: 6
    Running Step: 7
    Running Step: 8
    Running Step: 9
    Running Step: 2
    Running Step: 1

Notez que lorsque vous exécutez ce code, vous pouvez voir des nombres sortir dans le désordre. En effet, la méthode `IProgress<T>.Report()` est exécutée de manière asynchrone et n'est donc pas aussi adaptée aux situations où la progression doit être signalée dans l'ordre.

## Utilisation de IProgress<T>
Il est important de noter que la classe `System.Progress<T>` n'a pas la méthode `Report()` disponible dessus. Cette méthode a été implémentée explicitement à partir de l'interface `IProgress<T>` et doit donc être appelée sur un `Progress<T>` lorsqu'elle est convertie en `IProgress<T>`.

    var p1 = new Progress<int>();
    p1.Report(1); //compiler error, Progress does not contain method 'Report'

    IProgress<int> p2 = new Progress<int>();
    p2.Report(2); //works
    
    var p3 = new Progress<int>();
    ((IProgress<int>)p3).Report(3); //works

