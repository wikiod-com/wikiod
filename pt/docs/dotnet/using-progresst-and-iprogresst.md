---
title: "Usando Progress<T> e IProgress<T>"
slug: "usando-progresst-e-iprogresst"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Relatório de progresso simples
`IProgress<T>` pode ser usado para relatar o progresso de algum procedimento para outro procedimento. Este exemplo mostra como você pode criar um método básico que relata seu progresso.

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

Resultado:

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

Observe que, quando esse código é executado, você pode ver os números serem emitidos fora de ordem. Isso ocorre porque o método `IProgress<T>.Report()` é executado de forma assíncrona e, portanto, não é adequado para situações em que o progresso deve ser relatado em ordem.

## Usando IProgress<T>
É importante notar que a classe `System.Progress<T>` não tem o método `Report()` disponível nela. Este método foi implementado explicitamente a partir da interface `IProgress<T>` e, portanto, deve ser chamado em um `Progress<T>` quando é convertido em um `IProgress<T>`.

    var p1 = new Progress<int>();
    p1.Report(1); //compiler error, Progress does not contain method 'Report'

    IProgress<int> p2 = new Progress<int>();
    p2.Report(2); //works
    
    var p3 = new Progress<int>();
    ((IProgress<int>)p3).Report(3); //works

