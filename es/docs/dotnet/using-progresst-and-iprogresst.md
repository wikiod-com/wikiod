---
title: "Uso de Progress<T> e IProgress<T>"
slug: "uso-de-progresst-e-iprogresst"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Informes de progreso simples
`IProgress<T>` se puede usar para informar el progreso de algún procedimiento a otro procedimiento. Este ejemplo muestra cómo puede crear un método básico que informe su progreso.

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

Producción:

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

Tenga en cuenta que cuando ejecuta este código, es posible que vea que los números salen fuera de orden. Esto se debe a que el método `IProgress<T>.Report()` se ejecuta de forma asincrónica y, por lo tanto, no es adecuado para situaciones en las que el progreso debe informarse en orden.

## Usando IProgress<T>
Es importante tener en cuenta que la clase `System.Progress<T>` no tiene disponible el método `Report()`. Este método se implementó explícitamente desde la interfaz `IProgress<T>` y, por lo tanto, se debe llamar en un `Progress<T>` cuando se convierte en un `IProgress<T>`.

    var p1 = new Progress<int>();
    p1.Report(1); //compiler error, Progress does not contain method 'Report'

    IProgress<int> p2 = new Progress<int>();
    p2.Report(2); //works
    
    var p3 = new Progress<int>();
    ((IProgress<int>)p3).Report(3); //works

