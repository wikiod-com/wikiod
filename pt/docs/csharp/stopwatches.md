---
title: "Cronômetros"
slug: "cronometros"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Sintaxe
- stopWatch.Start() - Inicia o cronômetro.
- stopWatch.Stop() - Pára o cronômetro.
- stopWatch.Elapsed - Obtém o tempo total decorrido medido pelo intervalo atual.

Os cronômetros são frequentemente usados ​​em programas de benchmarking para programar o tempo e ver como os diferentes segmentos de código são otimizados para serem executados.

## ÉAltaResolução
    

- A propriedade IsHighResolution indica se o cronômetro é baseado em um contador de desempenho de alta resolução ou na classe DateTime.
- Este campo é só de leitura.


    // Display the timer frequency and resolution.
    if (Stopwatch.IsHighResolution)
    {
        Console.WriteLine("Operations timed using the system's high-resolution performance counter.");
    }
    else 
    {
        Console.WriteLine("Operations timed using the DateTime class.");
    }

    long frequency = Stopwatch.Frequency;
    Console.WriteLine("  Timer frequency in ticks per second = {0}",
        frequency);
    long nanosecPerTick = (1000L*1000L*1000L) / frequency;
    Console.WriteLine("  Timer is accurate within {0} nanoseconds", 
        nanosecPerTick);
    }
https://dotnetfiddle.net/ckrWUo

O temporizador usado pela classe Stopwatch depende do hardware do sistema e do sistema operacional. IsHighResolution é true se o cronômetro do cronômetro for baseado em um contador de desempenho de alta resolução. Caso contrário, IsHighResolution é false, o que indica que o cronômetro do cronômetro é baseado no cronômetro do sistema.

Os tiques no cronômetro são dependentes da máquina/SO, portanto, você nunca deve contar com a proporção de tiques do cronômetro em segundos para ser a mesma entre dois sistemas e possivelmente até mesmo no mesmo sistema após uma reinicialização. Assim, você nunca pode contar com os tiques do cronômetro para serem o mesmo intervalo que os tiques de DateTime/TimeSpan.

Para obter o tempo independente do sistema, certifique-se de usar as propriedades Elapsed ou ElapsedMilliseconds do Stopwatch, que já levam em consideração o Stopwatch.Frequency (tiques por segundo).

O cronômetro deve sempre ser usado sobre Datetime para processos de temporização, pois é mais leve e usa Datetime se não puder usar um contador de desempenho de alta resolução.

[Fonte](http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx)

## Criando uma instância de um cronômetro
Uma instância de Stopwatch pode medir o tempo decorrido em vários intervalos com o tempo total decorrido sendo todos os intervalos individuais somados. Isso fornece um método confiável de medir o tempo decorrido entre dois ou mais eventos.


    Stopwatch stopWatch = new Stopwatch();
    stopWatch.Start();

    double d = 0;
    for (int i = 0; i < 1000 * 1000 * 1000; i++)
    {
        d += 1;
    }

    stopWatch.Stop();
    Console.WriteLine("Time elapsed: {0:hh\\:mm\\:ss\\.fffffff}", stopWatch.Elapsed);


`Stopwach` está em `System.Diagnostics` então você precisa adicionar `using System.Diagnostics;` ao seu arquivo.

