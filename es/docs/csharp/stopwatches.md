---
title: "Cronómetros"
slug: "cronometros"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Sintaxis
- stopWatch.Start() - Inicia el cronómetro.
- stopWatch.Stop() - Detiene el cronómetro.
- stopWatch.Elapsed: obtiene el tiempo total transcurrido medido por el intervalo actual.

Los cronómetros se utilizan a menudo en programas de evaluación comparativa para cronometrar el código y ver qué tan óptimos son los diferentes segmentos de código que tardan en ejecutarse.

## EsAltaResolución
    

- La propiedad IsHighResolution indica si el temporizador se basa en un contador de rendimiento de alta resolución o en la clase DateTime.
- Este campo es de solo lectura.


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

El temporizador utilizado por la clase Cronómetro depende del hardware del sistema y del sistema operativo. IsHighResolution es verdadero si el cronómetro se basa en un contador de rendimiento de alta resolución. De lo contrario, IsHighResolution es falso, lo que indica que el temporizador del cronómetro se basa en el temporizador del sistema.

Los ticks en Stopwatch dependen de la máquina/SO, por lo que nunca debe contar con que la proporción de ticks de Stopwatch a segundos sea la misma entre dos sistemas, y posiblemente incluso en el mismo sistema después de un reinicio. Por lo tanto, nunca puede contar con que los ticks del cronómetro tengan el mismo intervalo que los ticks de DateTime/TimeSpan.

Para obtener un tiempo independiente del sistema, asegúrese de utilizar las propiedades Elapsed o ElapsedMilliseconds del cronómetro, que ya tienen en cuenta la frecuencia del cronómetro (pasos por segundo).

El cronómetro siempre debe usarse sobre Datetime para los procesos de tiempo, ya que es más liviano y usa Dateime si no puede usar un contador de rendimiento de alta resolución.

[Fuente](http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx)

## Crear una instancia de un cronómetro
Una instancia de cronómetro puede medir el tiempo transcurrido en varios intervalos y el tiempo total transcurrido es la suma de todos los intervalos individuales. Esto brinda un método confiable para medir el tiempo transcurrido entre dos o más eventos.


    Stopwatch stopWatch = new Stopwatch();
    stopWatch.Start();

    double d = 0;
    for (int i = 0; i < 1000 * 1000 * 1000; i++)
    {
        d += 1;
    }

    stopWatch.Stop();
    Console.WriteLine("Time elapsed: {0:hh\\:mm\\:ss\\.fffffff}", stopWatch.Elapsed);


`Stopwach` está en `System.Diagnostics`, por lo que debe agregar `using System.Diagnostics;` a su archivo.

