---
title: "Kronometreler"
slug: "kronometreler"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Sözdizimi
- stopWatch.Start() - Kronometreyi başlatır.
- stopWatch.Stop() - Kronometreyi durdurur.
- stopWatch.Elapsed - Geçerli aralık tarafından ölçülen toplam geçen süreyi alır.

Kronometreler genellikle kıyaslama programlarında kodu zamanlamak ve farklı kod bölümlerinin nasıl en uygun şekilde çalıştığını görmek için kullanılır.

## Yüksek Çözünürlük
    

- IsHighResolution özelliği, zamanlayıcının yüksek çözünürlüklü bir performans sayacını mı yoksa DateTime sınıfını mı temel aldığını gösterir.
- Bu alan salt okunurdur.


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

Kronometre sınıfı tarafından kullanılan zamanlayıcı, sistem donanımına ve işletim sistemine bağlıdır. Kronometre zamanlayıcısı yüksek çözünürlüklü bir performans sayacını temel alıyorsa IsHighResolution doğrudur. Aksi takdirde, IsHighResolution false olur ve bu, Kronometre zamanlayıcısının sistem zamanlayıcısını temel aldığını gösterir.

Kronometredeki işaretler makineye/işletim sistemine bağlıdır, bu nedenle iki sistem arasında ve hatta yeniden başlatmanın ardından muhtemelen aynı sistemde Kronometre tiklerinin saniyelere oranının aynı olacağına asla güvenmemelisiniz. Bu nedenle, Kronometre kenelerinin DateTime/TimeSpan keneleriyle aynı aralıkta olacağına asla güvenemezsiniz.

Sistemden bağımsız süre elde etmek için, Kronometre'nin zaten Kronometreyi hesaba katan Elapsed veya ElapsedMilisaniye özelliklerini kullandığınızdan emin olun.

Kronometre, daha hafif olduğundan ve yüksek çözünürlüklü bir performans sayacı kullanamıyorsa Datetime kullandığından, zamanlama işlemleri için her zaman Datetime üzerinden kullanılmalıdır.

[Kaynak](http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx)

## Bir Kronometre Örneği Oluşturma
Bir Kronometre örneği, geçen süreyi, toplam geçen sürenin tümü tek tek aralıklarla eklendiğinden, birkaç aralıkta ölçebilir. Bu, iki veya daha fazla olay arasında geçen süreyi ölçmek için güvenilir bir yöntem sağlar.


    Stopwatch stopWatch = new Stopwatch();
    stopWatch.Start();

    double d = 0;
    for (int i = 0; i < 1000 * 1000 * 1000; i++)
    {
        d += 1;
    }

    stopWatch.Stop();
    Console.WriteLine("Time elapsed: {0:hh\\:mm\\:ss\\.fffffff}", stopWatch.Elapsed);


'Stopwach', 'System.Diagnostics' içindedir, bu nedenle dosyanıza 'using System.Diagnostics;' eklemeniz gerekir.

