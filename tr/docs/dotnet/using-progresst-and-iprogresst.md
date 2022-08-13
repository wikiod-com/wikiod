---
title: "Progress<T> ve IProgress<T> kullanma"
slug: "progresst-ve-iprogresst-kullanma"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Basit İlerleme raporlaması
`IPogress<T>`, bir prosedürün ilerlemesini başka bir prosedüre bildirmek için kullanılabilir. Bu örnek, ilerlemesini bildiren temel bir yöntemi nasıl oluşturabileceğinizi gösterir.

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

Çıktı:

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

Bu kodu çalıştırdığınızda, sayıların düzensiz çıktığını görebilirsiniz. Bunun nedeni, "IProgress<T>.Report()" yönteminin eşzamansız olarak çalıştırılması ve bu nedenle ilerlemenin sırayla rapor edilmesi gereken durumlar için uygun olmamasıdır.

## IProgress<T> Kullanımı
'System.Progress<T>' sınıfında 'Report()' yönteminin bulunmadığına dikkat etmek önemlidir. Bu yöntem, "IProgress<T>" arabiriminden açıkça uygulandı ve bu nedenle, bir "IProgress<T>" öğesine dönüştürüldüğünde bir "Progress<T>" üzerinde çağrılmalıdır.

    var p1 = new Progress<int>();
    p1.Report(1); //compiler error, Progress does not contain method 'Report'

    IProgress<int> p2 = new Progress<int>();
    p2.Report(2); //works
    
    var p3 = new Progress<int>();
    ((IProgress<int>)p3).Report(3); //works

