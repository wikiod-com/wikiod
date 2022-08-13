---
title: ".Net çerçevesini kullanarak paralel işleme"
slug: "net-cercevesini-kullanarak-paralel-isleme"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Bu Konu, .NET çerçevesi ile Task Parallel Library kullanarak çok çekirdekli programlama hakkındadır. Görev paralel kitaplığı, insan tarafından okunabilen ve mevcut Çekirdek sayısı ile kendini ayarlayan kod yazmanıza olanak tanır. Böylece yazılımınızın yükseltme ortamıyla kendini otomatik olarak yükselteceğinden emin olabilirsiniz.

## Paralel Uzantılar
Veri Paralelliğini elde etmek için Paralel Uzantılar, Görev Paralel Kitaplığı ile birlikte tanıtıldı. Veri paralelliği, aynı işlemin bir kaynak koleksiyondaki veya dizideki öğeler üzerinde eşzamanlı olarak (yani paralel olarak) gerçekleştirildiği senaryoları ifade eder. .NET, Parallel.For ve Parallel.Foreach yapılarını kullanarak veri paralelliğini elde etmek için yeni yapılar sağlar.

    //Sequential version

    foreach (var item in sourcecollection){

    Process(item);

    }

    // Parallel equivalent

    Parallel.foreach(sourcecollection, item => Process(item));


Yukarıda bahsedilen Parallel.ForEach yapısı birden çok çekirdeği kullanır ve böylece performansı aynı şekilde artırır.

