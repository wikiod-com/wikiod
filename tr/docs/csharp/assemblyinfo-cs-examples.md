---
title: "AssemblyInfo.cs Örnekleri"
slug: "assemblyinfocs-ornekleri"
draft: false
images: []
weight: 9764
type: docs
toc: true
---

"AssemblyInfo.cs" dosya adı, geliştiricilerin oluşturmakta oldukları tüm montajı tanımlayan meta veri özniteliklerini yerleştirdiği kaynak dosya olarak geleneksel olarak kullanılır.

## Küresel ve yerel AssemblyInfo


## [AssemblyVersion]
Bu öznitelik, derlemeye bir sürüm uygular.

    [assembly: AssemblyVersion("1.0.*")]

`*` karakteri, her derleme yaptığınızda sürümün bir bölümünü otomatik olarak artırmak için kullanılır (genellikle "yapı" numarası için kullanılır)

## [AssemblyTitle]
Bu öznitelik, bu özel derlemeye bir ad vermek için kullanılır.

    [assembly: AssemblyTitle("MyProduct")]



## [MontajÜrünü]
Bu öznitelik, bu özel montajın ait olduğu ürünü tanımlamak için kullanılır. Birden çok montaj aynı ürünün bileşenleri olabilir; bu durumda hepsi bu özellik için aynı değeri paylaşabilir.

    [assembly: AssemblyProduct("MyProduct")]


## Otomatik sürüm oluşturma


## Ortak alanlar


## [InternalsVisibleTo]
Bir derlemenin "iç" sınıflarını veya işlevlerini başka bir derlemeden erişilebilir kılmak istiyorsanız, bunu "InternalsVisibleTo" ve erişime izin verilen derleme adıyla bildirirsiniz.


Bu örnekte, "MyAssembly.UnitTests" derlemesindeki kodun "MyAssembly"den "dahili" öğeleri çağırmasına izin verilir.

    [assembly: InternalsVisibleTo("MyAssembly.UnitTests")]

Bu, özellikle gereksiz "genel" bildirimleri önlemek için birim testi için kullanışlıdır.

## Montaj Niteliklerini Okuma
.NET'in zengin yansıma API'lerini kullanarak bir derlemenin meta verilerine erişebilirsiniz. Örneğin, aşağıdaki kodla ``bu`` derlemenin başlık niteliğini alabilirsiniz.

    using System.Linq;
    using System.Reflection;
    
    ...
    
    Assembly assembly = typeof(this).Assembly;
    var titleAttribute = assembly.GetCustomAttributes<AssemblyTitleAttribute>().FirstOrDefault();
    
    Console.WriteLine($"This assembly title is {titleAttribute?.Title}");


## [MontajYapılandırması]
AssemblyConfiguration: AssemblyConfiguration özniteliği, montajı oluşturmak için kullanılan konfigürasyona sahip olmalıdır.
Farklı derleme konfigürasyonlarını düzgün bir şekilde dahil etmek için koşullu derlemeyi kullanın.
Aşağıdaki örneğe benzer bloğu kullanın. Yaygın olarak kullandığınız kadar çok farklı konfigürasyon ekleyin.


    #if (DEBUG)
    
    [assembly: AssemblyConfiguration("Debug")]

    #else

    [assembly: AssemblyConfiguration("Release")]
    
    #endif


## [AssemblyKeyFile]
Derlememizin ne zaman GAC'ye kurulmasını istersek, güçlü bir isme sahip olması gerekir. Güçlü adlandırma derlemesi için bir ortak anahtar oluşturmalıyız.
`.snk` dosyasını oluşturmak için.

Kesin ad anahtar dosyası oluşturmak için

> 1. VS2015 için geliştiriciler komut istemi (yönetici Erişimi ile)
> 2. Komut istemine cd C:\Directory_Name yazın ve ENTER'a basın.
> 3. Komut istemine sn -k AnahtarDosyaAdı.snk yazın ve ENTER tuşuna basın.

keyFileName.snk belirtilen dizinde oluşturulduktan sonra projenize referans verin. Sınıf kitaplığımızı oluşturduğumuzda anahtarı oluşturmak için `AssemblyKeyFileAttribute` yolunu `snk` dosyasına öznitelik verin.
    
> Özellikler -> AssemblyInfo.cs
    
    [assembly: AssemblyKeyFile(@"c:\Directory_Name\KeyFileName.snk")]

Thi, derlemeden sonra güçlü bir ad derlemesi oluşturacaktır. Güçlü ad derlemenizi oluşturduktan sonra, onu GAC'ye yükleyebilirsiniz.

Mutlu Kodlama :)

