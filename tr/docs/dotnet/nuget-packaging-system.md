---
title: "NuGet paketleme sistemi"
slug: "nuget-paketleme-sistemi"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

[NuGet.org](https://www.nuget.org/):

> NuGet, .NET dahil olmak üzere Microsoft geliştirme platformu için paket yöneticisidir. NuGet istemci araçları, paket üretme ve tüketme yeteneği sağlar. NuGet Galerisi, tüm paket yazarları ve tüketiciler tarafından kullanılan merkezi paket deposudur.

Örneklerdeki resimler [NuGet.org](https://www.nuget.org/)'nin izniyle.

## Çözümdeki bir projeden paketi kaldırma
    PM> Uninstall-Package -ProjectName MyProjectB EntityFramework

## Bir paketin belirli bir sürümünü yükleme
    PM> Install-Package EntityFramework -Version 6.1.2  


## NuGet Paket Yöneticisini Yükleme
Projelerinizin paketlerini yönetebilmek için NuGet Paket Yöneticisine ihtiyacınız var. Bu, resmi belgelerde açıklanan bir Visual Studio Uzantısıdır: [NuGet İstemcisini Yükleme ve Güncelleme](https://docs.nuget.org/consume/installing-nuget).

Visual Studio 2012'den itibaren, NuGet her sürümde bulunur ve şuradan kullanılabilir: Araçlar -> NuGet Paket Yöneticisi -> Paket Yöneticisi Konsolu.

Bunu Visual Studio'nun Araçlar menüsünden Uzantılar ve Güncellemeler'e tıklayarak yaparsınız:

[![buraya resim açıklamasını girin][1]][1]

[1]: http://i.stack.imgur.com/zTzgp.png

Bu, hem GUI'yi yükler:

* Bir projede veya Referanslar klasöründe "NuGet Paketlerini Yönet..." seçeneğine tıklayarak erişilebilir

Ve Paket Yönetici Konsolu:

* Araçlar -> NuGet Paket Yöneticisi -> Paket Yöneticisi Konsolu.

## Paket kaynak beslemesi ekleme (MyGet, Klondike, vb)
    nuget sources add -name feedname -source http://sourcefeedurl

## Kullanıcı Arayüzü aracılığıyla Paketleri Yönetme
Bir projeye (veya Referanslar klasörüne) sağ tıkladığınızda, "NuGet Paketlerini Yönet..." seçeneğine tıklayabilirsiniz. Bu, [Paket Yöneticisi İletişim Kutusunu](https://docs.nuget.org/consume/package-manager-dialog) gösterir.

[![buraya resim açıklamasını girin][1]][1]

[1]: http://i.stack.imgur.com/Fi0Uq.png

## Konsol üzerinden Paketleri Yönetme
Konsolu IDE'nizde göstermek için Araçlar -> NuGet Paket Yöneticisi -> Paket Yöneticisi Konsolu menülerine tıklayın. [Resmi belgeler burada](https://docs.nuget.org/consume/package-manager-console-powershell-reference).

Burada, diğerlerinin yanı sıra, girilen paketi şu anda seçili olan "Varsayılan proje"ye yükleyen "install-package" komutlarını verebilirsiniz:

    Install-Package Elmah

"Varsayılan proje" açılır menüsünde seçilen projeyi geçersiz kılarak paketin kurulacağı projeyi de sağlayabilirsiniz:

    Install-Package Elmah -ProjectName MyFirstWebsite

## Paket güncelleme
Bir paketi güncellemek için aşağıdaki komutu kullanın:

    PM> Update-Package EntityFramework
EntityFramework, güncellenecek paketin adıdır. Güncellemenin tüm projeler için çalışacağını ve bu nedenle yalnızca "Varsayılan proje"ye yüklenecek olan `Install-Package EntityFramework`ten farklı olduğunu unutmayın.

Ayrıca tek bir projeyi açıkça belirtebilirsiniz:

    PM> Update-Package EntityFramework -ProjectName MyFirstWebsite



## Bir paketi kaldırma
    PM> Uninstall-Package EntityFramework  

## paketin belirli bir sürümünü kaldır
    
    PM> uninstall-Package EntityFramework -Version 6.1.2

## UI kullanarak farklı (yerel) Nuget paket kaynakları kullanma
Paketlerin farklı ekipler arasında dağıtılması için şirketin kendi nuget sunucusunu kurması yaygındır.

1. Çözüm Gezgini'ne gidin ve <kbd>Sağ Fare</kbd> düğmesini tıklayın, ardından `Çözüm için NuGet Paketlerini Yönet'i seçin.

[![buraya resim açıklamasını girin][1]][1]

2. Açılan pencerede `Ayarlar`a tıklayın

[![buraya resim açıklamasını girin][2]][2]

3. Sağ üst köşedeki `+` üzerine tıklayın, ardından yerel nuget sunucunuza işaret eden ad ve url ekleyin.

[![buraya resim açıklamasını girin][3]][3]


[1]: http://i.stack.imgur.com/PhB3d.png
[2]: http://i.stack.imgur.com/8vKM6.png
[3]: http://i.stack.imgur.com/h85QG.png

