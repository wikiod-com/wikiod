---
title: "DbGeography ve DbGeometry'yi Kullanma"
slug: "dbgeography-ve-dbgeometryyi-kullanma"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Yapılandırma gerekli
1. gerekli `Microsoft.SqlServer.Types` derlemesini kurun; varsayılan olarak yüklenmezler ve "Microsoft® için Microsoft® Sistem CLR Türleri" olarak [buradan Microsoft'tan edinilebilir](https://www.microsoft.com/en-gb/download/details.aspx?id=29065) SQL Server® 2012" - x86 ve x64 için ayrı yükleyiciler olduğunu unutmayın.
2. [`Dapper.EntityFramework`](https://www.nuget.org/packages/dapper.entityframework) (veya güçlü adlandırılmış eşdeğerini) yükleyin; bu, IDE'nin "NuGet Paketlerini Yönet..." Kullanıcı Arayüzü aracılığıyla veya (Paket Yönetici Konsolunda):

       install-package Dapper.EntityFramework
3. gerekli derleme bağlama yönlendirmelerini ekleyin; bunun nedeni, Microsoft'un derlemelerin v11'ini göndermesi, ancak Entity Framework'ün v10 istemesidir; aşağıdakileri "<configuration>" öğesinin altındaki "app.config" veya "web.config"e ekleyebilirsiniz:

       <runtime>
         <assemblyBinding xmlns="urn:schemas-microsoft-com:asm.v1">
           <dependentAssembly>
             <assemblyIdentity name="Microsoft.SqlServer.Types" 
                 publicKeyToken="89845dcd8080cc91" />
             <bindingRedirect oldVersion="10.0.0.0" newVersion="11.0.0.0" />
           </dependentAssembly>
         </assemblyBinding>
       </runtime>
4. Şunu ekleyerek (veritabanını kullanmayı denemeden önce, başlangıçtaki bir yerde):

       Dapper.EntityFramework.Handlers.Register();

## Geometri ve coğrafyayı kullanma
Tür işleyicileri kaydedildikten sonra, her şey otomatik olarak çalışmalı ve bu türleri parametre veya dönüş değerleri olarak kullanabilmelisiniz:

    string redmond = "POINT (122.1215 47.6740)";
    DbGeography point = DbGeography.PointFromText(redmond,
        DbGeography.DefaultCoordinateSystemId);
    DbGeography orig = point.Buffer(20); // create a circle around a point


    var fromDb = connection.QuerySingle<DbGeography>(
      "declare @geos table(geo geography); insert @geos(geo) values(@val); select * from @geos",
      new { val = orig });

    Console.WriteLine($"Original area: {orig.Area}");
    Console.WriteLine($"From DB area: {fromDb.Area}");


