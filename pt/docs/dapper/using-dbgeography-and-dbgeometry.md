---
title: "Usando DbGeography e DbGeometry"
slug: "usando-dbgeography-e-dbgeometry"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Configuração necessária
1. instale o assembly `Microsoft.SqlServer.Types` necessário; eles não são instalados por padrão e estão [disponíveis na Microsoft aqui](https://www.microsoft.com/en-gb/download/details.aspx?id=29065) como "Microsoft® System CLR Types for Microsoft® SQL Server® 2012" - observe que existem instaladores separados para x86 e x64.
2. instale o [`Dapper.EntityFramework`](https://www.nuget.org/packages/dapper.entityframework) (ou o equivalente de nome forte); isso pode ser feito por meio da interface do usuário "Gerenciar pacotes NuGet..." do IDE ou (no console do gerenciador de pacotes):

       install-package Dapper.EntityFramework
3. adicione os redirecionamentos de associação de montagem necessários; isso ocorre porque a Microsoft envia a v11 dos assemblies, mas o Entity Framework solicita a v10; você pode adicionar o seguinte ao `app.config` ou `web.config` no elemento `<configuration>`:

       <runtime>
         <assemblyBinding xmlns="urn:schemas-microsoft-com:asm.v1">
           <dependentAssembly>
             <assemblyIdentity name="Microsoft.SqlServer.Types" 
                 publicKeyToken="89845dcd8080cc91" />
             <bindingRedirect oldVersion="10.0.0.0" newVersion="11.0.0.0" />
           </dependentAssembly>
         </assemblyBinding>
       </runtime>
4. informe "dapper" sobre os novos manipuladores de tipos disponíveis, adicionando (em algum lugar em sua inicialização, antes de tentar usar o banco de dados):

       Dapper.EntityFramework.Handlers.Register();

## Usando geometria e geografia
Depois que os manipuladores de tipos forem registrados, tudo deverá funcionar automaticamente e você poderá usar esses tipos como parâmetros ou valores de retorno:

    string redmond = "POINT (122.1215 47.6740)";
    DbGeography point = DbGeography.PointFromText(redmond,
        DbGeography.DefaultCoordinateSystemId);
    DbGeography orig = point.Buffer(20); // create a circle around a point


    var fromDb = connection.QuerySingle<DbGeography>(
      "declare @geos table(geo geography); insert @geos(geo) values(@val); select * from @geos",
      new { val = orig });

    Console.WriteLine($"Original area: {orig.Area}");
    Console.WriteLine($"From DB area: {fromDb.Area}");


