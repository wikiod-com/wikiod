---
title: "Utiliser DbGeography et DbGeometry"
slug: "utiliser-dbgeography-et-dbgeometry"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

##Configuration requise
1. installez l'assembly `Microsoft.SqlServer.Types` requis ; ils ne sont pas installés par défaut et sont [disponibles auprès de Microsoft ici](https://www.microsoft.com/en-gb/download/details.aspx?id=29065) en tant que "Microsoft® System CLR Types for Microsoft® SQL Server® 2012" - notez qu'il existe des programmes d'installation distincts pour x86 et x64.
2. installez [`Dapper.EntityFramework`](https://www.nuget.org/packages/dapper.entityframework) (ou l'équivalent avec un nom fort) ; cela peut être fait via l'interface utilisateur "Gérer les packages NuGet ..." de l'IDE, ou (sur la console du gestionnaire de packages):

       install-package Dapper.EntityFramework
3. ajoutez les redirections de liaison d'assembly requises ; cela est dû au fait que Microsoft fournit la v11 des assemblys, mais Entity Framework demande la v10 ; vous pouvez ajouter ce qui suit à `app.config` ou `web.config` sous l'élément `<configuration>` :

       <runtime>
         <assemblyBinding xmlns="urn:schemas-microsoft-com:asm.v1">
           <dependentAssembly>
             <assemblyIdentity name="Microsoft.SqlServer.Types" 
                 publicKeyToken="89845dcd8080cc91" />
             <bindingRedirect oldVersion="10.0.0.0" newVersion="11.0.0.0" />
           </dependentAssembly>
         </assemblyBinding>
       </runtime>
4. informez "dapper" des nouveaux gestionnaires de types disponibles, en ajoutant (quelque part dans votre démarrage, avant qu'il n'essaie d'utiliser la base de données):

       Dapper.EntityFramework.Handlers.Register();

## Utilisation de la géométrie et de la géographie
Une fois les gestionnaires de types enregistrés, tout devrait fonctionner automatiquement et vous devriez pouvoir utiliser ces types comme paramètres ou valeurs de retour :

    string redmond = "POINT (122.1215 47.6740)";
    DbGeography point = DbGeography.PointFromText(redmond,
        DbGeography.DefaultCoordinateSystemId);
    DbGeography orig = point.Buffer(20); // create a circle around a point


    var fromDb = connection.QuerySingle<DbGeography>(
      "declare @geos table(geo geography); insert @geos(geo) values(@val); select * from @geos",
      new { val = orig });

    Console.WriteLine($"Original area: {orig.Area}");
    Console.WriteLine($"From DB area: {fromDb.Area}");


