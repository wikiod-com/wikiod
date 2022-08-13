---
title: "Uso de DbGeography y DbGeometry"
slug: "uso-de-dbgeography-y-dbgeometry"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Configuración requerida
1. instale el ensamblado `Microsoft.SqlServer.Types` requerido; no se instalan de forma predeterminada y están [disponibles en Microsoft aquí](https://www.microsoft.com/en-gb/download/details.aspx?id=29065) como "Microsoft® System CLR Types for Microsoft® SQL Server® 2012": tenga en cuenta que hay instaladores independientes para x86 y x64.
2. instale [`Dapper.EntityFramework`](https://www.nuget.org/packages/dapper.entityframework) (o el equivalente con nombre seguro); esto se puede hacer a través de la interfaz de usuario "Administrar paquetes NuGet..." del IDE, o (en la Consola del administrador de paquetes):

       install-package Dapper.EntityFramework
3. agregue las redirecciones de enlace de ensamblaje requeridas; esto se debe a que Microsoft envía la versión 11 de los ensamblajes, pero Entity Framework solicita la versión 10; puede agregar lo siguiente a `app.config` o `web.config` bajo el elemento `<configuration>`:

       <runtime>
         <assemblyBinding xmlns="urn:schemas-microsoft-com:asm.v1">
           <dependentAssembly>
             <assemblyIdentity name="Microsoft.SqlServer.Types" 
                 publicKeyToken="89845dcd8080cc91" />
             <bindingRedirect oldVersion="10.0.0.0" newVersion="11.0.0.0" />
           </dependentAssembly>
         </assemblyBinding>
       </runtime>
4. dígale a "dapper" sobre los nuevos controladores de tipo disponibles, agregando (en algún lugar de su inicio, antes de que intente usar la base de datos):

       Dapper.EntityFramework.Handlers.Register();

## Usando geometría y geografía
Una vez que se registran los controladores de tipo, todo debería funcionar automáticamente y debería poder usar estos tipos como parámetros o valores de retorno:

    string redmond = "POINT (122.1215 47.6740)";
    DbGeography point = DbGeography.PointFromText(redmond,
        DbGeography.DefaultCoordinateSystemId);
    DbGeography orig = point.Buffer(20); // create a circle around a point


    var fromDb = connection.QuerySingle<DbGeography>(
      "declare @geos table(geo geography); insert @geos(geo) values(@val); select * from @geos",
      new { val = orig });

    Console.WriteLine($"Original area: {orig.Area}");
    Console.WriteLine($"From DB area: {fromDb.Area}");


