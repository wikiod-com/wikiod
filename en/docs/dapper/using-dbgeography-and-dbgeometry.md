---
title: "Using DbGeography and DbGeometry"
slug: "using-dbgeography-and-dbgeometry"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## Configuration required
1. install the required `Microsoft.SqlServer.Types` assembly; they are not installed by default, and are [available from Microsoft here](https://www.microsoft.com/en-gb/download/details.aspx?id=29065) as "Microsoft® System CLR Types for Microsoft® SQL Server® 2012" - note that there are separate installers for x86 and x64.
2. install [`Dapper.EntityFramework`](https://www.nuget.org/packages/dapper.entityframework) (or the strong-named equivalent); this could be done via the IDE's "Manage NuGet Packages..." UI, or (at the Package Manager Console):

       install-package Dapper.EntityFramework
3. add the required assembly binding redirects; this is because Microsoft ships v11 of the assemblies, but Entity Framework asks for v10; you can add the following to `app.config` or `web.config` under the `<configuration>` element:

       <runtime>
         <assemblyBinding xmlns="urn:schemas-microsoft-com:asm.v1">
           <dependentAssembly>
             <assemblyIdentity name="Microsoft.SqlServer.Types" 
                 publicKeyToken="89845dcd8080cc91" />
             <bindingRedirect oldVersion="10.0.0.0" newVersion="11.0.0.0" />
           </dependentAssembly>
         </assemblyBinding>
       </runtime>
4. tell "dapper" about the new type handlers available, by adding (somewhere in your startup, before it tries using the database):

       Dapper.EntityFramework.Handlers.Register();

## Using geometry and geography
Once the type handlers are registered, everything should work automatically, and you should be able to use these types as either parameters or return values:

    string redmond = "POINT (122.1215 47.6740)";
    DbGeography point = DbGeography.PointFromText(redmond,
        DbGeography.DefaultCoordinateSystemId);
    DbGeography orig = point.Buffer(20); // create a circle around a point


    var fromDb = connection.QuerySingle<DbGeography>(
      "declare @geos table(geo geography); insert @geos(geo) values(@val); select * from @geos",
      new { val = orig });

    Console.WriteLine($"Original area: {orig.Area}");
    Console.WriteLine($"From DB area: {fromDb.Area}");


