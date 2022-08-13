---
title: "Primeros pasos con Dapper.NET"
slug: "primeros-pasos-con-dappernet"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Instalar Dapper desde Nuget
Busque en la GUI de Visual Studio:

Herramientas > Administrador de paquetes NuGet > Administrar paquetes para la solución... (Visual Studio 2015)

[![captura de pantalla de la interfaz del administrador de paquetes de Visual Studio con Dapper seleccionado][1]][1]

O ejecute este comando en una instancia de Nuget Power Shell para instalar la última versión estable

    Install-Package Dapper

O para una versión específica

    Install-Package Dapper -Version 1.42.0

[1]: http://i.stack.imgur.com/sWn6V.png

## Usando Dapper en C#
    using System.Data;
    using System.Linq;
    using Dapper;
    
    class Program
    {
        static void Main()
        {
            using (IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true"))
            {
                db.Open();
                var result = db.Query<string>("SELECT 'Hello World'").Single();
                Console.WriteLine(result);
            }
        }
    }

Envolver la conexión en un [`Using` block](https://www.wikiod.com/es/docs/c%23/38/using-statement/157/cleaner-dispose-syntax) cerrará la conexión

## Uso de Dapper en LINQPad
[LINQPad](http://www.linqpad.net/) es ideal para probar consultas de bases de datos e incluye [integración de NuGet](http://www.linqpad.net/Purchase.aspx#NuGet). Para usar Dapper en LINQPad, presione **F4** para abrir Query Properties y luego seleccione **Add NuGet**. Busque **dapper dot net** y seleccione **Agregar a consulta**. También querrá hacer clic en **Agregar espacios de nombres** y resaltar Dapper para incluir los métodos de extensión en su consulta de LINQPad.

Una vez que Dapper está habilitado, puede cambiar el menú desplegable Idioma a **Programa C#**, asignar los resultados de la consulta a las clases C# y utilizar el método .Dump() para inspeccionar los resultados:

vacío principal()
	{
usando (IDbConnection db = new SqlConnection("Server=myServer;Trusted_Connection=true")){
db.Abrir();
var escalar = db.Query<string>("SELECT GETDATE()").SingleOrDefault();
escalar.Dump("Este es un resultado escalar de cadena:");
			
var resultados = db.Query<miobjeto>(@"
SELECCIONAR DE (
VALORES (1,'uno'),
(2,'dos'),
(3 tres')
) COMO mitabla(id,nombre)");
resultados.Dump("Esta es una tabla asignada a una clase:");
		}
	}
	
// Definir otros métodos y clases aquí
clase miobjeto {
identificación int pública { obtener; establecer; }
nombre de cadena pública { obtener; establecer; }
	}

Los resultados al ejecutar el programa se verían así:

[![Captura de pantalla de LINQPad][1]][1]


[1]: http://i.stack.imgur.com/swXB1.png

