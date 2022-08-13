---
title: "Parámetros dinámicos"
slug: "parametros-dinamicos"
draft: false
images: []
weight: 9497
type: docs
toc: true
---

## Uso básico
No siempre es posible empaquetar ordenadamente todos los parámetros en un solo objeto/llamada. Para ayudar con escenarios más complicados, dapper permite que el parámetro `param` sea una instancia `IDynamicParameters`. Si hace esto, su método `AddParameters` personalizado se llama en el momento apropiado y se le entrega el comando para agregarlo. En la mayoría de los casos, sin embargo, es suficiente usar el tipo `DynamicParameters` preexistente:

    var p = new DynamicParameters(new { a = 1, b = 2 });
    p.Add("c", dbType: DbType.Int32, direction: ParameterDirection.Output);
    connection.Execute(@"set @c = @a + @b", p);
    int updatedValue = p.Get<int>("@c");

Esta espectáculos:

- (opcional) población de un objeto existente
- (opcional) agregar parámetros adicionales sobre la marcha
- pasando los parámetros al comando
- recuperar cualquier valor actualizado después de que el comando haya terminado

Tenga en cuenta que debido a cómo funcionan los protocolos RDBMS, generalmente solo es confiable obtener valores de parámetros actualizados **después** de que cualquier dato (de una operación `Query` o QueryMultiple`) se haya **consumido por completo** (por ejemplo, en SQL Server, los valores de los parámetros actualizados se encuentran al *final* del flujo TDS).

## Parámetros dinámicos en Dapper
    
    connection.Execute(@"some Query with @a,@b,@c", new {a=somevalueOfa,b=somevalueOfb,c=somevalueOfc});

## Usar un objeto de plantilla
Puede usar una instancia de un objeto para formar sus parámetros

    public class SearchParameters {
      public string SearchString { get; set; }
      public int Page { get; set; }
    }

    var template= new SearchParameters {
      SearchString = "Dapper",
      Page = 1
    };

    var p = new DynamicParameters(template);

También puede usar un objeto anónimo o un `Diccionario`

