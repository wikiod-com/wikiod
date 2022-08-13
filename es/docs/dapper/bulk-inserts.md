---
title: "Insertos a granel"
slug: "insertos-a-granel"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

`WriteToServer` y `WriteToServerAsync` tienen sobrecargas que aceptan matrices IDataReader (visto en los ejemplos), DataTable y DataRow (`DataRow[]`) como origen de los datos para la copia masiva.

## Copia masiva asíncrona
Esta muestra usa un método `ToDataReader` descrito aquí [Creación de un lector de datos de lista genérica para SqlBulkCopy] (https://www.csvreader.com/posts/generic_list_datareader.php).

Esto también se puede hacer usando métodos no asíncronos.
    
    public class Widget
    {
        public int WidgetId {get;set;}
        public string Name {get;set;}
        public int Quantity {get;set;}
    }
    
    public async Task AddWidgets(IEnumerable<Widget> widgets)
    {
        using(var conn = new SqlConnection("{connection string}")) {
            await conn.OpenAsync();
    
            using(var bulkCopy = new SqlBulkCopy(conn)) {
                bulkCopy.BulkCopyTimeout = SqlTimeoutSeconds;
                bulkCopy.BatchSize = 500;
                bulkCopy.DestinationTableName = "Widgets";
                bulkCopy.EnableStreaming = true;
    
                using(var dataReader = widgets.ToDataReader())
                {
                    await bulkCopy.WriteToServerAsync(dataReader);
                }
            }
        }
    }
    

## Copia masiva
Esta muestra usa un método `ToDataReader` descrito aquí [Creación de un lector de datos de lista genérica para SqlBulkCopy] (https://www.csvreader.com/posts/generic_list_datareader.php).

Esto también se puede hacer usando métodos asincrónicos.
    
    public class Widget
    {
        public int WidgetId {get;set;}
        public string Name {get;set;}
        public int Quantity {get;set;}
    }
    
    public void AddWidgets(IEnumerable<Widget> widgets)
    {
        using(var conn = new SqlConnection("{connection string}")) {
            conn.Open();
    
            using(var bulkCopy = new SqlBulkCopy(conn)) {
                bulkCopy.BulkCopyTimeout = SqlTimeoutSeconds;
                bulkCopy.BatchSize = 500;
                bulkCopy.DestinationTableName = "Widgets";
                bulkCopy.EnableStreaming = true;
    
                using(var dataReader = widgets.ToDataReader())
                {
                    bulkCopy.WriteToServer(dataReader);
                }
            }
        }
    }
    

