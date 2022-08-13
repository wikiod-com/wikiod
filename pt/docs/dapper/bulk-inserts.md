---
title: "Inserções a granel"
slug: "insercoes-a-granel"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

O `WriteToServer` e o `WriteToServerAsync` têm sobrecargas que aceitam matrizes IDataReader (vistas nos exemplos), DataTable e DataRow (`DataRow[]`) como a origem dos dados para a cópia em massa.

## Cópia em massa assíncrona
Este exemplo usa um método `ToDataReader` descrito aqui [Creating a Generic List DataReader for SqlBulkCopy](https://www.csvreader.com/posts/generic_list_datareader.php).

Isso também pode ser feito usando métodos não assíncronos.
    
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
    

## Cópia em massa
Este exemplo usa um método `ToDataReader` descrito aqui [Creating a Generic List DataReader for SqlBulkCopy](https://www.csvreader.com/posts/generic_list_datareader.php).

Isso também pode ser feito usando métodos assíncronos.
    
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
    

