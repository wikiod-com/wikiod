---
title: "Bulk inserts"
slug: "bulk-inserts"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

The `WriteToServer` and `WriteToServerAsync` have overloads that accept IDataReader (seen in the examples), DataTable, and DataRow arrays (`DataRow[]`) as the source of the data for the Bulk Copy.

## Async Bulk Copy
This sample uses a `ToDataReader` method described here [Creating a Generic List DataReader for SqlBulkCopy](https://www.csvreader.com/posts/generic_list_datareader.php).

This can also be done using non-async methods.
    
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
    

## Bulk Copy
This sample uses a `ToDataReader` method described here [Creating a Generic List DataReader for SqlBulkCopy](https://www.csvreader.com/posts/generic_list_datareader.php).

This can also be done using async methods.
    
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
    

