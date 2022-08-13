---
title: "Encarts en vrac"
slug: "encarts-en-vrac"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

`WriteToServer` et `WriteToServerAsync` ont des surcharges qui acceptent les tableaux IDataReader (voir dans les exemples), DataTable et DataRow (`DataRow []`) comme source des données pour la copie en bloc.

## Copie groupée asynchrone
Cet exemple utilise une méthode `ToDataReader` décrite ici [Creating a Generic List DataReader for SqlBulkCopy](https://www.csvreader.com/posts/generic_list_datareader.php).

Cela peut également être fait en utilisant des méthodes non asynchrones.
    
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
    

## Copie en masse
Cet exemple utilise une méthode `ToDataReader` décrite ici [Creating a Generic List DataReader for SqlBulkCopy](https://www.csvreader.com/posts/generic_list_datareader.php).

Cela peut également être fait en utilisant des méthodes asynchrones.
    
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
    

