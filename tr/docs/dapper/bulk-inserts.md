---
title: "Toplu ekler"
slug: "toplu-ekler"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

"WriteToServer" ve "WriteToServerAsync", Toplu Kopyalama için veri kaynağı olarak IDataReader (örneklerde görülmektedir), DataTable ve DataRow dizilerini ("DataRow[]") kabul eden aşırı yüklemelere sahiptir.

## Zaman Uyumsuz Toplu Kopyalama
Bu örnek, burada [SqlBulkCopy için Genel Liste Veri Okuyucusu Oluşturma](https://www.csvreader.com/posts/generic_list_datareader.php) açıklanan bir "ToDataReader" yöntemini kullanır.

Bu, zaman uyumsuz yöntemler kullanılarak da yapılabilir.
    
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
    

## Toplu Kopyalama
Bu örnek, burada [SqlBulkCopy için Genel Liste Veri Okuyucusu Oluşturma](https://www.csvreader.com/posts/generic_list_datareader.php) açıklanan bir "ToDataReader" yöntemini kullanır.

Bu, zaman uyumsuz yöntemler kullanılarak da yapılabilir.
    
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
    

