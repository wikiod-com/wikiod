---
title: "işlemler"
slug: "islemler"
draft: false
images: []
weight: 9629
type: docs
toc: true
---

## Sözdizimi
- conn.Execute(sql, işlem: tran); // parametreyi ada göre belirtin
- conn.Execute(sql, parametreler, tran);
- conn.Query(sql, işlem: tran);
- conn.Query(sql, parametreler, tran);
- bekle conn.ExecuteAsync(sql, işlem: tran); // zaman uyumsuz
- bekle conn.ExecuteAsync(sql, parametreler, tran);
- bekle conn.QueryAsync(sql, işlem: tran);
- bekle conn.QueryAsync(sql, parametreler, tran);

## Ekleri hızlandırın
Bir işlemde bir grup eki sarmak, bu [StackOverflow Sorusu/Cevabına](http://stackoverflow.com/questions/10689779/bulk-inserts-taking-longer-than-expected-using-dapper) göre onları hızlandıracaktır. .

Bu tekniği kullanabilir veya gerçekleştirilecek bir dizi ilgili işlemi hızlandırmak için Toplu Kopyalamayı kullanabilirsiniz.


    // Widget has WidgetId, Name, and Quantity properties
    public void InsertWidgets(IEnumerable<Widget> widgets)
    {
        using(var conn = new SqlConnection("{connection string}")) {
            conn.Open();
    
            using(var tran = conn.BeginTransaction()) {
                try
                {
                    var sql = "insert Widget (WidgetId,Name,Quantity) Values(@WidgetId, @Name, @Quantity)";
                    conn.Execute(sql, widgets, tran);
                    tran.Commit();
                }
                catch(Exception ex)
                {
                    tran.Rollback();
                    // handle the error however you need to.
                    throw;
                }
            }
        }   
    }

## İşlem Kullanma
Bu örnek, SqlConnection kullanır, ancak herhangi bir IDbConnection desteklenir.

Ayrıca herhangi bir IDbTransaction, ilgili IDbConnection'dan desteklenir.

    public void UpdateWidgetQuantity(int widgetId, int quantity)
    {
        using(var conn = new SqlConnection("{connection string}")) {
            conn.Open();
    
            // create the transaction
            // You could use `var` instead of `SqlTransaction`
            using(SqlTransaction tran = conn.BeginTransaction()) {
                try
                {
                    var sql = "update Widget set Quantity = @quantity where WidgetId = @id";
                    var parameters = new { id = widgetId, quantity };
    
                    // pass the transaction along to the Query, Execute, or the related Async methods.
                    conn.Execute(sql, parameters, tran);
    
                    // if it was successful, commit the transaction
                    tran.Commit();
                }
                catch(Exception ex)
                {
                    // roll the transaction back
                    tran.Rollback();
    
                    // handle the error however you need to.
                    throw;
                }
            }
        }   
    }

