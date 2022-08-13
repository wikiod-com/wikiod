---
title: "Transactions"
slug: "transactions"
draft: false
images: []
weight: 9629
type: docs
toc: true
---

## Syntax
 - conn.Execute(sql, transaction: tran);  // specify the parameter by name
 - conn.Execute(sql, parameters, tran);
 - conn.Query(sql, transaction: tran);
 - conn.Query(sql, parameters, tran);
 - await conn.ExecuteAsync(sql, transaction: tran); // Async
 - await conn.ExecuteAsync(sql, parameters, tran);
 - await conn.QueryAsync(sql, transaction: tran);
 - await conn.QueryAsync(sql, parameters, tran);

## Speed up inserts
Wrapping a group of inserts in a transaction will speed them up according to this [StackOverflow Question/Answer](http://stackoverflow.com/questions/10689779/bulk-inserts-taking-longer-than-expected-using-dapper).

You can use this technique, or you can use Bulk Copy to speed up a series of related operations to perform.


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

## Using a Transaction
This example uses SqlConnection, but any IDbConnection is supported.

Also any IDbTransaction is supported from the related IDbConnection.

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

