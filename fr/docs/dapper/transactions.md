---
title: "Transactions"
slug: "transactions"
draft: false
images: []
weight: 9629
type: docs
toc: true
---

## Syntaxe
- conn.Execute(sql, transaction : tran); // spécifiez le paramètre par son nom
- conn.Execute(sql, paramètres, tran);
- conn.Query(sql, transaction : tran);
- conn.Query(sql, paramètres, tran);
- attendre conn.ExecuteAsync(sql, transaction : tran); // Asynchrone
- attendre conn.ExecuteAsync(sql, parameters, tran);
- attendre conn.QueryAsync(sql, transaction : tran);
- attendre conn.QueryAsync(sql, parameters, tran);

## Accélérez les insertions
Envelopper un groupe d'insertions dans une transaction les accélérera selon cette [StackOverflow Question/Answer](http://stackoverflow.com/questions/10689779/bulk-inserts-taking-longer-than-expected-using-dapper) .

Vous pouvez utiliser cette technique ou vous pouvez utiliser Bulk Copy pour accélérer une série d'opérations connexes à effectuer.


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

## Utilisation d'une transaction
Cet exemple utilise SqlConnection, mais toute IDbConnection est prise en charge.

De plus, toute IDbTransaction est prise en charge à partir de l'IDbConnection associée.

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

