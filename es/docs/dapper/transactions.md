---
title: "Actas"
slug: "actas"
draft: false
images: []
weight: 9629
type: docs
toc: true
---

## Sintaxis
- conn.Execute(sql, transacción: tran); // especificar el parámetro por nombre
- conn.Execute(sql, parámetros, tran);
- conn.Query(sql, transacción: tran);
- conn.Query(sql, parámetros, tran);
- espera conn.ExecuteAsync(sql, transacción: tran); // asíncrono
- espera conn.ExecuteAsync(sql, parámetros, tran);
- espera conn.QueryAsync(sql, transacción: tran);
- espera conn.QueryAsync(sql, parámetros, tran);

## Acelerar las inserciones
Envolver un grupo de insertos en una transacción los acelerará de acuerdo con esta [Pregunta/respuesta de StackOverflow] (http://stackoverflow.com/questions/10689779/bulk-inserts-take-longer-than-expected-using-dapper) .

Puede usar esta técnica, o puede usar Bulk Copy para acelerar una serie de operaciones relacionadas para realizar.


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

## Uso de una transacción
Este ejemplo usa SqlConnection, pero se admite cualquier IDbConnection.

También se admite cualquier IDbTransaction desde la IDbConnection relacionada.

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

