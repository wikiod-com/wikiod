---
title: "Transações"
slug: "transacoes"
draft: false
images: []
weight: 9629
type: docs
toc: true
---

## Sintaxe
- conn.Execute(sql, transação: tran); // especifica o parâmetro pelo nome
- conn.Execute(sql, parâmetros, tran);
- conn.Query(sql, transação: tran);
- conn.Query(sql, parâmetros, tran);
- aguarde conn.ExecuteAsync(sql, transação: tran); // Assíncrono
- aguarde conn.ExecuteAsync(sql, parâmetros, tran);
- aguarde conn.QueryAsync(sql, transação: tran);
- aguarde conn.QueryAsync(sql, parâmetros, tran);

## Acelerar inserções
Envolver um grupo de inserções em uma transação irá acelerá-los de acordo com esta [StackOverflow Question/Answer](http://stackoverflow.com/questions/10689779/bulk-inserts-taking-longer-than-expected-using-dapper) .

Você pode usar essa técnica ou pode usar a cópia em massa para acelerar uma série de operações relacionadas a serem executadas.


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

## Usando uma transação
Este exemplo usa SqlConnection, mas qualquer IDbConnection é compatível.

Além disso, qualquer IDbTransaction é compatível com o IDbConnection relacionado.

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

