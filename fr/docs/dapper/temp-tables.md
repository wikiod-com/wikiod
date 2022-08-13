---
title: "Tableaux temporaires"
slug: "tableaux-temporaires"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

## Table temporaire qui existe tant que la connexion reste ouverte
Lorsque la table temporaire est créée par elle-même, elle restera tant que la connexion est ouverte.

    // Widget has WidgetId, Name, and Quantity properties
    public async Task PurchaseWidgets(IEnumerable<Widget> widgets)
    {
        using(var conn = new SqlConnection("{connection string}")) {
            await conn.OpenAsync();
    
            await conn.ExecuteAsync("CREATE TABLE #tmpWidget(WidgetId int, Quantity int)");
    
            // populate the temp table
            using(var bulkCopy = new SqlBulkCopy(conn)) {
                bulkCopy.BulkCopyTimeout = SqlTimeoutSeconds;
                bulkCopy.BatchSize = 500;
                bulkCopy.DestinationTableName = "#tmpWidget";
                bulkCopy.EnableStreaming = true;
    
                using(var dataReader = widgets.ToDataReader())
                {
                    await bulkCopy.WriteToServerAsync(dataReader);
                }
            }
    
            await conn.ExecuteAsync(@"
                update w
                set Quantity = w.Quantity - tw.Quantity
                from Widgets w
                    join #tmpWidget tw on w.WidgetId = tw.WidgetId");
        }
    }

## Comment travailler avec des tables temporaires
Le point sur les tables temporaires est qu'elles sont limitées à la portée de la connexion. Dapper ouvrira et fermera automatiquement une connexion si elle n'est pas déjà ouverte. Cela signifie que toute table temporaire sera perdue directement après sa création, si la connexion transmise à Dapper n'a pas été ouverte.

Cela ne fonctionnera pas :

    private async Task<IEnumerable<int>> SelectWidgetsError()
    {
      using (var conn = new SqlConnection(connectionString))
      {
        await conn.ExecuteAsync(@"CREATE TABLE #tmpWidget(widgetId int);");

        // this will throw an error because the #tmpWidget table no longer exists
        await conn.ExecuteAsync(@"insert into #tmpWidget(WidgetId) VALUES (1);");

        return await conn.QueryAsync<int>(@"SELECT * FROM #tmpWidget;");
      }
    }

En revanche, ces deux versions fonctionneront :

    private async Task<IEnumerable<int>> SelectWidgets()
    {
      using (var conn = new SqlConnection(connectionString))
      {
        // Here, everything is done in one statement, therefore the temp table
        // always stays within the scope of the connection
        return await conn.QueryAsync<int>(
          @"CREATE TABLE #tmpWidget(widgetId int);
            insert into #tmpWidget(WidgetId) VALUES (1);
            SELECT * FROM #tmpWidget;");
      }
    }

    private async Task<IEnumerable<int>> SelectWidgetsII()
    {
      using (var conn = new SqlConnection(connectionString))
      {
        // Here, everything is done in separate statements. To not loose the 
        // connection scope, we have to explicitly open it
        await conn.OpenAsync();

        await conn.ExecuteAsync(@"CREATE TABLE #tmpWidget(widgetId int);");
        await conn.ExecuteAsync(@"insert into #tmpWidget(WidgetId) VALUES (1);");
        return await conn.QueryAsync<int>(@"SELECT * FROM #tmpWidget;");
      }
    }

