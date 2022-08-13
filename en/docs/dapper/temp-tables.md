---
title: "Temp Tables"
slug: "temp-tables"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

## Temp Table that exists while the connection remains open
When the temp table is created by itself, it will remain while the connection is open.

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

## How to work with temp tables
The point about temporary tables is that they're limited to the scope of the connection. Dapper will automatically open and close a connection if it's not already opened. That means that any temp table will be lost directly after creating it, if the connection passed to Dapper has not been opened.

This will not work:

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

On the other hand, these two versions will work:

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

