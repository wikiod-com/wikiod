---
title: "Tabelas temporárias"
slug: "tabelas-temporarias"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

## Tabela temporária que existe enquanto a conexão permanece aberta
Quando a tabela temporária é criada sozinha, ela permanecerá enquanto a conexão estiver aberta.

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

## Como trabalhar com tabelas temporárias
O ponto sobre tabelas temporárias é que elas são limitadas ao escopo da conexão. O Dapper abrirá e fechará automaticamente uma conexão se ainda não estiver aberta. Isso significa que qualquer tabela temporária será perdida diretamente após criá-la, se a conexão passada ao Dapper não tiver sido aberta.

Isso não funcionará:

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

Por outro lado, essas duas versões funcionarão:

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

