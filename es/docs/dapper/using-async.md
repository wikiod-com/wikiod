---
title: "Usando asíncrono"
slug: "usando-asincrono"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Llamar a un procedimiento almacenado
    public async Task<Product> GetProductAsync(string productId)
    {
        using (_db)
        {
            return await _db.QueryFirstOrDefaultAsync<Product>("usp_GetProduct", new { id = productId },
                commandType: CommandType.StoredProcedure);
        }
    }

## Llamar a un procedimiento almacenado e ignorar el resultado
    public async Task SetProductInactiveAsync(int productId)
    {
        using (IDbConnection con = new SqlConnection("myConnectionString"))
        {
            await con.ExecuteAsync("SetProductInactive", new { id = productId }, 
                commandType: CommandType.StoredProcedure);
        }
    }

