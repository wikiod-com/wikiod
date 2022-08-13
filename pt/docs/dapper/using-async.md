---
title: "Usando assíncrono"
slug: "usando-assincrono"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Chamando um procedimento armazenado
    public async Task<Product> GetProductAsync(string productId)
    {
        using (_db)
        {
            return await _db.QueryFirstOrDefaultAsync<Product>("usp_GetProduct", new { id = productId },
                commandType: CommandType.StoredProcedure);
        }
    }

## Chamando um procedimento armazenado e ignorando o resultado
    public async Task SetProductInactiveAsync(int productId)
    {
        using (IDbConnection con = new SqlConnection("myConnectionString"))
        {
            await con.ExecuteAsync("SetProductInactive", new { id = productId }, 
                commandType: CommandType.StoredProcedure);
        }
    }

