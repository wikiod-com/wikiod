---
title: "Utilisation de l'asynchrone"
slug: "utilisation-de-lasynchrone"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Appel d'une procédure stockée
    public async Task<Product> GetProductAsync(string productId)
    {
        using (_db)
        {
            return await _db.QueryFirstOrDefaultAsync<Product>("usp_GetProduct", new { id = productId },
                commandType: CommandType.StoredProcedure);
        }
    }

## Appeler une procédure stockée et ignorer le résultat
    public async Task SetProductInactiveAsync(int productId)
    {
        using (IDbConnection con = new SqlConnection("myConnectionString"))
        {
            await con.ExecuteAsync("SetProductInactive", new { id = productId }, 
                commandType: CommandType.StoredProcedure);
        }
    }

