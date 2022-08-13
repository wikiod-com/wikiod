---
title: "Using Async"
slug: "using-async"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Calling a Stored Procedure
    public async Task<Product> GetProductAsync(string productId)
    {
        using (_db)
        {
            return await _db.QueryFirstOrDefaultAsync<Product>("usp_GetProduct", new { id = productId },
                commandType: CommandType.StoredProcedure);
        }
    }

## Calling a stored procedure and ignoring the result
    public async Task SetProductInactiveAsync(int productId)
    {
        using (IDbConnection con = new SqlConnection("myConnectionString"))
        {
            await con.ExecuteAsync("SetProductInactive", new { id = productId }, 
                commandType: CommandType.StoredProcedure);
        }
    }

