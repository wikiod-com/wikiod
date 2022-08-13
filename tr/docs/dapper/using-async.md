---
title: "Zaman uyumsuz kullanma"
slug: "zaman-uyumsuz-kullanma"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Saklı Yordam Çağırma
    public async Task<Product> GetProductAsync(string productId)
    {
        using (_db)
        {
            return await _db.QueryFirstOrDefaultAsync<Product>("usp_GetProduct", new { id = productId },
                commandType: CommandType.StoredProcedure);
        }
    }

## Saklı yordamı çağırma ve sonucu yok sayma
    public async Task SetProductInactiveAsync(int productId)
    {
        using (IDbConnection con = new SqlConnection("myConnectionString"))
        {
            await con.ExecuteAsync("SetProductInactive", new { id = productId }, 
                commandType: CommandType.StoredProcedure);
        }
    }

