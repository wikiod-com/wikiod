---
title: "Caching"
slug: "caching"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Caching helps in improving performance of an application by maintaining easily accessible copy of the data. *Aspnet Core* comes with two easy to use and testing friendly caching abstractions.

**Memory Cache** will store data in to local server's memory caching.

**Distributed Cache** will hold the data cache in a centralized location which is accessible by servers in cluster. It comes with three implementations out of the box : In Memory (for unit testing and local dev), Redis and Sql Server.

## Using InMemory cache in ASP.NET Core application

To use an in memory cache in your ASP.NET application, add the following dependencies to your `project.json` file:

      "Microsoft.Extensions.Caching.Memory": "1.0.0-rc2-final",

add the cache service (from Microsoft.Extensions.Caching.Memory) to ConfigureServices method in Startup class

    services.AddMemoryCache();

To add items to the cache in our application, we will use `IMemoryCache` which can be injected to any class (for example Controller) as shown below.

    private IMemoryCache _memoryCache;
    public HomeController(IMemoryCache memoryCache)
    {
        _memoryCache = memoryCache;
    }


**Get** will return the value if it exists, but otherwise returns `null`.

        // try to get the cached item; null if not found
        // greeting = _memoryCache.Get(cacheKey) as string;

        // alternately, TryGet returns true if the cache entry was found
        if(!_memoryCache.TryGetValue(cacheKey, out greeting))

Use the **Set** method to write to the cache. Set accepts the key to use to look up the value, the value to be cached, and a set of `MemoryCacheEntryOptions`. The `MemoryCacheEntryOptions` allow you to specify absolute or sliding time-based cache expiration, caching priority, callbacks, and dependencies. One of the sample below-

    _memoryCache.Set(cacheKey, greeting,
                    new MemoryCacheEntryOptions()
                    .SetAbsoluteExpiration(TimeSpan.FromMinutes(1)));

## Distributed Caching
To leverage distributed cache, you'll have to reference one of the available implementations :
* [Redis](https://www.nuget.org/packages/Microsoft.Extensions.Caching.Redis/)
* [Sql server](https://www.nuget.org/packages/Microsoft.Extensions.Caching.SqlServer/)

For instance you'll register Redis implemention as follows :
```
public void ConfigureServices(IServiceCollection services)
{
    services.AddDistributedRedisCache(options =>
    {
        options.Configuration = "ServerAdress";
        options.InstanceName = "InstanceName";
    });
}
```

Require `IDistributedCache` dependency where you need it:

```csharp
public class BooksController {
    private IDistributedCache distributedCache;

    public BooksController(IDistributedCache distributedCache) {
        this.distributedCache = distributedCache;
    }

    [HttpGet]
    public async Task<Books[]> GetAllBooks() {
        var serialized = this.distributedCache.GetStringAsync($"allbooks");
        Books[] books = null;
        if (string.IsNullOrEmpty(serialized)) {
            books = await Books.FetchAllAsync();
            this.distributedCache.SetStringAsync($"allbooks", JsonConvert.SerializeObject(books));
        } else {
            books = JsonConvert.DeserializeObject<Books[]>(serialized);
        }
        return books;
    }
}
```

