---
title: "System.Runtime.Caching.MemoryCache (ObjectCache)"
slug: "systemruntimecachingmemorycache-objectcache"
draft: false
images: []
weight: 9921
type: docs
toc: true
---

## Adicionando Item ao Cache (Set)
A função Set insere uma entrada de cache no cache usando uma instância de CacheItem para fornecer a chave e o valor para a entrada de cache.

Esta função substitui `ObjectCache.Set(CacheItem, CacheItemPolicy)`

    private static bool SetToCache()
    {
        string key = "Cache_Key";
        string value = "Cache_Value";

        //Get a reference to the default MemoryCache instance.
        var cacheContainer = MemoryCache.Default; 

        var policy = new CacheItemPolicy()
        {
            AbsoluteExpiration = DateTimeOffset.Now.AddMinutes(DEFAULT_CACHE_EXPIRATION_MINUTES)
         };
         var itemToCache = new CacheItem(key, value); //Value is of type object.
         cacheContainer.Set(itemToCache, policy);                
    }



## System.Runtime.Caching.MemoryCache (ObjectCache)
Esta função obtém o cache de formulário de item existente e, se o item não existir no cache, ele buscará o item com base na função valueFetchFactory.

        public static TValue GetExistingOrAdd<TValue>(string key, double minutesForExpiration, Func<TValue> valueFetchFactory)
        {            
            try
            {
                //The Lazy class provides Lazy initialization which will evaluate 
                //the valueFetchFactory only if item is not in the cache.
                var newValue = new Lazy<TValue>(valueFetchFactory);

                //Setup the cache policy if item will be saved back to cache.
                CacheItemPolicy policy = new CacheItemPolicy()
                {
                    AbsoluteExpiration = DateTimeOffset.Now.AddMinutes(minutesForExpiration)
                };

                //returns existing item form cache or add the new value if it does not exist.
                var cachedItem = _cacheContainer.AddOrGetExisting(key, newValue, policy) as Lazy<TValue>;

                return (cachedItem ?? newValue).Value;
            }
            catch (Exception excep)
            {
                return default(TValue);
            }
        }

