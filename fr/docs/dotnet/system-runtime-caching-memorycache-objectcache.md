---
title: "System.Runtime.Caching.MemoryCache (ObjectCache)"
slug: "systemruntimecachingmemorycache-objectcache"
draft: false
images: []
weight: 9921
type: docs
toc: true
---

## Ajout d'un élément au cache (ensemble)
La fonction Set insère une entrée de cache dans le cache en utilisant une instance de CacheItem pour fournir la clé et la valeur de l'entrée de cache.

Cette fonction remplace `ObjectCache.Set(CacheItem, CacheItemPolicy)`

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
Cette fonction récupère le cache de formulaire d'élément existant, et si l'élément n'existe pas dans le cache, il récupère l'élément en fonction de la fonction valueFetchFactory.

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

