---
title: "System.Runtime.Caching.MemoryCache (ObjectCache)"
slug: "systemruntimecachingmemorycache-objectcache"
draft: false
images: []
weight: 9921
type: docs
toc: true
---

## Adición de elementos a la caché (conjunto)
La función Set inserta una entrada de caché en la memoria caché mediante el uso de una instancia de CacheItem para proporcionar la clave y el valor de la entrada de caché.

Esta función anula `ObjectCache.Set(CacheItem, CacheItemPolicy)`

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
Esta función obtiene el caché del formulario del elemento existente, y si el elemento no existe en el caché, obtendrá el elemento en función de la función valueFetchFactory.

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

