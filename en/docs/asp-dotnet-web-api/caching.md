---
title: "Caching"
slug: "caching"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Caching is the process of storing data somewhere for the future requests, in our case we can avoid the unwanted hit to database to get the data if we cache the data somewhere, this way we can make sure that the data is served in a faster manner.

## System.Runtime.Caching (MemoryCache)
Import the namespace System.Runtime.Caching(Make sure that you have added System.Runtime.Caching DLL to your project reference).

Create an instance of MemoryCache class.

    MemoryCache memCache = MemoryCache.Default;

**Add values to MemoryCache**

    public IQueryable<tblTag> GettblTags()
            {
                var ca = db.tblTags;
                memCache.Add("tag", ca, DateTimeOffset.UtcNow.AddMinutes(5));
                return db.tblTags;
            }

Here “tag” is my key and “ca” is my values and DateTimeOffset.UtcNow.AddMinutes(5) is for setting the cache for five minutes from now.

**Get values from MemoryCache**

    var res = memCache.Get("tag");
                if (res != null)
                {
                    return res;
                }
                else {
                    var ca = db.tblTags;
                    memCache.Add("tag", ca, DateTimeOffset.UtcNow.AddMinutes(5));
                    return db.tblTags;
                }

We will get the cache values in the variable res, remember this values will be there only for five minutes. You can always change that as per need. If the value is not null, we will just return it and do the manipulation and if it is null we will go ahead and fetch the data from database and add the value to cache. 

**Remove values from MemoryCache**

                if (memCache.Contains("tag"))
                {
                    memCache.Remove("tag");
                }

