---
title: "EdgeCache"
slug: "edgecache"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

**Details**

* When Edge Cache is enabled and working, App Engine sends an `age` header whose value is the time (in seconds) since the response has been cached. For example, if the response has been cached for two minutes thus far, the response will include a header of `age: 120`. If no `age` header is being sent, it's a good sign that you haven't gotten Edge Cache to work yet. Additionally, when the response doesn't include an `age` header, it means the request missed Edge Cache.
* Edge Cache doesn't work in the local development environment.
* There are different caches for different regions. There are separate caches for separate regions of the world. For example, a request from Europe might be hit the cache, while a request at the exact same time from Australia may not hit Edge Cache.
* There are multiple caches even in the same region. For example, two sequential requests from the same client may receive responses with different values for the `age` header.
* Google evicts content from Edge Cache before the max-age has been reached. This is especially true if the resource has not been requested for more than 5 minutes.
* Google automatically sends a `Vary` header with the value `Accept-Encoding` with some responses. See [this documentation](https://developers.google.com/appengine/docs/python/requests#Python_Responses).
* App Engine will not allow you to set the `Cache-Control` header to public if you also have a `Set-Cookie` header present. App Engine will even change the value of the `Cache-Control` header from `public` to `private` if there is a `Set-Cookie` header. See [here](https://developers.google.com/appengine/docs/python/requests#Python_Responses).

**How EdgeCache works**

EdgeCache is a reverse proxy cache that stores data for a certain period of time, and returns it quickly upon seeing the same request as long as the cache is still valid.

Here is a diagram from [this video](https://www.youtube.com/watch?v=QJp6hmASstQ) from the App Engine team about how EdgeCache works:

![EdgeCache Diagram](http://i.imgur.com/DJOWOLC.png)

The "Google Front End" data centers are located around the world and can store cached data to be returned quickly upon request without ever having to run any of your App Engine code (which runs on the "App Engine Front End").

See [this StackOverflow answer](http://stackoverflow.com/a/366212/1206439) for more information about reverse proxies in general.

**Current Situation**

Unfortunately, the current state of knowledge about GAE's EdgeCache is pretty bad. The extent of the documentation on this secretive feature can be found in [this forum post](https://groups.google.com/forum/#!topic/google-appengine/6xAV2Q5x8AU/discussion) (read it now!) from 2011 and the 24 seconds between 11:11 and 11:35 in [this video](http://youtu.be/QJp6hmASstQ?t=11m11s) from Google.

**More Resources**

Below is the list of more resources we've found pertaining to App Engine's EdgeCache feature:

* [App Engine Architecture and Services video](http://youtu.be/QJp6hmASstQ?t=11m11s)
* [Fantastic forum post from Brandon Wirtz](https://groups.google.com/forum/#!topic/google-appengine/6xAV2Q5x8AU/discussion)
* [Google App Engine Issue #2258](https://code.google.com/p/googleappengine/issues/detail?id=2258) (which has been open since 2009)
* StackOverflow question: [Details on Google App Engine's caching proxy?](http://stackoverflow.com/questions/3947643/details-on-google-app-engines-caching-proxy)
* [A Word on App Engine Caching](https://davepeck.org/2011/10/25/a-word-on-app-engine-caching/)
* [Configuring EdgeCache](http://learntogoogleit.com/post/54611844088/configuring-edgecache-a-cache-for-app-engine)


## Enabling EdgeCache
* In order to enable EdgeCache, set the following HTTP response headers (*Do not deviate from this exact format*):
    * `Cache-Control` header to `public, max-age=X` where:
        * `X = the number of seconds that you want the response to be cached for`
        * `X > 60 seconds`
        * `X < 365*24*60*60`
    * Set the `Pragma` header to `Public`. 


