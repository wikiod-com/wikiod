---
title: "Caching HTTP responses"
slug: "caching-http-responses"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

Responses are cached separately for each URL and each HTTP method. 

HTTP caching is defined in [RFC 7234](http://httpwg.org/specs/rfc7234.html).

## Glossary

- **fresh** — state of a cached response, which hasn't expired yet. Typically, a fresh response can *satisfy* requests without a need to contact the server the response originated from.
- **stale** — state of a cached response, which is past its expiration date. Typically, stale responses can't be used to *satisfy* a request without checking with the server whether it's still valid.
- **satisfy** — cached response *satisfies* a request when all conditions in the request match the cached response, e.g. they have the same HTTP method and URL, the response is fresh or the request allows stale responses, request headers match headers listed in response's `Vary` header, etc.
- **revalidation** — checking whether a cached response is fresh. This is usually done with a *conditional request* containing `If-Modified-Since` or `If-None-Match` and response status `304`.
- **private cache** — cache for a single user, e.g. in a web browser. Private caches can store personalized responses.
- **public cache** — cache shared between many users, e.g. in a proxy server. Such cache can send the same response to multiple users.


## Changing cached resources
The easiest method to bypass cache is to change the URL. This is used as a best practice when the URL contains a version or a checksum of the resource, e.g.

    http://example.com/image.png?version=1
    http://example.com/image.png?version=2

These two URLs will be cached separately, so even if `…?version=1` was cached *forever*, a new copy could be immediately retrieved as `…?version=2`.

Please don't use random URLs to bypass caches. Use `Cache-control: no-cache` or `Cache-control: no-store` instead. If responses with random URLs are sent without the `no-store` directive, they will be unnecessarily stored in caches and push out more useful responses out of the cache, degrading performance of the entire cache.



## Cache response for everyone for 1 year
    Cache-Control: public, max-age=31536000

`public` means the response is the same for all users (it does not contain any personalized information). `max-age` is in seconds from now. 31536000 = 60 * 60 * 24 * 365.

This is recommended for static assets that are never meant to change.

## Cache personalized response for 1 minute
    Cache-Control: private, max-age=60

`private` specifies that the response can be cached only for user who requested the resource, and can't be reused when other users request the same resource. This is appropriate for responses that depend on cookies.

## Stop use of cached resources without checking with the server first
    Cache-Control: no-cache

The client will behave as if the response was not cached. This is appropriate for resources that can unpredictably change at any time, and which users must always see in the latest version.

Responses with `no-cache` will be slower (high latency) due to need to contact the server every time they're used.

However, to save bandwidth, the clients *may* still store such responses. Responses with `no-cache` won't be used to satisfy requests without contacting the server each time to check whether the cached response can be reused.


## Request responses not to be stored at all
     Cache-control: no-store

Instructs clients no to cache the response in any way, and to forget it at soon as possible.

This directive was originally designed for sensitive data (today HTTPS should be used instead), but can be used to avoid polluting caches with responses that can't be reused.

It's appropriate only in specific cases where the response data is always different, e.g. an API endpoint that returns a large random number. Otherwise, `no-cache` and revalidation can be used to have a behavior of "uncacheable" response, while still being able to save some bandwidth.


## Obsolete, redundant and non-standard headers
* `Expires` — specifies date when the resource becomes stale. It relies on servers and clients having accurate clocks and supporting time zones correctly. `Cache-control: max-age` takes precedence over `Expires`, and is generally more reliable.

* `post-check` and `pre-check` directives are non-standard Internet Explorer extensions that enable use of stale responses. The standard alternative is [`stale-while-revalidate`](https://tools.ietf.org/rfc/rfc5861.txt).

* `Pragma: no-cache` — obsoleted in [1999](http://www.ietf.org/rfc/rfc2616.txt). `Cache-control` should be used instead.


