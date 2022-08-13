---
title: "HTTP Strict Transport Security (HSTS)"
slug: "http-strict-transport-security-hsts"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Parameters
| Parameter | Details |
| ------ | ------ |
| `max-age=31536000`   | Time in seconds. HSTS will be enforced for this future time period.   |
| `includeSubDomains` | HSTS should be applied for this domain and all of its sub-domains. |
| `preload` | This domain agrees to be included in a [HSTS pre-load list](https://hstspreload.appspot.com/) |



See also

* [MDN - HTTP Strict Transport Security](https://developer.mozilla.org/en-US/docs/Web/Security/HTTP_strict_transport_security)
* [Wikipedia - HTTP Strict Transport Security](https://en.wikipedia.org/wiki/HTTP_Strict_Transport_Security)
* [HSTS preload list](https://hstspreload.appspot.com/)

## HSTS Header
    Strict-Transport-Security: max-age=31536000; includeSubDomains

`Strict-Transport-Security` is a promise to the browser that all future requests to this domain will be secure.  
For the future time period `max-age`:

* All outgoing HTTP requests from the browser will be converted to HTTPS *on the client* (not an HTTP redirect).
* If the certificate is invalid (e.g. outdated or self-singed), the user will be unable to white-list it and the site will remain inaccessible.

HSTS behavior is meant to eliminate Man-in-the-Middle attacks that use HTTPS stripping, issuing of invalid certificates (and expecting the user to add and exception), and redirecting on HTTP requests to another destination.

## HSTS preload list
    Strict-Transport-Security: max-age=31536000; includeSubDomains; preload

HSTS is activated only after a successful HTTPS request to the server with a valid certificate. There is still a risk of a first-time user accessing the site, at which point a Man-in-the-Middle attack is possible.  
To make the site secure even before the first request the domain can be [added to a preload list](https://hstspreload.appspot.com/), already configured in browsers.  
The `preload` parameter is not used by the browsers directly, but it an indiciation to the browser developers that the site developers really asked to be added to the preload list.

