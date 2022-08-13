---
title: "Cross Origin and Access Control"
slug: "cross-origin-and-access-control"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

[Cross-origin resource sharing][1] is designed to allow dynamic requests between domains, often using techniques such as [AJAX][2]. While the scripting does most of the work, the HTTP server must support the request using the correct headers.


[1]: https://www.w3.org/TR/cors
[2]:https://en.wikipedia.org/wiki/Ajax_(programming)

## Client: sending a cross-origin resource sharing (CORS) request
A cross-origin *request* must be sent including the `Origin` header. This indicates from where the request originated. For example, a cross-origin request from `http://example.com` to `http://example.org` would look like this:

    GET /cors HTTP/1.1
    Host: example.org
    Origin: example.com

The server will use this value to determine if the request is authorized.

## Server: responding to a CORS request
The response to a CORS request must include an `Access-Control-Allow-Origin` header, which dictates what origins are allowed to use the CORS resource. This header can take one of three values:

- An origin. Doing this permits requests from *that origin only*.
- The character `*`. This permits requests from *any origin*.
- The string `null`. This permits *no CORS requests*.

For example, on reception of a CORS request from the origin `http://example.com`, if `example.com` is an authorized origin, the server would send back this response:

    HTTP/1.1 200 OK
    Access-Control-Allow-Origin: example.com

An any-origin response would also permit this request, i.e.:

    HTTP/1.1 200 OK
    Access-Control-Allow-Origin: *

## Permitting user credentials or session
Allowing user credentials or the user's session to be sent with a CORS request allows the server to persist user data across CORS requests. This is useful if the server needs to check if the user is logged in before providing data (for example, only performing an action if a user is logged in - this would require the CORS request to be sent with credentials).

This can be achieved server-side for preflighted requests, by sending the `Access-Control-Allow-Credentials` header in response to the `OPTIONS` preflight request. Take the following case of a CORS request to `DELETE` a resource:

    OPTIONS /cors HTTP/1.1
    Host: example.com
    Origin: example.org
    Access-Control-Request-Method: DELETE

<!-- -->

    HTTP/1.1 200 OK
    Access-Control-Allow-Origin: example.org
    Access-Control-Allow-Methods: DELETE
    Access-Control-Allow-Credentials: true

The `Access-Control-Allow-Credentials: true` line indicates that the following `DELETE` CORS request may be sent with user credentials.


[1]: https://www.w3.org/TR/cors/#access-control-allow-credentials-response-header

## Preflighting requests
A basic CORS request is allowed to use one of only two methods:

- GET
- POST

and only a few select headers. POST CORS requests can additionally choose from only three content types.

To avoid this issue, requests that wish to use other methods, headers, or content types must first issue a *preflight* request, which is an `OPTIONS` request that includes access-control Request headers. For example, this is a preflight request that checks if the server will accept a `PUT` request that includes a `DNT` header:

    OPTIONS /cors HTTP/1.1
    Host: example.com
    Origin: example.org
    Access-Control-Request-Method: PUT
    Access-Control-Request-Headers: DNT

## Server: responding to preflight requests
When a server receives a preflight request, it must check if it supports the requested method and headers, and send back a response that indicates its ability to support the request, as well as any other permitted data (such as credentials).

These are indicated in access-control Allow headers. The server may also send back an access-control `Max-Age` header, indicating how long the preflight response can be cached for.

This is what a request-response cycle for a preflight request might look like:

    OPTIONS /cors HHTP/1.1
    Host: example.com
    Origin: example.org
    Access-Control-Request-Method: PUT
    Access-Control-Request-Headers: DNT

<!-- -->

    HTTP/1.1 200 OK
    Access-Control-Allow-Origin: example.org
    Access-Control-Allow-Methods: PUT
    Access-Control-Allow-Headers: DNT

