---
title: "HTTP Status Codes"
slug: "http-status-codes"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

In HTTP, status codes are a machine-readable mechanism indicating the result of a previously issued request. From [RFC 7231, sec. 6](https://tools.ietf.org/html/rfc7231#section-6): "The status-code element is a three-digit integer code giving the result of the attempt to understand and satisfy the request."

The [formal grammar](https://tools.ietf.org/html/rfc7230#section-3.1.2) allows codes to be anything between `000` and `999`. However, only the range from `100` to `599` has assigned meaning.

HTTP/1.1 defines a number of numeric [HTTP status codes](http://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml) that appear in the status line - the first line of an HTTP response - to summarise what the client should do with the response.

The first digit of a status codes defines the response’s class:

- `1xx` [Informational](https://tools.ietf.org/html/rfc7231#section-6.2)
- `2xx` [Client request successful](https://tools.ietf.org/html/rfc7231#section-6.3)
- `3xx` [Request redirected](https://tools.ietf.org/html/rfc7231#section-6.4) - further action necessary, such as a new request
- `4xx` [Client error](https://tools.ietf.org/html/rfc7231#section-6.5) - do not repeat the same request
- `5xx` [Server error](https://tools.ietf.org/html/rfc7231#section-6.6) - maybe try again

In practice, it is not always easy to choose the most appropriate status code.

## 404 Not Found
**HTTP 404 Not Found** means that the server couldn't find the path using the URI that the client requested.

    HTTP/1.1 404 Not Found

Most often, the requested file was deleted, but sometimes it can be a document root misconfiguration or a lack of permissions (though missing permissions more frequently triggers HTTP 403 Forbidden). 

For example, Microsoft's IIS writes 404.0 (0 is the sub-status) to its log files when the requested file was deleted. But when the incoming request is blocked by request filtering rules, it writes 404.5-404.19 to log files according to which rule blocks the request. A more detailed error code reference can be found at [Microsoft Support](https://support.microsoft.com/en-us/kb/943891).

## Responding to a conditional request for cached content
Send a **304 Not Modified** response status from the server send in response to a client request that contains headers `If-Modified-Since` and `If-None-Match`, if the request resource hasn’t changed.

For example if a client request for a web page includes the header `If-Modified-Since: Fri, 22 Jul 2016 14:34:40 GMT` and the page wasn’t modified since then, respond with the status line `HTTP/1.1 304 Not Modified`.

## 500 Internal Server Error
A **HTTP 500 Internal Server Error** is a general message meaning that the server encountered something unexpected. Applications (or the overarching web server) should use a 500 when there's an error processing the request - i.e. an exception is thrown, or a condition of the resource prevents the process completing.

Example status line:

    HTTP/1.1 500 Internal Server Error

## Denying access to protected files
Use 403 Forbidden when a client has requested a resource that is inaccessible due to existing access controls. For example, if your app has an `/admin` route that should only be accessible to users with administrative rights, you can use 403 when a normal user requests the page.

    GET /admin HTTP/1.1
    Host: example.com

<!-- -->

    HTTP/1.1 403 Forbidden

## Successful request
Send an HTTP response with status code `200` to indicate a successful request. The HTTP response status line is then:

    HTTP/1.1 200 OK

The status text `OK` is only informative. The response body (message payload) should contain a representation of the requested resource. If there is no representation 201 No Content should be used.

## Top 10 HTTP Status Code
# 2xx Success
- **200 OK** - Standard response for successful HTTP requests.
- **201 Created** - The request has been fulfilled, resulting in the creation of a new resource.
- **204 No Content** - The server successfully processed the request and is not returning any content.
# 3xx Redirection
- **304 Not Modified** - Indicates that the resource has not been modified since the version specified by the request headers `If-Modified-Since` or `If-None-Match`.
# 4xx Client Error
- **400 Bad Request** - The server cannot or will not process the request due to an apparent client error (e.g., malformed request syntax, too large size, invalid request message framing, or deceptive request routing).
- **401 Unauthorized** - *Similar to 403 Forbidden*, but specifically for use when authentication is required and has failed or has not yet been provided. The response must include a `WWW-Authenticate` header field containing a challenge applicable to the requested resource.
- **403 Forbidden** - The request was a valid request, but the server is refusing to respond to it. The user might be logged in but does not have the necessary permissions for the resource.
- **404 Not Found** - The requested resource could not be found but may be available in the future. Subsequent requests by the client are permissible.
- **409 Conflict** - Indicates that the request could not be processed because of conflict in the request, such as an edit conflict between multiple simultaneous updates.
# 5xx Server Error
- **500 Internal Server Error** - A generic error message, given when an unexpected condition was encountered and no more specific message is suitable.



