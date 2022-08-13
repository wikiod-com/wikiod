---
title: "HTTP responses"
slug: "http-responses"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Parameters
| Status Code | Reason-Phrase — Description |
|-------------|-----------------------------|
| 100 | **Continue** — the client should send the following part of a multi-part request. |
| 101 | **Switching Protocols** — the server is changing the version or type of protocol used in this communication. |
| 200 | **OK** — the server has received and completed the client's request. |
| 201 | **Created** — the server has accepted the request and created a new resource, which is available under the URI in the `Location` header. |
| 202 | **Accepted** — the server has received and accepted the client's request, but it has not yet started or completed processing. |
| 203 | **Non-Authoritative Information** — the server is returning data that may be a sub- or superset of the information available on the original server. Mainly used by proxies. |
| 204 | **No Content** — used in place of 200 (OK) when there is no body to the response. |
| 205 | **Reset Content** — identical to 204 (No Content), but the client should reload the active document view. |
| 206 | **Partial Content** — used in place of 200 (OK) when the client requested a `Range` header. |
| 300 | **Multiple Choices** — the requested resource is available at multiple URIs, and the client should redirect the request to a URI specified in the list in the message body. |
| 301 | **Moved Permanently**  — the requested resource is no longer available at this URI, and the client should redirect this and all future requests to the URI specified in the `Location` header. |
| 302 | **Found** — the resource temporarily resides under a different URI. This request should be redirected on user confirmation to the URI in the `Location` header, but future requests should not be altered. |
| 303 | **See Other** — very similar to 302 (Found), but does not require user input to redirect to the provided URI. The provided URI should be retrieved with a GET request. |
| 304 | **Not Modified** — the client sent an `If-Modified-Since` or similar header, and the resource has not been modified since that point; the client should display a cached copy of the resource. |
| 305 | **Use Proxy** — the requested resource must be requested again through the proxy specified in the `Location` header field. |
| 307 | **Temporary Redirect** — identical to 302 (Found), but HTTP 1.0 clients do not support 307 responses. |
| 400 | **Bad Request** — the client sent a malformed request containing syntax errors, and should modify the request to correct this before repeating it. |
| 401 | **Unauthorized** — the requested resource is not available without authentication. The client may repeat the request using an `Authorization` header to provide authentication details. |
| 402 | **Payment Required** — reserved, unspecified status code for use by applications that require user subscriptions to view content. |
| 403 | **Forbidden** — the server understands the request, but refuses to fulfil it due to existing access controls. The request should not be repeated. |
| 404 | **Not Found** — there is no resource available on this server that matches the requested URI. May be used in place of 403 to avoid exposing access control details. |
| 405 | **Method Not Allowed** — the resource does not support the request method (HTTP verb); the `Allow` header lists acceptable request methods. |
| 406 | **Not Acceptable** — the resource has characteristics that violate the accept  headers sent in the request. |
| 407 | **Proxy Authentication Required** — similar to 401 (Unauthorized), but indicates that the client must first authenticate with the intermediate proxy. |
| 408 | **Request Timeout** — the server expected another request from the client, but none were provided within an acceptable timeframe. |
| 409 | **Conflict** — the request could not be completed because it conflicted with the current state of the resource. |
| 410 | **Gone** — similar to 404 (Not Found), but indicates a permanent removal. No forwarding address is available. |
| 411 | **Length Required** — the client did not specify a valid `Content-Length` header, and must do so before the server will accept this request. |
| 412 | **Precondition Failed** — the resource is not available with all the conditions specified by the conditional headers sent by the client. |
| 413 | **Request Entity Too Large** — the server is presently unable to process a message body of the length that the client sent. |
| 414 | **Request-URI Too Long** — the server is refusing the request because the Request-URI is longer than the server is willing to interpret. |
| 415 | **Unsupported Media Type** — the server does not support the MIME or media type specified by the client, and cannot service this request. |
| 416 | **Requested Range Not Satisfiable** — the client requested a range of bytes, but the server cannot provide content to that specification. |
| 417 | **Expectation Failed** — the client specified constraints in the `Expect` header that the server cannot meet. |
| 500 | **Internal Server Error** — the server met an unexpected condition or error which prevents it from completing this request. |
| 501 | **Not Implemented** — the server does not support the functionality required to complete the request. Usually used to indicate a request method that is not supported on *any* resource. |
| 502 | **Bad Gateway** — the server is a proxy, and received an invalid response from the upstream server while processing this request. |
| 503 | **Service Unavailable** — the server is under high load or undergoing maintenance, and does not have the capacity to serve this request at present. |
| 504 | **Gateway Timeout** — the server is a proxy, and did not receive a response from the upstream server in a timely manner. |
| 505 | **HTTP Version Not Supported** — the server does not support the version of the HTTP protocol that the client made its request with. |

## Basic response format
When an HTTP server receives a well-formed [HTTP request][1], it must process the information that request contains and return a response to the client. A simple HTTP 1.1 response, may look like any of the following, usually followed by a number of header fields, and possibly a response body:

    HTTP/1.1 200 OK \r\n

<!-- -->

    HTTP/1.1 404 Not Found \r\n

<!-- -->

    HTTP/1.1 503 Service Unavailable \r\n

A simple HTTP 1.1 response has this format:

    HTTP-Version Status-Code Reason-Phrase CRLF

As in a request, `HTTP-Version` indicates the version of the HTTP protocol in use; for HTTP 1.1 this must always be the string `HTTP/1.1`.

`Status-Code` is a three-digit code that indicates the status of the client's request. The first digit of this code is the *status class*, which places the status code into one of 5 categories of response <sup>[\[1\]][2]</sup>:

- `1xx` **Informational** - the server has received the request and processing is continuing
- `2xx` **Success** - the server has accepted and processed the request
- `3xx` **Redirection** - further action is necessary on the client's part to complete the request
- `4xx` **Client Errors** - the client sent a request that was malformed or cannot be fulfilled
- `5xx` **Server Errors** - the request was valid, but the server cannot fulfil it at present

`Reason-Phrase` is a short description of the status code. For example, code `200` has a reason phrase of `OK`; code `404` has a phrase of `Not Found`. A full list of reason phrases is available in Parameters, below, or in the [HTTP specification][2].

The line ends with a carriage return—line feed pair, usually represented by `\r\n`.

[1]: https://www.wikiod.com/http/http-requests
[2]: https://tools.ietf.org/html/rfc7231#section-6

## Additional Headers
Like an HTTP request, an HTTP response may include additional headers to modify or augment the response it provides.

A full list of available headers is defined in [§6.2 of the specification][1]. The most commonly-used headers are:

- `Server`, which functions like a [`User-Agent` request header][2] for the server;
- `Location`, which is used on 201 and 3xx status responses to indicate a URI to redirect to; and
- `ETag`, which is a unique identifier for this version of the returned resource to enable clients to cache the response.

Response headers come after the status line, and as with [request headers][2] are formed as such:

    Name: Value CRLF

`Name` provides the header name, such as `ETag` or `Location`, and `Value` provides the value that the server is setting for that header. The line ends with a CRLF.

A response with headers might look like this:

    HTTP/1.1 201 Created \r\n
    Server: WEBrick/1.3.1 \r\n
    Location: http://example.com/files/129742 \r\n


[1]: https://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.2
[2]: https://www.wikiod.com/http/http-requests/9848/additional-headers#t=201607230904445800258

## Message Bodies
As with [request bodies][1], HTTP responses may contain a message body. This provides additional data that the client will process. Notably, 200 OK responses to a well-formed GET request should always provide a message body containing the requested data. (If there is none, 204 No Content is a more appropriate response).

A message body is included after all headers and a double CRLF. As for requests, its length in bytes should be given with `Content-Length` header. A successful response to a GET request, therefore, might look like this:

    HTTP/1.1 200 OK\r\n
    Server: WEBrick/1.3.1\r\n
    Content-Length: 39\r\n
    ETag: 4f7e2ed02b836f60716a7a3227e2b5bda7ee12c53be282a5459d7851c2b4fdfd\r\n
    \r\n
    Nobody expects the Spanish Inquisition.


[1]: https://www.wikiod.com/http/http-requests/9849/message-bodies#t=201607230904445800258

