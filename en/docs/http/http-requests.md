---
title: "HTTP requests"
slug: "http-requests"
draft: false
images: []
weight: 9642
type: docs
toc: true
---

## Parameters
| HTTP Method | Purpose |
|-------------|---------|
| `OPTIONS` | Retrieve information about the communication options (available methods and headers) available on the specified request URI. |
| `GET` | Retrieve the data identified by the request URI, or the data produced by the script available at the request URI. |
| `HEAD` | Identical to `GET` except that no message body will be returned by the server: only headers. |
| `POST` | Submit a block of data (specified in the message body) to the server for addition to the resouce specified in the request URI. Most commonly used for form processing. |
| `PUT` | Store the enclosed information (in the message body) as a new or updated resource under the given request URI. |
| `DELETE` | Delete, or queue for deletion, the resource identified by the request URI.
| `TRACE` | Essentially an echo command: a functioning, compliant HTTP server must send the entire request back as the body of a 200 (OK) response.

The `CONNECT` method is [reserved by the method definitions specification](https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html#sec9.9) for use with proxies that are able to switch between proxying and tunneling modes (such as for SSL tunneling).

## Sending a minimal HTTP request manually using Telnet


## Basic request format
In HTTP 1.1, a minimal HTTP request consists of a request line and a `Host` header:

    GET /search HTTP/1.1 \r\n
    Host: google.com \r\n
    \r\n

The first line has this format:

    Method Request-URI HTTP-Version CRLF

`Method` should be a valid HTTP method; one of <sup>[\[1\]][1][\[2\]][2]</sup>:

- `OPTIONS`
- `GET`
- `HEAD`
- `POST`
- `PUT`
- `DELETE`
- `PATCH`
- `TRACE`
- `CONNECT`

`Request-URI` indicates either the URI or the path to the resource that the client is requesting. It can be either:

- a fully-qualified URI, including scheme, host, (optional) port and path; or
- a path, in which case the host must be specified in the `Host` header

`HTTP-Version` indicates the version of the HTTP protocol the client is using. For HTTP 1.1 requests this must always be `HTTP/1.1`.

The request line ends with a carriage return—line feed pair, usually represented by `\r\n`.

[1]: https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html
[2]: https://tools.ietf.org/html/rfc5789

## Request header fields
Header fields (usually just called ‘headers’) may be added to an HTTP request to provide additional information with the request. A header has semantics similar to parameters passed to a method in any programming language that supports such things.

A request with `Host`, `User-Agent` and `Referer` headers might look like this:

    GET /search HTTP/1.1 \r\n
    Host: google.com \r\n
    User-Agent: Chrome/54.0.2803.1 \r\n
    Referer: http://google.com/ \r\n
    \r\n

A full list of supported HTTP 1.1 request headers can be found in [the specification][1]. The most common are:

- `Host` - the host name part of the request URL (required in HTTP/1.1)
- `User-Agent` - a string that represents the user agent requesting;
- `Referer` - the URI that the client was referred here from; and
- `If-Modified-Since` - gives a date that the server can use to determine if a resource has changed and indicate that the client can used a cached copy if it has not.

A header should be formed as `Name: Value CRLF`. `Name` is the header name, such as `User-Agent`. `Value` is the data assigned to it, and the line should end with a CRLF. Header names are case-insensitive and may only use letters, digits and the characters ``!#$%&'*+-.^_`|~`` (RFC7230 section [3.2.6 Field value components][2]).

The `Referer` header field name is a typo for ‘referrer’, introduced accidentally in [RFC1945][3].

[1]: http://httpwg.org/specs/rfc7231.html#request.header.fields
[2]: http://httpwg.org/specs/rfc7230.html#field.components
[3]: https://tools.ietf.org/html/rfc1945

## Message bodies
Some HTTP requests may contain a message body. This is additional data that the server will use to process the request. Message bodies are most often used in POST or PATCH and PUT requests, to provide new data that the server should apply to a resource.

Requests that include a message body should always include its length in bytes with `Content-Length` header.

A message body is included *after* all headers and a double CRLF. An example PUT request with a body might look like this:

    PUT /files/129742 HTTP/1.1\r\n
    Host: example.com\r\n
    User-Agent: Chrome/54.0.2803.1\r\n
    Content-Length: 202\r\n
    \r\n
    This is a message body. All content in this message body should be stored under the 
    /files/129742 path, as specified by the PUT specification. The message body does
    not have to be terminated with CRLF.

`HEAD` and `TRACE` requests must not include a message body.

