---
title: "Getting started with http-headers"
slug: "getting-started-with-http-headers"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## HTTP Request
A simple HTTP Request for the resource `/index.html`. The host `www.example.com` is specified in the HTTP `Host` header.

````
GET /index.html HTTP/1.1
Host: www.example.com
````


## HTTP Response
A possible response to the request above. The response contains the HTTP headers `Date`, `Content-Type`, `Content-Encoding` and `Content-Length`.

````
HTTP/1.1 200 OK
Date: Wed, 21 Jun 2017 10:58:03 GMT
Content-Type: text/html; charset=UTF-8
Content-Encoding: UTF-8
Content-Length: 150

<response body>
````

The response body is sent after the headers, separated by a blank line.

