---
title: "Response encodings and compression"
slug: "response-encodings-and-compression"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## HTTP compression
The HTTP message body can be compressed (since HTTP/1.1). Either by the server compresses the request and adds a `Content-Encoding` header, or by a proxy does and adds a `Transfer-Encoding` header.

A client may send an `Accept-Encoding` request header to indicate which encodings it accepts.

The most commonly used encodings are:

* gzip - deflate algorithm (LZ77) with CRC32 checksum implemented in "gzip" file's compression program ([RFC1952][1])
* deflate - "zlib" data format ([RFC1950][2]), deflate algorithm (hybrid LZ77 and Huffman) with Adler32 checksum


  [1]: https://tools.ietf.org/html/rfc1952
  [2]: https://tools.ietf.org/html/rfc1950


## Multiple compression methods
It is possible to compress an HTTP response message body more than once. The encoding names should then be separated by a comma in the order in which they were applied. For example, if a message has been compressed via deflate and then gzip, the header should look like:

    Content-Encoding: deflate, gzip

Multiple `Content-Encoding` headers are also valid, though not recommended:

    Content-Encoding: deflate
    Content-Encoding: gzip


## gzip compression
The client first sends a request with an `Accept-Encoding` header that indicates it supports gzip:

    GET / HTTP/1.1\r\n
    Host: www.google.com\r\n
    Accept-Encoding: gzip, deflate\r\n
    \r\n

The server may then send a response with a compressed response body and a `Content-Encoding` header that specifies that gzip encoding was used::

    HTTP/1.1 200 OK\r\n
    Content-Encoding: gzip\r\n
    Content-Length: XX\r\n
    \r\n
    ... compressed content ...



