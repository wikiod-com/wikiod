---
title: "Accept-Encoding (Request)"
slug: "accept-encoding-request"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

`Accept-Encoding` tells the server what encoding the client accepts.  Encoding is mostly used for compression.

## Syntax
 * Accept-Encoding: Encoding;QualityFactor
 * Accept-Encoding: Encoding;QualityFactor, type;QualityFactor, type;QualityFactor, ...


## Parameters
| **Parameter** | **Description** |
| ------ | ------ |
| Encoding | The type of encoding to use.  This can also be a `*` to say the client has no preference to what encoding to use |
| QualityFactor | The quality factor in the format `;q=0.8`.  If this is set to 0 then it means "not acceptable".  (optional) |


`Accept-Encoding` takes a number of encoding and includes an optional preference for which one the server should use.  The encoding is one from the list
of available encodings at <a href='https://www.iana.org/assignments/http-parameters/http-parameters.xml#http-parameters-1'>IANA registry</a>.  For example `gzip`.

The encoding is separated by commas with an optional quality factor (using a `;q=`) that is
used the clients preference for using this encoding.  The quality factor has a value from 0 to 1 with the higher the number the more preference for that encoding.

If this header is not included then the client does not state any preference for the encoding.  It does not mean that the client supports all encodings.

A value of `identity` is always acceptable unless you reject it with `identity;q=0`.

The server uses `Content-Encoding` to inform the client what encoding it is using.

If the server can't find an acceptable charset to reply with then it should send a 406 (not acceptable) response or ignore this header and not
doing any content negotiation.


## Request gzip
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xml;q=0.9,*/*;q=0.8
<b>Accept-Encoding: gzip</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept gzip and identity encoding.


## Request gzip and deflate
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xml;q=0.9,*/*;q=0.8
<b>Accept-Encoding: compress, gzip</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept only gzip, compress, and identity encodings.


## Request compres but prefer gzip
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xml;q=0.9,*/*;q=0.8
<b>Accept-Encoding: gzip;q=1.0, compress;q=0.5</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept gzip, compress, and identity encoding but prefers gzip (which has a quality factor of 1.0).


## No preference for the type of encoding
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xml;q=0.9,*/*;q=0.8
<b>Accept-Encoding: *</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client has not preference for the type of encoding.


