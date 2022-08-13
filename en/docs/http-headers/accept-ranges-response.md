---
title: "Accept-Ranges (Response)"
slug: "accept-ranges-response"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

`Accept-Ranges` tells the client that this server will supports ranges for this resource (file).


## Syntax
 * Accept-Ranges: RangeType
 * Accept-Ranges: none


## Parameters
| **Parameter** | **Description** |
| ------ | ------ |
| RangeType | That type of ranges are supported.  This is currently only `bytes` or `none`.  |
| none | The server does not support ranges on this resource |


`Accept-Ranges` is part of the ranges system.  The ranges system lets the client request only part of a file instead of having to download the whole file.

For example if a client only needs the last 100 bytes of a 10M file it can request the server only send data from offset 10485660 to 10485760.

`Accept-Ranges` is sent from the server to tell the client if it supports ranges.  This only applies to this particular resource (file), other files may accept different
range types.

Only two values are currently defined, `bytes` and `none`.  The values `bytes` means that you can request byte ranges (offset and end will be in bytes).
A value of 'none' means the server does not support ranges.

Clients are free to request byte range requests without checking if the server supports ranges.

The client uses `Range` to request a range from the server and the server replies with a status of 206 (Partial Content) if it is sending the range of bytes or
200 (ok) if it is going to send the whole file.


## Server supports ranges
### Request:"http://example.com" ###

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

### Response: ###

<pre>
HTTP/1.1 200 OK
Date: Sat, 01 Jan 2000 01:00:00 GMT
Server: Apache/2.4.10 (Win32) OpenSSL/1.0.1h PHP/5.4.31
Keep-Alive: timeout=5, max=97
Connection: Keep-Alive
Content-Type: text/html
<b>Accept-Ranges: bytes</b>
Content-Length: 500
<b></b>
</pre>


## Server doesn't support ranges
### Request:"http://example.com" ###

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

### Response: ###

<pre>
HTTP/1.1 200 OK
Date: Sat, 01 Jan 2000 01:00:00 GMT
Server: Apache/2.4.10 (Win32) OpenSSL/1.0.1h PHP/5.4.31
Keep-Alive: timeout=5, max=97
Connection: Keep-Alive
Content-Type: text/html
<b>Accept-Ranges: none</b>
Content-Length: 500
<b></b>
</pre>


