---
title: "Accept-Charset (Request)"
slug: "accept-charset-request"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

`Accept-Charset` tells the server what character sets the client accepts.

## Syntax
 * Accept-Charset: type;QualityFactor
 * Accept-Charset: type;QualityFactor, type;QualityFactor, type;QualityFactor, ...


## Parameters
| **Parameter** | **Description** |
| ------ | ------ |
| type | A character set name.  This can also be a `*` for all character sets |
| QualityFactor | The quality factor in the format `;q=0.8` (optional) |


`Accept-Charset` takes a number of character sets and includes an optional preference for which one the server should use.  The charset is one from the list
of available charsets at <a href='http://www.iana.org/assignments/character-sets/character-sets.xhtml'>IANA "Character Sets" registry</a>.  For example `UTF-8`.

The charset is separated by commas with an optional quality factor (using a `;q=`) that is
used the clients preference for using this type.  The quality factor has a value from 0 to 1 with the higher the number the more preference for that type.

If this header is not included then the client will accept any charset.

The server uses `Content-Type` to inform the client what character set it is using.

If the server can't find an acceptable charset to reply with then it should send a 406 (not acceptable) response or ignore this header and not
doing any content negotiation.


## Only accept UTF-8
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xml;q=0.9,*/*;q=0.8
<b>Accept-Charset: UTF-8</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept only UTF-8 char sets.


## Only accept UTF-8 and iso-8859-1
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xml;q=0.9,*/*;q=0.8
<b>Accept-Charset: UTF-8, iso-8859-1</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept only UTF-8 and iso-8859-1 char sets.


## Only accept UTF-8, iso-8859-1 with a preference
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xml;q=0.9,*/*;q=0.8
<b>Accept-Charset: UTF-8, iso-8859-1;q=0.8</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept UTF-8 and iso-8859-1 char sets but prefers UTF-8 (which has a quality factor of 1.0).


## Accept any charset but have preference for some types
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html,application/xml;q=0.9,*/*;q=0.8
<b>Accept-Charset: UTF-8, iso-8859-1;q=0.8, *;q=0.5</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept any charset but prefers UTF-8 and then iso-8859-1 if UTF-8 is not available.


