---
title: "Accept-Language (Request)"
slug: "accept-language-request"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

`Accept-Language` tells the server what language (such as English) does the client accept.

## Syntax
 * Accept-Language: Language;QualityFactor
 * Accept-Language: Language;QualityFactor, Language;QualityFactor, ...
 * Accept-Language: *


## Parameters
| **Parameter** | **Description** |
| ------ | ------ |
| Language | What language is acceptable.  |
| QualityFactor | The quality factor in the format `;q=0.8` (optional) |
| \* | Match any language |


`Accept-Language` takes a number of languages and includes an optional preference for which one the server should use.  The language is one from the list
of available at <a href='https://www.iana.org/assignments/language-subtag-registry/language-subtag-registry'>IANA Language Subtag Registry page</a>.  For example `en` is English, and `en-US` is USA English.

The language is separated by commas with an optional quality factor (using a `;q=`) that is
used the clients preference for using this language.  The quality factor has a value from 0 to 1 with the higher the number the more preference for that language.

If this header is not included then the client will accept any language.

The server uses `Content-Language` to inform the client what langauge it is using.


## English only
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html
<b>Accept-Language: en</b>
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will only accept Content-Language of English.


## US English or basic english
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html
<b>Accept-Language: en-US,en;q=0.5</b>
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept English but prefers US English.


## US English or basic english
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html
<b>Accept-Language: da, en-gb;q=0.8, en;q=0.7</b>
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client prefers Danish, but will also accept British English, or if that's not available basic English.


## Match any language
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
Accept: text/html
<b>Accept-Language: *</b>
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept any language.


