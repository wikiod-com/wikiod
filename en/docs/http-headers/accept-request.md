---
title: "Accept (Request)"
slug: "accept-request"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

What `Content-Type` does the client accept.

## Syntax
 * Accept: MIMEType/MIMESubtype;QualityFactor
 * Accept: MIMEType/MIMESubtype;QualityFactor, MIMEType/MIMESubtype;QualityFactor, ...


## Parameters
| **Parameter** | **Description** |
| ------ | ------ |
| MIMEType | The first half of the mime type.  This can also be a `*/*` for all types |
| MIMESubtype | The second half of the mime type or a * for all sub types (ie `image/*`) |
| QualityFactor | The quality factor in the format `;q=0.8` (optional) |


The content types are MIME types (ie `text/html`) separated by comma with an optional quality factor (using a `;q=`) that is
used the clients preference for using this type.  The quality factor has a value from 0 to 1 with the higher the number the more preference for that type.

If the server can't find an acceptable type to reply with then it should send a 406 (not acceptable) response.


## HTML only type
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
<b>Accept: text/html</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept Content-Types of text/html


## Match all text types
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
<b>Accept: text/*</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept Content-Types of any of the text/* types of MIME types.  For example text/html, text/plain, text/css.


## text/html and application/xml with a preference text/html
Request:"http://example.com"
<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
<b>Accept: text/html;q=1.0,application/xml;q=0.9</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept Content-Types of text/html and application/xml but it prefers text/html


## Preference for one type over another
Request:"http://example.com"

<pre>
GET / HTTP/1.1
Host: example.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:53.0) Gecko/20100101 Firefox/53.0
<b>Accept: text/html,application/xml;q=0.9,*/*;q=0.8</b>
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
</pre>

The client will accept all types of Content-Types but prefers text/html and application/xml


