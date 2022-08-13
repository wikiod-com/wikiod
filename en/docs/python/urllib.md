---
title: "urllib"
slug: "urllib"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## HTTP GET
<!-- if version <Python 2.x> [lte 2.7] -->
## Python 2

    import urllib
    response = urllib.urlopen('https://www.wikiod.com/docs/)

Using `urllib.urlopen()` will return a response object, which can be handled similar to a file.

    print response.code
    # Prints: 200

The `response.code` represents the http return value.  200 is OK, 404 is NotFound, etc.

    print response.read()
    '<!DOCTYPE html>\r\n<html>\r\n<head>\r\n\r\n<title>Documentation - Stack. etc'

`response.read()` and `response.readlines()` can be used to read the actual html file returned from the request.  These methods operate similarly to `file.read*`
<!-- end version if -->

<!-- if version <Python 3.x> [gte 3.0] -->

## Python 3

    import urllib.request
    
    print(urllib.request.urlopen("https://www.wikiod.com/docs/"))
    # Prints: <http.client.HTTPResponse at 0x7f37a97e3b00>
    
    response = urllib.request.urlopen("https://www.wikiod.com/docs/")
    
    print(response.code)
    # Prints: 200
    print(response.read())
    # Prints: b'<!DOCTYPE html>\r\n<html>\r\n<head>\r\n\r\n<title>Documentation - Stack Overflow</title> 

The module has been updated for Python 3.x, but use cases remain basically the same.  `urllib.request.urlopen` will return a similar file-like object.

<!-- end version if -->

## HTTP POST
To POST data pass the encoded query arguments as data to urlopen()

<!-- if version <Python 2.x> [lte 2.7] -->

## Python 2

    import urllib
    query_parms = {'username':'stackoverflow', 'password':'me.me'}
    encoded_parms = urllib.urlencode(query_parms)
    response = urllib.urlopen("https://stackoverflow.com/users/login", encoded_parms)
    response.code
    # Output: 200
    response.read()
    # Output: '<!DOCTYPE html>\r\n<html>\r\n<head>\r\n\r\n<title>Log In - Stack Overflow'

<!-- end version if -->

<!-- if version <Python 3.x> [gte 3.0] -->

## Python 3

    import urllib
    query_parms = {'username':'stackoverflow', 'password':'me.me'}
    encoded_parms = urllib.parse.urlencode(query_parms).encode('utf-8')
    response = urllib.request.urlopen("https://stackoverflow.com/users/login", encoded_parms)
    response.code
    # Output: 200
    response.read()
    # Output: b'<!DOCTYPE html>\r\n<html>....etc'

<!-- end version if -->

## Decode received bytes according to content type encoding
The received bytes have to be decoded with the correct character encoding to be interpreted as text:

<!-- if version <Python 3.x> [gte 3.0] -->

    import urllib.request
    
    response = urllib.request.urlopen("http://stackoverflow.com/")
    data = response.read()
    
    encoding = response.info().get_content_charset()
    html = data.decode(encoding)

<!-- end version if -->

<!-- if version <Python 2.x> [lte 2.7] -->

    import urllib2
    response = urllib2.urlopen("http://stackoverflow.com/")
    data = response.read()
    
    encoding = response.info().getencoding()
    html = data.decode(encoding)

<!-- end version if -->





