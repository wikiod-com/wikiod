---
title: "Speed Optimization"
slug: "speed-optimization"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Enable Compression (Apache 2.0+)
<!-- language-all: lang-bash -->

>Enabling gzip compression can reduce the size of the transferred response by up to 90%, which can significantly reduce the amount of time to download the resource, reduce data usage for the client, and improve the time to first render of your pages. — [*PageSpeed Insights*](https://developers.google.com/speed/docs/insights/EnableCompression)

Compression can be enabled with this:

    AddOutputFilterByType DEFLATE "text/html"/
                                  "text/plain"/
                                  "text/xml"/
                                  "text/css"/
                                  "text/javascript"/
                                  "application/javascript"

[Apache Docs][1]


  [1]: http://httpd.apache.org/docs/current/mod/mod_deflate.html

## Leverage Browser Caching (Apache 2.0+)
<!-- language-all: lang-bash -->

>Fetching resources over the network is both slow and expensive: the download may require multiple roundtrips between the client and server, which delays processing and may block rendering of page content, and also incurs data costs for the visitor. All server responses should specify a caching policy to help the client determine if and when it can reuse a previously fetched response. — [*PageSpeed Insights*][1]

You can leverage browser caching like this:

    # Enable browser caching
    ExpiresActive On

    # Set the default caching duration
    ExpiresDefault "access plus 1 week"

    # Change the caching duration by file type
    ExpiresByType text/html "access plus 2 weeks"

[Apache Docs][2]


  [1]: https://developers.google.com/speed/docs/insights/LeverageBrowserCaching
  [2]: http://httpd.apache.org/docs/current/mod/mod_expires.html

## Enable KeepAlive (Apache 2.0+)
<!-- language-all: lang-bash -->

>The Keep-Alive extension to HTTP/1.0 and the persistent connection feature of HTTP/1.1 provide long-lived HTTP sessions which allow multiple requests to be sent over the same TCP connection. In some cases this has been shown to result in an almost 50% speedup in latency times for HTML documents with many images. To enable Keep-Alive connections, set KeepAlive On. — [*Apache Docs*](https://httpd.apache.org/docs/2.4/mod/core.html#keepalive)

    # Enable KeepAlive
    KeepAlive On

    # OPTIONAL — limit the amount of requests per connection with 'MaxKeepAliveRequests'
    # Example: MaxKeepAliveRequests 500

    # OPTIONAL — limit the amount of time the server will wait before it closes 
    # the connection with 'KeepAliveTimeout'
    # Example: KeepAliveTimeout 500

[Apache Docs](https://httpd.apache.org/docs/2.4/mod/core.html#keepalive)

