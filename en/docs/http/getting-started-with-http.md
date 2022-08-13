---
title: "Getting started with HTTP"
slug: "getting-started-with-http"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## HTTP/2


## HTTP requests and responses
[![HTTP clients and servers send HTTP requests and responses, respectively][1]][1]

HTTP describes how an HTTP client, such as a web browser, sends an HTTP request via a network to an HTTP server, which then sends an HTTP response back to the client.

The HTTP request is typically either a request for an online resource, such as a web page or image, but may also include additional information, such as data entered on a form. The HTTP response is typically a representation of an online resource, such as a web page or image.

  [1]: http://i.stack.imgur.com/tLYrL.png

## HTTP/1.0


## HTTP/1.1


## HTTP/0.9
The first version of HTTP that came into existence is 0.9, often referred to as "[HTTP As Implemented](https://www.w3.org/Protocols/HTTP/AsImplemented.html)." A common description of 0.9 is "a subsect of the full HTTP [i.e. 1.0] protocol." However, this greatly fails to illustrate the disparity in capabilities between 0.9 and 1.0.

Neither requests nor responses in 0.9 feature headers. Requests consist of a single CRLF-terminated line of `GET`, followed by a space, followed by the requested resource URL. Responses are expected to be a single HTML document. The end of said document is marked by dropping the connection server-side. There are no facilities to indicate success or failure of an operation. The only interactive property is the [search string](https://www.w3.org/Addressing/Search.html) which is closely tied to the [`<isindex>`](https://www.w3.org/MarkUp/html-spec/html-spec_5.html#SEC5.2.3) HTML tag.

Usage of HTTP/0.9 is nowadays exceptionally rare. It is occasionally seen on embedded systems as an alternative to [tftp](http://stackoverflow.com/tags/tftp/info).

