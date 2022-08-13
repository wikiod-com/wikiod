---
title: "Getting started with web-services"
slug: "getting-started-with-web-services"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Server side (Host Webservices) 
===========
Web services must be installed and running (deployed) in a web server as web application components. They can be part of a bigger application, or they can be deployed alone as they may compose a complete application.

It is responsibility of the server to forward an incoming HTTP request to the corresponding deployed application, and responsibility of the application to handle the request according to:

 - the HTTP verb (GET, POST, PUT, DELETE, OPTIONS, HEAD, TRACE, CONNECT)
 - the request URL

The application uses the combination of these elements to locate the corresponding web-service component that should process the request.

After the web-service is located, then the request parameters are used as input data to the web-service. The web-service is responsible to convert data to the correct datatypes, and to establish a convention with the clients about transmitting different datatypes.

The web-service is processing the input data and it produces an output dataset. The output dataset is wrapped in a HTTP response and it is sent back to the sender of the request.

Client-side
===========
A client has to prepare a HTTP request, complying to the rules of the server, and send it to the server. The response that will be received will contain the required data.

Why to use web-services
=======================
Using web-services client programs and a server programs can exchange information and collaborate to produce new services and results regardless their physical location and the technology they are built on. They only need to comply with the application level specifications.

The difference between using web-services and web-HTML-serving (browsing) is mainly that web-services are focused and specialized in processing and converting data types to produce structured results, that can be used for remote procedure calling. Web-HTML-serving is more about serving renderable/downloadable resources.

Exchanging process results using web-services is facilitating:
 - integration of applications
 - separation of concerns
 - distributed/decentralized application architectures

Java implementations
====================
In Java web-services are implemented as servlets. The most popular web-services frameworks are implementing a servlet that needs to be mapped with a URL. Examples of frameworks:

 - Axis
 - [CXF][1]
 - [Jersey][2]

Web service related components
==============================
 1. [WSDL][3] (Web service Description Language)
 2. UDDI (Universal Description Discovery and Integration)
 3. [SOAP][4] (Simple Object Access Protocol)


  [1]: https://www.wikiod.com/docs/cxf
  [2]: https://www.wikiod.com/docs/jersey
  [3]: https://www.wikiod.com/docs/wsdl
  [4]: https://www.wikiod.com/soap

