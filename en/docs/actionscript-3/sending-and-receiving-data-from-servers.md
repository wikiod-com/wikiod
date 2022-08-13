---
title: "Sending and Receiving Data From Servers"
slug: "sending-and-receiving-data-from-servers"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Making a request from Flash
The [`URLRequest`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/URLRequest.html) and [`URLLoader`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/URLLoader.html) classes work together to make requests from Flash to external resources. The `URLRequest` defines information about the request e.g. the request body and the request method type, and the `URLLoader` references this to perform the actual request and provide a means of being notified when a response is received from the resource.

Example:

    var request:URLRequest = new URLRequest('http://stackoverflow.com');
    var loader:URLLoader = new URLLoader();

    loader.addEventListener(Event.COMPLETE, responseReceived);
    loader.load(request);

    function responseReceived(event:Event):void {
        trace(event.target.data); // or loader.data if you have reference to it in
                                  // this scope.
    }

## Adding variables to your request
The [`URLVariables`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/URLVariables.html) class allows you to define data to be sent along with a `URLRequest`.

Example:

    var variables:URLVariables = new URLVariables();

    variables.prop = "hello";
    variables.anotherProp = 10;
    
    var request:URLRequest = new URLRequest('http://someservice.com');
    request.data = variables;

You can either send the request via a `URLLoader` or open the request URL with the variables attached in the querystring using [`navigateToURL`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/package.html#navigateToURL()).

## Altering the HTTP method (GET, POST, PUT, etc)
The [`URLRequestMethod`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/URLRequestMethod.html) class contains constants for the various request types you can make. These constants are to be allocated to `URLRequest`'s [`method`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/net/URLRequest.html#method) property:

    var request:URLRequest = new URLRequest('http://someservice.com');
    request.method = URLRequestMethod.POST;

> Note that only `GET` and `POST` are available outside the AIR runtime.

## My response data is always null, what does "asynchronous" mean?
When Flash makes a request for data from an external source, that operation is *asynchronous*. The most basic explanation of what this means is that the data loads "in the background" and triggers the event handler you allocate to `Event.COMPLETE` when it is received. This can happen at any point in the lifetime of your application.

Your data **WILL NOT** be available immediately after calling `load()` on your `URLLoader`. You **must** attach an event listener for `Event.COMPLETE` and interact with the response there.

    var request:URLRequest = new URLRequest('http://someservice.com');
    var loader:URLLoader = new URLLoader();
    
    loader.addEventListener(Event.COMPLETE, responseReceived);
    loader.load(request);
    
    trace(loader.data); // Will be null.
    
    function responseReceived(event:Event):void {
        trace(loader.data); // Will be populated with the server response.
    }
    
    trace(loader.data); // Will still be null.

You cannot get around this with any little tricks like using `setTimeout` or similar:

    setTimeout(function() {
        trace(loader.data); // Will be null if the data hasn't finished loading
                            // after 1000ms (which you can't guarantee).
    }, 1000);

## Cross-domain requests
Flash will not load data from a domain other than the one your application is running on unless that domain has an XML crossdomain policy either in the root of the domain (e.g. `http://somedomain.com/crossdomain.xml`) or somewhere that you can target with [`Security.loadPolicyFile()`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/system/Security.html#loadPolicyFile()). The crossdomain.xml file is where you can specify domains that are able to ask your server for data from a Flash application.

Example of the *most permissive* crossdomain.xml:

    <?xml version="1.0" ?>
    <cross-domain-policy>
      <allow-access-from domain="*"/>
      <allow-http-request-headers-from domain="*" headers="*"/>
    </cross-domain-policy>

> Note this example **should not be used in production environments**, use a more restrictive instance.

A more restrictive specific crossdomain.xml will look like this for example:

    <?xml version="1.0"?>
    <!DOCTYPE cross-domain-policy SYSTEM "http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd">
    <cross-domain-policy>
        <site-control permitted-cross-domain-policies="master-only" />

        <allow-access-from domain="*.domain.com" to-ports="80,843,8011" /> 
        <allow-access-from domain="123.123.123.123" to-ports="80,843,8011" /> 
    </cross-domain-policy>

Resources:

* [The crossdomain policy file specification](http://www.adobe.com/devnet-docs/acrobatetk/tools/AppSec/CrossDomain_PolicyFile_Specification.pdf).


