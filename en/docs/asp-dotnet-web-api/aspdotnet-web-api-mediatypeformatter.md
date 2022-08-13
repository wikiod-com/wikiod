---
title: "ASP.NET Web API MediaTypeFormatter"
slug: "aspnet-web-api-mediatypeformatter"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## MediaTypeFormatter Basic Information
`MediaTypeFormatter` is an abstract class from which `JsonMediaTypeFormatter` and `XmlMediaTypeFormatter` classes inherit from. Here, `JsonMediaTypeFormatter` class handles JSON objects and `XmlMediaTypeFormatter` class handles XML objects.

**Return only JSON irrespective of the Accept Header value:**

To return only JSON objects in the response of the request weather Accept Header value of request if `application/json` or `application/xml` write the following line in the `Register` method of `WebApiConfig` class.

    config.Formatters.Remove(config.Formatters.XmlFormatter);

Here, `config` is a object of `HttpConfiguration` class. This line of code completely removes `XmlFormatter` which forces ASP.NET Web API to always return JSON irrespective of the Accept header value in the client request. Use this technique when you want your service to support only JSON and not XML. 


**Return only XML irrespective of the Accept Header value:**

To return only XML objects in the response of the request weather Accept Header value of request if `application/json` or `application/xml` write the following line in the `Register` method of `WebApiConfig` class.

    config.Formatters.Remove(config.Formatters.JsonFormatter);

Here, `config` is a object of `HttpConfiguration` class as described above. This line of code completely removes `JsonFormatter` which forces ASP.NET Web API to always return XML irrespective of the Accept header value in the client request. Use this technique when you want your service to support only XML and not JSON.

**Return JSON instead of XML:**

 1. When a request is issued from the browser, the web API service should return JSON instead of XML.
 2. When a request is issued from a tool like fiddler the Accept header value should be respected. This means if the Accept header is set to application/xml the service should return XML and if it is set to application/json the service should return JSON.

**Method 1:**

Include the following line in `Register` method of `WebApiConfig` class.

    config.Formatters.JsonFormatter.SupportedMediaTypes.Add(new MediaTypeHeaderValue("text/html"));

This instruct ASP.NET Web API to use `JsonFormatter` when request is made for `text/html` which is the default for most browsers. The problem with this approach is that `Content-Type` header of the response is set to `text/html` which is misleading.

**Method 2:**

Use Custom formatters. Make a class which is derived from `JsonMediaTypeFormatter` class and implement `SetDefaultContentHeaders` method. 

Here is the example of custom JSON formatter class which returns JSON format in response.

    public class CustomJsonFormatter : JsonMediaTypeFormatter
    {
        public CustomJsonFormatter()
        {
            this.SupportedMediaTypes.Add(new MediaTypeHeaderValue("text/html"));
        }
    
        public override void SetDefaultContentHeaders(Type type, HttpContentHeaders headers, MediaTypeHeaderValue mediaType)
        {
            base.SetDefaultContentHeaders(type, headers, mediaType);
            headers.ContentType = new MediaTypeHeaderValue("application/json");
        }
    }

And this is the example of Custom Media type formatter which returns CSV format in response.

    public class CSVMediaTypeFormatter : MediaTypeFormatter {

        public CSVMediaTypeFormatter()
        {
            SupportedMediaTypes.Add(new MediaTypeHeaderValue("text/csv"));
        }
        
        public CSVMediaTypeFormatter(MediaTypeMapping mediaTypeMapping) : this()
        {
            MediaTypeMappings.Add(mediaTypeMapping);
        }
        
        public CSVMediaTypeFormatter(IEnumerable<MediaTypeMapping> mediaTypeMappings) : this()
        {
            foreach (var mediaTypeMapping in mediaTypeMappings)
            {
                MediaTypeMappings.Add(mediaTypeMapping);
            }
        }
    }

After, implementing the custom formatter class register it in `Register` method of `WebApiConfig` class.

    config.Formatters.Add(new CustomJsonFormatter());

Now, according to your formatter you will get response and `Content-Type` from the server.

