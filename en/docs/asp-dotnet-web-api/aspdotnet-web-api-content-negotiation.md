---
title: "ASP.NET Web API Content Negotiation"
slug: "aspnet-web-api-content-negotiation"
draft: false
images: []
weight: 9817
type: docs
toc: true
---

## ASP.NET Web API Content Negotiation Basic Information
**Content Negotiation** can be defined as the process of selecting best representation for a given resource. So Content negotiation means the client and server can negotiate between them so that client can get data according to their required format.

There are three points on which internet depends,

 - The Resource
 - A Pointer to resource(URL)
 - Representation of resource

Third point is more important than other two, because everything is works on the basis of how we can see the resource. We can represent a resource in two formats.

 1. XML(Extensible Markup Language) Format
 2. JSON(JavaScript Object Notation) Format

One of the standards of the RESTful service is that, the client should have the ability to decide in which format they want the response either in JSON or XML. A request that is sent to the server includes an Accept header. Using the Accept header the client can specify the format for the response.

For example,

`Accept: application/xml` returns result in XML format

`Accept: application/json` returns result in JSON format

Depending on the Accept header value in the request, the server sends the response. This is called Content Negotiation. 

**What happens behind the scene when we request data in specific format?**

The ASP.NET Web API controller generates the data that we want to send to the client and hands the data to the Web API pipeline which then look for Accept header of the client. Then, choose a appropriate formatter to format the data.

As ASP.NET Web API is greatly extensible, we can also specify multiple values for accept header in the request header. 

`Accept: application/xml,application/json`

In the above case, server choose the first formatter to format the data of response.

We can also specify quality factor in the accept header. In this case, server choose a format which have higher quality factor.

`Accept: application/json;q=0.8,application/xml;q=0.5`

If we don't specify any Accept header, then by default server choose JSON formatter.

When the response is being sent to the client in the requested format, notice that the `Content-Type` header of the response is set to the appropriate value. For example, if the client has requested `application/xml`, the server send the data in XML format and also sets the `Content-Type=application/xml`.

The formatters are used by the server for both request and response messages. When the client sends a request to the server, we set the Content-Type header to the appropriate value to let the server know the format of the data that we are sending.

For example, if the client is sending JSON data, the Content-Type header is set to `application/json`. The server knows it is dealing with JSON data, so it uses JSON formatter to convert JSON data to .NET Type. Similarly when a response is being sent from the server to the client, depending on the Accept header value, the appropriate formatter is used to convert .NET type to JSON, XML etc.

**Example of different types of response format:**

**application/json:**

    {
      "Email": "sample string 1",
      "HasRegistered": true,
      "LoginProvider": "sample string 3"
    }

**application/xml:**

    <UserInfoViewModel xmlns:i="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://schemas.datacontract.org/2004/07/WebApiDemo.Models">
      <Email>sample string 1</Email>
      <HasRegistered>true</HasRegistered>
      <LoginProvider>sample string 3</LoginProvider>
    </UserInfoViewModel>

Modern web based applications can provide data in various languages and formats. So, if we develop our API to cover global users across the world, then Content Negotiation is relevant.

## Content Negotiation in Web API
Understanding the concept
-------------------------

To understand content negotiation in Web API, it is important to understand the term `Resource`. 

On the web, any information that we can access can be referred as `HTTP resource`. There is a tremendous amount of material to view on the web which has different content type such as html documents, images, video, audio, executable files, spreadsheets etc. We can get any resource by making an http request to the resource uri. The http response for the request, returns the resource and also specifies the content type, which is `also known as media type`.

In order to access any resource, client can make http request by providing specific resource uri and the http verbs. However, in addition to this, client can also specify the accept-type which is the format of the content the user is looking for. The “accept-type” can be defined in the http request headers as the `“accept”` header. 

The server then checks the “accept” header from the requests and returns the response in the specified format, if available. Please note that the server can only return the response in the requested representation **if it is available**. If the requested representation is not available then it returns the resource in default representation. That is the reason it is called content negotiation. 


----------

A practical example
-------------------

As an example, assume that you are making a request to http://example.com/customer/1 to get the information of customer with the id 1. If you don’t specify the “accept” header in the request, the server will return the default representation of this resource. 

Assume that the server can return the customer information in `json and xml both`. Now, it is on the client to specify the required format of the customer information in the “accept” header in the request. The value of the `“accept”` header can be `“application/json”` for json representation, or `“text/xml”` for xml representation. The server will then return the response as per the requested format. 

If the requested format is “text/html” which is not supported by this host (as in this example), then it will *`simply return the resource in the default format`*. The http response contains a header `“content-type”` which tells the client about the format of the resource.

Please note that even in the case when the requested representation of the resource is not available, the default representation of the resource is still returned. 

**That is why it is referred as content negotiation**. 

> The client negotiates the representation of the response, however, if
> it is not available then gets the default one.


----------

How to configure in Web API
----------

In Web API, content negotiation can be configured in the `WebAPIConfig` class as 

    config.Formatters.JsonFormatter.SupportedMediaTypes.Add(new System.Net.Http.Headers.MediaTypeHeaderValue("text/html"));

You can also override the default content negotiation in Web API by implementing IContentNegotiator interface and its Negotiate method, and then setup this in the Web API request pipe line, in the WebAPI.config file as below:

    GlobalConfiguration.Configuration.Services.Replace(typeof(IContentNegotiator), new CustomContentNegotiator());

Following is a sample implemantation of Negotiate method.


    public class CustomContentNegotiator : DefaultContentNegotiator
        {
            public override ContentNegotiationResult Negotiate(Type type, HttpRequestMessage request, IEnumerable<MediaTypeFormatter> formatters)
            {
                var result = new ContentNegotiationResult(new JsonMediaTypeFormatter(), new MediaTypeHeaderValue("application/json"));
                return result;
            }






