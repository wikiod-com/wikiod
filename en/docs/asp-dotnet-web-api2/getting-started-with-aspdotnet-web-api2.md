---
title: "Getting started with asp.net-web-api2"
slug: "getting-started-with-aspnet-web-api2"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## What and why Asp.Net Web API2?
**What and Why ?**

Asp.Net’s Web API2 is the latest version of Web API. It is an easy way to implement a RESTful web service using all of the goodness that the Asp.Net framework provides. Once you understand the basic principles of REST, then a Asp.net Web API2 will be very easy to implement.
Web API2 is built on Asp.Net’s modular, pluggable pipeline model. This means that when a server hosting a web API2 receives a request, it passes through Asp.Nets request pipeline first. This enables you to easily add your own modules if you find that the default capabilities are not enough for your needs. With the recent announcements on `ASP.net vNext` this also means you can potentially host your Web API2 outside of Windows Server which opens up a whole range of usage cases. See [here][1] for detail.

**How works ?**

Web API2 uses the Controller and Action concepts from MVC. Resources are mapped directly to controllers; you would typically have a different controller for each of your main data entities (Product, Person, Order etc). Web API2 uses the Asp.Net routing engine to map URLs to controllers. Typically, APIs are held within a `/api/` route which helps to distinguish API controllers from other non-API in the same website.

Actions are used to map to specific HTTP verbs, for example you would typically have a GET action which returns all of the entities. This action would respond to `/api/Products` (where ‘products’ is your controller) and would look something like this:

     public IEnumerable<string> Get()
     {
         return new string[] { "value1", "value2" };
     }

You may also have a `GET` action which accepts a specific `ID` and returns a specific entity. It would respond to `/api/Products/81` and would look something like this:
 

    public string Get(int id)
     {
        return "value";
     }

There are many great hidden benefits to using Web API which you may not realise but actually save you a lot of work.

**Web API2 is part of the ‘One Asp.Net’**

Web API2 is part of the ‘One Asp.Net’ family which means that it natively supports all of the great shared features you may currently use with MVC or web forms, this includes (these are just a few examples):

 - Entity Framework
 - Authorisation and identity
 - Scaffolding
 - Routing

**Serialization and Model Binding**

Web API2 is setup by default to provide responses in either XML or JSON (JSON is default). However, as a developer you do not need to do any conversion or parsing – you simply return a strongly typed object and Web API2 will convert it to XML or JSON and return it to the calling client, this is a process called Content Negotiation. This is an example of a `GET` action which returns a strongly typed Product object.

     public Product GetProduct(int id)
     {
        var product = _products.FirstOrDefault(p => p.ID == id);
        if (product == null)
        {
           throw new HttpResponseException(HttpStatusCode.NotFound);
        }
        return Request.CreateResponse(HttpStatusCode.OK, product);
     }

 
This also works for incoming requests using a feature called Model Validation. With Model Validation, Web API2 is able to validate and parse incoming response body data to a strongly typed object for you to work with in your code. This is an example of model binding:

     public HttpResponseMessage Post(Product product)
     {
         if (ModelState.IsValid)
         {
             // Do something with the product (not shown).
     
             return new HttpResponseMessage(HttpStatusCode.OK);
         }
         else
         {
             return Request.CreateErrorResponse(HttpStatusCode.BadRequest, ModelState);
         }
     }


  [1]: http://www.asp.net/vnext

## Hello Web Api
**Web Api 2 - Hello World example**

We are going to create a new Web Api simple application which return to us Json with message and user name.  
Lets start! First create new Web Api project using Visual Studio and select Empty Template. Be sure to check "Web&nbsp;Api" folder:

[![enter image description here][1]][1]

**NOTE** I didn't choose "Web Api" template because it adds reference to ASP.NET MVC to provide API Help Page. And in such basic application we don't really need it.


**Adding a model**

A model is an C# class that represents some data in our app. ASP.NET Web API is able to automatically serialize model to JSON, XML, or some other format (depends on configuration).

In our application we will create only one model, but real-world apps usually have a lot of them.

In Solution Explorer, right-click the **Models folder**. Next select **Add** and then select **Class**. Name the class "HelloMessage". Our model needs two properties: *MessageText* and *UserName*:

    namespace WebApiHelloWorld.Models
    {
        public class HelloMessage
        {
            public string MessageText { get; set; }
            public string UserName { get; set; }
        }
    }

**Adding a controller**

A controller handles HTTP requests. Our app needs only one controller that returns Json with Hello message and user name (which we will pass in URL).  
In Solution Explorer, right-click the **Controllers folder**. Next select **Add** and then select **Controller**. In the opened window, select **Web API Controller - Empty** and click **Add**.

[![enter image description here][2]][2]

Set controller name as "HelloController". Next edit code of created controller. We need to add method which returns Hello message.

    using System.Web.Http;
    using WebApiHelloWorld.Models;
    
    namespace WebApiHelloWorld.Controllers
    {
        public class HelloController : ApiController
        {
            public HelloMessage GetMessage(string name)
            {
                HelloMessage message = new HelloMessage
                {
                    MessageText = "Hello my Dear!",
                    UserName = name
                };
    
                return message;
            }
        }
    }

**NOTE** Be sure to add `using WebApiHelloWorld.Models`. Without it your controller won't find HelloMessage class.

**Finish**

That's all! Now you only need to build and start your application. Simply hit **Ctrl + F5** or just **F5** (to start without debugging). Visual studio will launch web browser. You need to call your controller. To do that add at the end of the URL "/api/hello?name=John". The result should be:

    {
        "MessageText": "Hello my Dear!",
        "UserName": "John"
    }


  [1]: https://i.stack.imgur.com/TPLtf.png
  [2]: https://i.stack.imgur.com/otR9M.png

## Installation or Setup
Detailed instructions on getting asp.net-web-api 2 set up or installed.

