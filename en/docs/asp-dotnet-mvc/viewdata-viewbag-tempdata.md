---
title: "ViewData, ViewBag, TempData"
slug: "viewdata-viewbag-tempdata"
draft: false
images: []
weight: 9887
type: docs
toc: true
---

`ViewData` and `ViewBag` are used to transfer data from controller to view.  

ViewData is nothing but a dictionary of objects and it is accessible by string as key.  

ViewBag is very similar to ViewData. ViewBag is a dynamic property. ViewBag is just a wrapper around the ViewData.

TempData keeps data for the time of HTTP Request, which means that it holds data between two consecutive requests. TempData helps us to transfer data between controllers or between actions. Internally uses session.

## Syntax
1. ViewData[key] = value;

2. ViewBag.Key = value;

3. TempData[key] = value;


## TempData life cycle
Data saved to TempData is stored in the session and will be automatically removed at the end of the first request where the data is accessed. If never read, it will be kept until finally read or the session times out. 

The typicaly usage looks like the following sequence (where each line is invoked from a different request):
    
    //first request, save value to TempData
    TempData["value"] = "someValueForNextRequest";

    //second request, read value, which is marked for deletion
    object value = TempData["value"];

    //third request, value is not there as it was deleted at the end of the second request
    TempData["value"] == null

This behavior can be further controller with the [`Peek`](https://msdn.microsoft.com/en-us/library/system.web.mvc.tempdatadictionary.peek(v=vs.118).aspx) and [`Keep`](https://msdn.microsoft.com/en-us/library/ee703497(v=vs.118).aspx) methods.

- With `Peek` you can retrieve data stored in TempData without marking it for deletion, so data will still be available on a future request

      //first request, save value to TempData
      TempData["value"] = "someValueForNextRequest";

      //second request, PEEK value so it is not deleted at the end of the request
      object value = TempData.Peek("value");

      //third request, read value and mark it for deletion
      object value = TempData["value"];

- With `Keep` you can specify that a key that was marked for deletion should actually be retained. In this case retrieving the data and saving it from deletion requires 2 method calls:

      //first request, save value to TempData
      TempData["value"] = "someValueForNextRequest";

      //second request, get value marking it from deletion
      object value = TempData["value"];
      //later on decide to keep it
      TempData.Keep("value");

      //third request, read value and mark it for deletion
      object value = TempData["value"];

With this in mind, use `Peek` when you always want to retain the value for another request and use `Keep` when retaining the value depends on additional logic.

## What are ViewData, ViewBag, and TempData?
**ViewData** is the mechanism for a controller to provide data to the view it presents, without using a ViewModel. Specifically, ViewData is a dictionary which is available both in MVC action methods and views. You may use ViewData to transfer some data from your action method to the view returned by the action method.

Since it is a dictionary, you can use the dictionary like syntax to set and get data from it.

    ViewData[key] = value; // In the action method in the controller

For example, If you want to pass a string message from your Index action method to your Index view `Index.cshtml`, you can do this.

    public ActionResult Index()
    {
       ViewData["Message"] = "Welcome to ASP.NET MVC";
       return View(); // notice the absence of a view model
    }

To access this in your `Index.cshtml` view, you can simply access the ViewData dictionary with the key used in the action method.

    <h2>@ViewData["Message"]</h2>

**ViewBag** is the dynamic equivalent of the untyped ViewData dictionary. It takes advantage of the C# `dynamic` type for syntactical sugar experience.

The syntax for setting some data to ViewBag is

    ViewBag.Key = Value;

So if we want to pass the message string in our previous example using ViewBag, it will be 

    public ActionResult Index()
    {
       ViewBag.Message = "Welcome to ASP.NET MVC";
       return View(); // not the absence of a view model
    }

and in your Index view,

    <h2>@ViewBag.Message</h2>

Data is not shared between the ViewBag and the ViewData. ViewData requires typecasting for getting data from complex data types and check for null values to avoid error where as View Bag does not require typecasting.

**TempData** can be used when you want to persist data between one http request and the next HTTP request only. The life of data stored in the TempDataDictionary ends after the second request. So TempData is useful in scenarios where you are following the PRG pattern.

    [HttpPost]
    public ActionResult Create(string name)
    {
       // Create a user
       // Let's redirect (P-R-G Pattern)
       TempData["Message"] = "User created successfully";
       return RedirectToAction("Index");
    }
    public ActionResult Index()
    {
      var messageFromPreviousCall = TempData["Message"] as String;
      // do something with this message
      // to do : Return something
    }

When we do `return RedirectToAction("SomeActionMethod")`, the server will send a 302 response to the client(browser) with location header value set to the URL to "SomeActionMethod" and browser will make a totally new request to that. ViewBag / ViewData won't work in that case to share some data between these 2 calls. You need to use TempData in such scenarios.


