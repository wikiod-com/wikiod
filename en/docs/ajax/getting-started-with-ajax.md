---
title: "Getting started with ajax"
slug: "getting-started-with-ajax"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Simple jQuery Example to Communicate with Server
Taken from [`jQuery.ajax` API][1] web site:
    
    $.ajax({
        method: "POST",
        url: "some.php",
        data: {
            name: "John",
            location: "Boston"
        },
        success: function(msg) {
            alert("Data Saved: " + msg);
        },
        error: function(e) {
            alert("Error: " + e);
        }
    });
    
This chunk of code, due to jQuery, is easy to read and to understand what's going on.

- `$.ajax` - this bit calls jQuery's `ajax` functionality.
- `method: "POST"` - this line here declares that we're going to be using a POST method to communicate with the server. Read up on types of requests!
- `url` - this variable declares where the request is going to be **SENT** to. You're sending a request **TO** somewhere. That's the idea.
- `data` - pretty straight forward. This is the data you're sending with your request.
- `success` - this function here you write to decide what to do with the data you get back `msg`! as the example suggests, it's currently just creating an alert with the `msg` that gets returned.
- `error` - this function here you write to display error messages, or to provide actions to work when the `ajax` request went through errors.
- an alternative to `.done` is
      
      success: function(result) {
          // do something
      });

[1]: http://api.jquery.com/jquery.ajax/

## Installation or Setup
## What is AJAX? ##
----------
> AJAX stands for Asynchronous JavaScript and XML. In a nutshell, it is
> the use of the XMLHttpRequest object to communicate with server-side
> scripts. It can send as well as receive information in a variety of
> formats, including JSON, XML, HTML, and even text files. -Mozilla Developer Network 2016

The easiest way of implementing AJAX, especially if you're planning on communicating with servers is by using jQuery.

## What is jQuery? ##
----------
> jQuery is a fast, small, and feature-rich JavaScript library. It makes
> things like HTML document traversal and manipulation, event handling,
> animation, and Ajax much simpler with an easy-to-use API that works
> across a multitude of browsers. -jquery.com

For those who haven't used much jQuery, think of it as functions that we can use to make our lives easier. This is perfect for using with AJAX as it cuts down on the amount of code we have to write to accomplish the same thing!

## How to Add jQuery to your web site ##

If you need to Use Ajax, you need to add jQuery to your project. http://jquery.com/download/ In This link you can see many ways to add jquery. You can use downloaded version of jQuery or you can use a CDN. http://jquery.com/download/#jquery-39-s-cdn-provided-by-maxcdn. But there is some security risk if you uses CDN. Becauese the project call out to use jquery, so a hacker could manipulate the call. So better if you could use downloaded version. Lets see how to add jquery to html project. It's easy. First example is to use Downloaded source. 
Use this link to http://jquery.com/download/#jquery download. If you are just want to use jquery, I suggest you to download **Download the compressed, production jQuery 3.1.1** When you download it add jquery-version.min.js to appropriate place (like javascript folder of your project) Then just add <script> tag with src=jquery/location like below.
 
    <head>

    <script src="path/from/html/page/to/jquery.min.js"></script>
 
    </head>

Lets see how to use an CDN. This link http://jquery.com/download/#using-jquery-with-a-cdn you can see various CDN (Content Delivery Network).

    <head>
    
    <script   src="https://code.jquery.com/jquery-3.1.1.min.js"   integrity="sha256-hVVnYaiADRTO2PzUGmuLJr8BLUSjGIZsDYGmIJLv2b8="   crossorigin="anonymous"></script>
    
    </head>

As you can see in you just have to add the <script> tags which the CDN provider supply to the <head>. Now add some scripts to the html page to check it's working.

    <script>
    $(document).ready(function(){
        alert("jQuery Works")
    });
    </script>

If you see the **jQuery works** alert, That mean you added it correctly.

## Simple Ajax Request Sent Using the XMLHttpRequest Object
    var httpRequest = new XMLHttpRequest();
    
    httpRequest.onreadystatechange = getData;
    
    httpRequest.open('GET', 'https://url/to/some.file', true);
    httpRequest.send();

    function getData(){
        if (httpRequest.readyState === XMLHttpRequest.DONE) {
            alert(httpRequest.responseText);
        }
    }

`new XMLHttpRequest()` creates a new _XMLHttpRequest_ object - this is what we will send our request with

The `onreadystatechange` bit tells our request to call `getData()` everytime it's status changes

`.open()` creates our request - this takes a _request method_ ('**GET**', '**POST**', etc.), a url of the page you're querying, and optionally, whether or not the request should be **asynchrynous**

`.send()` sends our request - this optionally accepts data to send to the server like `.send(data)`

finally, the `getData()` is the function we've said should be called every time our request's **status** **changes**. if the readyState is equal to **DONE** then it alerts the `responseText` which is just the data recieved from the server.

More info can be found in the [getting started guide](https://developer.mozilla.org/en-US/docs/AJAX/Getting_Started) on MDN.

## Performing an Asynchronous AJAX Call using TypeScript
<h1>Adding a Product to a Shopping Cart</h1>

The following example demonstrates how to Add a product (or anything) to a Database Table asynchronously, using AJAX, and TypeScript.

    declare var document;
    declare var xhr: XMLHttpRequest;
    
    window.onLoad = () =>
    {
        Start();
    };
    
    function Start()
    {
        // Setup XMLHttpRequest (xhr).
        if(XMLHttpRequest)
        {
            xhr = new XMLHttpRequest();
        }
        else
        {
            xhr = new ActiveXObject("Microsoft.XMLHTTP");
        }
    
        AttachEventListener(document.body, "click", HandleCheckBoxStateChange);
    }
    
    function AttachEventListener(element: any, e, f)
    {
        // W3C Event Model.
        if(element.addEventListener)
        {
            element.addEventListener(e, f, false);
        }
        else if(element.attachEvent)
        {
            element.attachEvent("on" + e, (function(element, f)
            {
                return function()
                {
                    f.call(element, window.event);
                };
            })
            (element, f));
        }
    
        element = null;
    }
    
    function HandleCheckBoxStateChange(e)
    {
        var element = e.target || e.srcElement;
    
        if(element && element.type == "checkbox")
        {
            if(element.checked)
            {
                AddProductToCart(element);
            }
            else
            {
                // It is un-checked.
                // Remove item from cart.
            }
        }
        else
        {
            break;
        }
    }

    AddProductToCart(e)
    {
        var element = <HTMLInputElement>document.getElementById(e.id);
    
        // Add the product to the Cart (Database table)
        xhr.onreadystatechange = function()
        {
            if(xhr.readyState == 4)
            {
                if(xhr.status == 200)
                {
                    if(element != null)
                    {
                        console.log("200: OK");
                    }
                    else
                    {
                        console.log(":-(");
                    }
                }
                else
                {
                    // The server responded with a different response code; handle accordingly.
                    // Probably not the most informative output.
                    console.log(":-(");
                }
            }
        }
    
        var parameters = "ProductID=" + encodeURIComponent(e.id) + "&" + "Action=Add&Quantity=" + element.value;
    
        xhr.open("POST", "../Cart.cshtml");
        xhr.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
        xhr.setRequestHeader("Content-length", parameters.length.toString());
        xhr.setRequestHeader("Connection", "close");
    
        xhr.send(parameters);
    
        return e.id;
    }

In order to make this example complete, update your server-side code to actually insert this data into the database. The code above assumes you are using C# and have a `Cart.cshtml` file. However, simply replace `cshtml` with `php`, and write your own server-side logic using the language of your choice.










## Sync - Async ajax requests
**Asynchronous ajax call**

With this type of ajax call, code does not wait for the call to complete.

    $('form.ajaxSubmit').on('submit',function(){
        // initilization...
        var form   = $(this);
        var formUrl    = form.attr('action');
        var formType   = form.attr('method');
        var formData   = form.serialize();
        
        $.ajax({
            url: formUrl,
            type: formType,
            data: formData,
            async: true,
            success: function(data) {
                // .....
            }
        });
        //// code flows through without waiting for the call to complete
        return false;
    });
    

**Synchronous ajax call**

With this type of ajax call, code waits for the call to complete.

    $('form.ajaxSubmit').on('submit',function(){
        // initilization...
        var form   = $(this);
        var formUrl    = form.attr('action');
        var formType   = form.attr('method');
        var formData   = form.serialize();
        var data = $.ajax({
                url: formUrl,
                type: formType,
                data: formData,
                async: false
            }).responseText;
        //// waits for call to complete
        return false;
    });

## Using ajax in vanilla javascript with a simple callback
Here is our function to create a simple ajax call written in vanilla javascript (not es2015):

    function ajax(url, callback) {
        var xhr;
         
        if(typeof XMLHttpRequest !== 'undefined') xhr = new XMLHttpRequest();
        else {
            var versions = ["MSXML2.XmlHttp.5.0", 
                            "MSXML2.XmlHttp.4.0",
                            "MSXML2.XmlHttp.3.0", 
                            "MSXML2.XmlHttp.2.0",
                            "Microsoft.XmlHttp"]
 
             for(var i = 0, len = versions.length; i < len; i++) {
                try {
                    xhr = new ActiveXObject(versions[i]);
                    break;
                }
                catch(e){}
             } // end for
        }
         
        xhr.onreadystatechange = ensureReadiness;
         
        function ensureReadiness() {
            if(xhr.readyState < 4) {
                return;
            }
             
            if(xhr.status !== 200) {
                return;
            }
 
            // all is well  
            if(xhr.readyState === 4) {
                callback(xhr);
            }           
        }
         
        xhr.open('GET', url, true);
        xhr.send('');
    }

and it could be used as:

    ajax('myFile.html', function(response) {
        document.getElementById('container').innerHTML = response.responseText;
    });

If you want to use Ecmascript 6 (also known as es2015) you can use the **fetch** method, which returns a promise:

    fetch('myFile.json').then(function(res){
        return res.json();
     });

For furthner reading about es2015 Promises follow the link bellow: https://www.wikiod.com/javascript/promises

