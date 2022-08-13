---
title: "jQuery Ajax Call With Asp MVC"
slug: "jquery-ajax-call-with-asp-mvc"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Posting JavaScript objects with jQuery Ajax Call
Ajax calls, request and retrieve data for giving the user a sense of a better interactive user interface experience. This article will show you how to use jQuery and send data through Ajax calls.
For this example, we’re going to POST the following JavaScript object to our server.

    var post = {
        title: " Posting JavaScript objects with jQuery Ajax Call",
        content: " Posting JavaScript objects with jQuery Ajax Call",
        tags: ["asp mvc", "jquery"]
    };

**The server side**

The server side model corresponding the javascript object.

    public class Post
    {
        public string Title { get; set; }
        public string Content { get; set; }
        public string[] Tags { get; set; }
    }

All we need to do is create a standard ASP.NET MVC controller method which takes a single parameter of the Person type, like so.

    public class PostController : BaseController
    {
        public bool Create(Post model)
        {
            //Do somthing
        }
    }

**The client side**

To send JavaScript Objects we need to use the JSON.stringify() method for send the object to the data option.

    $.ajax({
        url: '@Url.Action("create", "Post")',
        type: "POST",
        contentType: "application/json",
        data: JSON.stringify({ model: post })
    }).done(function(result){
        //do something
    });



