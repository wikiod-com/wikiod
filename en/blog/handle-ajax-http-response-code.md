---
title: "How to Handle AJAX HTTP Response Code"
description: "In addition to .done, .fail and .always promise callbacks, which are triggered based on whether the request was successful or not, there is the option to trigger a function when a specific HTTP Status Code is returned from the server. This can be done using the statusCode parameter."
excerpt: "In addition to .done, .fail and .always promise callbacks, which are triggered based on whether the request was successful or not, there is the option to trigger a function when a specific HTTP Status Code is returned from the server. This can be done using the statusCode parameter."
date: 2021-11-04T09:19:42+01:00
lastmod: 2021-11-04T09:19:42+01:00
draft: false
weight: 50
images: []
categories: ["AJAX"]
tags: ["AJAX", "HTTP"]
pinned: false
homepage: false
---

In addition to <code>.done</code> <code>.fail</code> and <code>.always</code> promise callbacks, which are triggered based on whether the request was successful or not, there is the option to trigger a function when a specific HTTP Status Code is returned from the server. This can be done using the <code>statusCode</code> parameter.

    $.ajax({
        type: {POST or GET or PUT etc.},
        url:  {server.url},
        data: {someData: true},
        statusCode: {
            404: function(responseObject, textStatus, jqXHR) {
                // No content found (404)
                // This code will be executed if the server returns a 404 response
            },
            503: function(responseObject, textStatus, errorThrown) {
                // Service Unavailable (503)
                // This code will be executed if the server returns a 503 response
            }           
        }
    })
    .done(function(data){
        alert(data);
    })
    .fail(function(jqXHR, textStatus){
        alert('Something went wrong: ' + textStatus);
    })
    .always(function(jqXHR, textStatus) {
        alert('Ajax request was finished')
    });

As the official jQuery documentation states:

If the request is successful, the status code functions take the same parameters as the success callback; if it results in an error (including 3xx redirect), they take the same parameters as the <code>error</code> callback.
