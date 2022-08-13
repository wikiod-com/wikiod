---
title: "Getting started with django-views"
slug: "getting-started-with-django-views"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Detailed instructions on getting django-views set up or installed.

## Django Views
Django Views are simply the functions that get called when a request is made to a certain URLs.                                                                     
URL patterns are written in `urls.py` file, each URL regex is given a function(Django view) from a `views.py`, so when a request is made, that function gets the call, with the HTTP request object, and then you can do whatever fun you want to do with that request.                                                                            

A simple example of view,
<!-- language: lang-py -->
```
from django.http import HttpResponse
import datetime

def current_datetime(request):
    now = datetime.datetime.now()
    html = "<html><body>It is now %s.</body></html>" % now
    return HttpResponse(html)
```

Calling the above view from a URL would return the current time, everytime you call the URL assigned to this view.    
The `request` object has many parameters related to the HTTP request you get, like headers, request type and more.
Read the official [doc][1] with more detailed examples. 


  [1]: https://docs.djangoproject.com/en/dev/topics/http/views/

