---
title: "Views"
slug: "views"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

A view function, or view for short, is simply a Python function that takes a Web request and returns a Web response. [-Django Documentation-][1]


  [1]: https://docs.djangoproject.com/en/1.11/topics/http/views/#writing-views

## [Introductory] Simple View (Hello World Equivalent)
Let's create a very simple view to respond a "Hello World" template in html format.

 1. To do that go to `my_project/my_app/views.py` (Here we are housing our view functions) and add the following view:

        from django.http import HttpResponse

        def hello_world(request):
            html = "<html><title>Hello World!</title><body>Hello World!</body></html>"
            return HttpResponse(html)

 2. To call this view, we need to configure a url pattern in `my_project/my_app/urls.py`:

        from django.conf.urls import url

        from . import views

        urlpatterns = [
            url(r'^hello_world/$', views.hello_world, name='hello_world'),
        ]

 3. Start the server: `python manage.py runserver`

    Now if we hit `http://localhost:8000/hello_world/`, our template (the html string) will be rendered in our browser.

