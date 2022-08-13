---
title: "Routers"
slug: "routers"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Routing is the process of mapping the logic (view methods etc.) to a set of URLs.
REST framework adds support for automatic URL routing to Django.

## Syntax
 - router = routers.SimpleRouter()
 - router.register(prefix, viewset)
 - router.urls  # the generated set of urls for the registered viewset.

## [Introductory] Basic usage, SimpleRouter
Automatic routing for the DRF, can be achieved for the `ViewSet` classes.

 1. Assume that the ViewSet class goes by the name of `MyViewSet` for
    this example.
 2. To generate the routing of `MyViewSet`, the `SimpleRouter` will be utilized.<br> On `myapp/urls.py`:
    
        from rest_framework import routers

        router = routers.SimpleRouter()        # initialize the router.
        router.register(r'myview', MyViewSet)  # register MyViewSet to the router.

 3. That will generate the following URL patterns for `MyViewSet`:
    
    - `^myview/$` with name `myview-list`.
    - `^myview/{pk}/$` with name `myview-detail`

 4. Finally to add the generated patterns in the `myapp`'s URL patterns, the django's [`include()`][1] will be used. <br>
    On `myapp/urls.py`: 

        from django.conf.urls import url, include
        from rest_framework import routers

        router = routers.SimpleRouter()
        router.register(r'myview', MyViewSet)

        urlpatterns = [
            url(r'other/prefix/if/needed/', include(router.urls)),
        ]


  [1]: https://docs.djangoproject.com/en/1.11/ref/urls/#include

