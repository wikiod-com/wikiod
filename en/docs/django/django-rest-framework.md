---
title: "Django Rest Framework"
slug: "django-rest-framework"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Simple barebones read-only API
Assuming you have a model that looks like the following, we will get up an running with a simple barebones **read-only** API driven by Django REST Framework ("DRF").


**models.py**

    class FeedItem(models.Model):
        title = models.CharField(max_length=100, blank=True)
        url = models.URLField(blank=True)
        style = models.CharField(max_length=100, blank=True)
        description = models.TextField(blank=True)

The serializer is the component that will take all of the information from the Django model (in this case the `FeedItem`) and turn it into JSON. It is very similar to creating form classes in Django. If you have any experience in that, this will be very comfortable for you.

**serializers.py**

    from rest_framework import serializers
    from . import models

    class FeedItemSerializer(serializers.ModelSerializer):
        class Meta:
            model = models.FeedItem
            fields = ('title', 'url', 'description', 'style')

**views.py**

DRF offers [many view classes][1] to handle a variety of use cases. In this example, we are only going to have a **read-only** API, so, rather than using a more comprehensive [viewset][2], or a bunch of related generic views, we will use a single subclass of DRF's `ListAPIView`.

The purpose of this class is to link the data with the serializer, and wrap it all together for a response object.

    from rest_framework import generics
    from . import serializers, models

    class FeedItemList(generics.ListAPIView):
        serializer_class = serializers.FeedItemSerializer
        queryset = models.FeedItem.objects.all()


  [1]: http://www.django-rest-framework.org/api-guide/generic-views/
  [2]: http://www.django-rest-framework.org/api-guide/viewsets/

**urls.py**

Make sure you point your route to your DRF view.

    from django.conf.urls import url
    from . import views

    urlpatterns = [
        ...
        url(r'path/to/api', views.FeedItemList.as_view()),
    ]

