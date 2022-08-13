---
title: "Mixins"
slug: "mixins"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

The mixin classes provide the actions that are used to provide the basic view behavior. Note that the mixin classes provide action methods rather than defining the handler methods, such as `.get()` and `.post()`, directly. This allows for more flexible composition of behavior. -[Official Django rest Framework Documentation][1]-


  [1]: http://www.django-rest-framework.org/api-guide/generic-views/#mixins

## [Introductory] List of Mixins And Usage on Views/Viewsets
<hr>

**List of available mixins:**

 - **ListModelMixin:** provides a `.list()` method to the view/viewset
 - **RetrieveModelMixin:** provides a `.retrieve()` method to the view/viewset
 - **CreateModelMixin:** provides a `.create()` method to the view/viewset
 - **UpdateModelMixin:** provides a `.update()` method to the view/viewset
 - **DestroyModelMixin:** provides a `.destroy()` method to the view/viewset
<hr>

We can mix and match the mixins in our generic views and viewsets, in order to give them the corresponding utility that we need:

 1. An API view with `.list()` `.create()` and `.destroy()` methods? <br>
    Inherit from the `GenericAPIView` and combine the appropriate mixins:

        from rest_framework import mixins, generics

        from myapp.models import MyModel
        from myapp.serializers import MyModelSerializer

        class MyCustomAPIView(mixins.ListModelMixin, 
                              mixins.CreateModelMixin,
                              mixins.DestroyModelMixin,
                              generics.GenericAPIView):

            queryset = MyModel.objects.all()
            serializer_class = MyModelSerializer

            def get(self, request, *args, **kwargs):
                return self.list(request, *args, **kwargs)

            def post(self, request, *args, **kwargs):
                return self.create(request, *args, **kwargs)

            def delete(self, request, *args, **kwargs):
                return self.destroy(request, *args, **kwargs)

 2. A viewset with only a `.list()` and `.update()` methods? <br>
    Inherit from the `GenericViewSet` and add the appropriate mixins:

        from rest_framework import mixins

        class MyCustomViewSet(mixins.ListModelMixin,
                              mixins.UpdateModelMixin,
                              viewsets.GenericViewSet):
            pass
     Yes, it was that easy!!
<hr>

To conclude, we can mix and match every mixin we need and utilize it to customize our views and their methods in any possible combination!

## [Intermediate] Creating Custom Mixins
<hr>
DRF offers the chance to further customize the behavior of the generic views/viewsets by allowing the creation of custom mixins.<br>
<hr>

**How to:**

To define a custom mixin we just need to create a class inheriting from `object`.

Let's assume that we want to define two separate views for a model named `MyModel`. Those views will share the same `queryset` and the same `serializer_class`. We will save ourselves some code repetition and we will put the above in a single mixin to be inherited by our views:

 - `my_app/views.py` (that is not the only file option available to place our custom mixins, but it is the less complex):

       from rest_framework.generics import CreateAPIView, RetrieveUpdateAPIView
       from rest_framework.permissions import IsAdminUser

       class MyCustomMixin(object):
           queryset = MyModel.objects.all()
           serializer_class = MyModelSerializer

       class MyModelCreateView(MyCustomMixin, CreateAPIView):
           """
           Only an Admin can create a new MyModel object
           """
           permission_classes = (IsAdminUser,)
           
           Do view staff if needed...

       class MyModelCreateView(MyCustomMixin, RetrieveUpdateAPIView):
           """
           Any user can Retrieve and Update a MyModel object
           """
           Do view staff here...

**Conclusion:**

Mixins are essentially blocks of reusable code for our application.

