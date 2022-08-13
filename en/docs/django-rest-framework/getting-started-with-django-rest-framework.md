---
title: "Getting started with django-rest-framework"
slug: "getting-started-with-django-rest-framework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Install or Setup
**Requirements**

 - Python (2.7, 3.2, 3.3, 3.4, 3.5, 3.6) 
 - Django (1.7+, 1.8, 1.9, 1.10, 1.11)

**Install**

You can either use `pip` to install or clone the project from github.

 - **Using `pip`:**
   
       pip install djangorestframework

 - **Using `git clone`:**

       git clone git@github.com:tomchristie/django-rest-framework.git

After installing, you need to **add `rest_framework` to your `INSTALLED_APPS` settings.**  
   
    INSTALLED_APPS = (
        ...
        'rest_framework',
    )
If you're intending to use the browsable API you'll probably also
   want to add REST framework's login and logout views. Add the
   following to your root urls.py file.
   
    urlpatterns = [
        ...
        url(r'^api-auth/', include('rest_framework.urls', namespace='rest_framework'))
    ]

## Example
Let's take a look at a quick example of using REST framework to build a simple model-backed API.

We'll create a read-write API for accessing information on the users of our project.

Any global settings for a REST framework API are kept in a single configuration dictionary named `REST_FRAMEWORK`. Start off by adding the following to your `settings.py` module:

    REST_FRAMEWORK = {
        # Use Django's standard `django.contrib.auth` permissions,
        # or allow read-only access for unauthenticated users.
        'DEFAULT_PERMISSION_CLASSES': [
            'rest_framework.permissions.DjangoModelPermissionsOrAnonReadOnly'
        ]
    }

We're ready to create our API now. Here's our project's root `urls.py` module:

    from django.conf.urls import url, include
    from django.contrib.auth.models import User
    from rest_framework import routers, serializers, viewsets
    
    # Serializers define the API representation.
    class UserSerializer(serializers.HyperlinkedModelSerializer):
        class Meta:
            model = User
            fields = ('url', 'username', 'email', 'is_staff')
    
    # ViewSets define the view behavior.
    class UserViewSet(viewsets.ModelViewSet):
        queryset = User.objects.all()
        serializer_class = UserSerializer
    
    # Routers provide an easy way of automatically determining the URL conf.
    router = routers.DefaultRouter()
    router.register(r'users', UserViewSet)
    
    # Wire up our API using automatic URL routing.
    # Additionally, we include login URLs for the browsable API.
    urlpatterns = [
        url(r'^', include(router.urls)),
        url(r'^api-auth/', include('rest_framework.urls', namespace='rest_framework'))
    ]

You can now open the API in your browser at `http://127.0.0.1:8000/`, and view your new 'users' API. If you use the login control in the top right corner you'll also be able to add, create and delete users from the system.





